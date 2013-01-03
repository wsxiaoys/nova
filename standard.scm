;;; Core forms

;; (set! var val) -> set ast
(define (macrology/set!)
  (make-macrology
    (lambda (define-macro)
      (define-macro 'set!
        (lambda (expr use-env mac-env)
          (receive-expr* (form meta) expr
            (if (not (and (= (length form) 3)
                        (name? (expr*/form (cadr form)))))
              (classify-error "bad set! syntax" form)) 
            (make-set meta (classify (cadr form) use-env) (classify (caddr form) use-env))))))))

;; (define var val) -> def ast
(define (macrology/define)
  (make-macrology
    (lambda (define-macro)
      (define-macro 'define
        (lambda (expr use-env mac-env)
          (receive-expr* (form meta) expr
            (if (< (length form) 3)
              (classify-error "bad define syntax" form)) 
            (receive (name body)
                     (if (name? (expr*/form (cadr form)))
                       (values (cadr form) (caddr form)) 
                       (values (car (expr*/form (cadr form)))
                               `(,(close-syntax 'lambda mac-env)
                                  ,(cdr (expr*/form (cadr form)))
                                  ,@(cddr form))))
                (bind-variable! (expr*/form name) use-env)
                (make-def meta (classify name use-env) (delay (classify body use-env))))))))))

;; (begin . body) -> seq ast
(define (macrology/begin)
  (make-macrology
    (lambda (define-macro)
      (define-macro 'begin
        (lambda (expr use-env mac-env)
          (receive-expr* (form meta) expr
            (make-seq meta (classify* (cdr form) use-env))))))))

;; (lambda params . body) -> lam ast
(define (macrology/lambda)
  (make-macrology
    (lambda (define-macro)
      (define-macro 'lambda
        (lambda (expr use-env mac-env)
          (receive-expr* (form meta) expr
            (if (< (length form) 3) 
              (classify-error "bad lambda syntax" form)) 
            (let ((env (syntactic-extend use-env)))
              (make-lam meta
                (map* (lambda (expr)
                        (receive-expr* (x _) expr
                          (if (not (name? x)) 
                            (classify-error "bad lambda param" form) 
                            (bind-variable! x env)) 
                          (classify expr env)))
                      (expr*/form (cadr form)))
                (classify* (cddr form) (syntactic-extend env))
                env))))))))

;; (if test succ fail) -> seq ast
(define (macrology/if)
  (make-macrology
    (lambda (define-macro)
      (define-macro 'if
        (lambda (expr use-env mac-env)
          (receive-expr* (form meta) expr
            (let ((fail
                    (case (length form)
                      ((3) (make-lit #f (void)))
                      ((4) (classify (cadddr form) use-env))
                      (else
                        (classify-error "bad if syntax" form)))))
              (make-cnd meta
                        (classify (cadr form) use-env)
                        (classify (caddr form) use-env)
                        fail))))))))

;; (quote form) -> lit ast
(define (macrology/quote)
  (make-macrology
    (lambda (define-macro)
      (define-macro 'quote
        (lambda (expr use-env mac-env)
          (receive-expr* (form meta) expr
            (if (not (= (length form) 2))
              (classify-error "bad quote syntax" form))
            (make-lit meta (syntax->datum (expr-strip-meta (cadr form))))))))))

;; (syntax-quote form) -> lit ast
(define (macrology/syntax-quote)
  (make-macrology
    (lambda (define-macro)
      (define-macro 'syntax-quote
        (lambda (expr use-env mac-env)
          (receive-expr* (form meta) expr
            (if (not (= (length form) 2))
              (classify-error "bad quote syntax" form))
            (make-lit meta (expr-strip-meta (cadr form)))))))))

(define (bind-syntax name value bind-env eval-env)
  (let* ((ast (classify value eval-env #t))
         (mac (if (macro? ast)
                ast 
                ;; Where the MAGIC happens
                (make-macro eval-env (eval-no-hook (expr-strip-meta (ast->expr ast)))))))
    (if (and (macro? mac) (procedure? (macro/procedure mac)))
      (syntactic-bind! bind-env name mac) 
      (classify-error "non-procedure macro" mac))))

(define (macrology/define-syntax)
  (make-macrology
    (lambda (define-macro)
      (define-macro 'define-syntax
        (lambda (expr use-env mac-env)
          (receive-expr* (form meta) expr
            (if (not (and (= (length form) 3)
                          (name? (expr*/form (cadr form)))))
              (classify-error "bad define-syntax syntax " form))
            (bind-syntax (expr*/form (cadr form)) (caddr form) use-env use-env)
            (make-lit meta (void))))))))

(define (let-syntax-helper expr mac-env bind-env eval-env)
  (receive-expr* (form meta) expr
    (if (not (>= (length form) 3))
      (classify-error "bad let-syntax " form))
    (map
      (lambda (x)
        (receive-expr* (form meta) x
          (if (not (name? (expr*/form (car form))))
            (classify-error "bad let-syntax name " form))
          (bind-syntax (expr*/form (car form)) (cadr form) bind-env eval-env)))
      (expr*/form (cadr form)))
    (syntactic-seal! bind-env)
    (let ((body `(,(close-syntax 'begin mac-env) ,@(cddr form))))
      (classify body bind-env #f))))

(define (macrology/let-syntax)
  (make-macrology
    (lambda (define-macro)
      (define-macro 'let-syntax
        (lambda (expr use-env mac-env)
          (define splicing-env (syntactic-splicing-extend use-env))
          (let-syntax-helper expr mac-env splicing-env use-env))))))

(define (macrology/letrec-syntax)
  (make-macrology
    (lambda (define-macro)
      (define-macro 'letrec-syntax
        (lambda (expr use-env mac-env)
          (define splicing-env (syntactic-splicing-extend use-env))
          (let-syntax-helper expr mac-env splicing-env splicing-env))))))
