;; returns: List of AST
(define (classify* forms environment)
  (map (lambda (x) (classify x environment)) forms))


;; Form x Environment -> AST | Macro
(define (classify expr environment #!optional (allow-macro #f))
  (receive-expr* (form meta) expr
    (cond
      ((pair? form)
       (classify-pair expr environment))
      ((name? form)
       (classify-name expr environment allow-macro))
      ((syntactic-closure? form)
       (classify-syntactic-closure expr environment))
      (else
        ;; Self evaluating
        (make-lit meta form)))))

;; returns: AST
(define (classify-syntactic-closure expr environment)
  (receive-expr* (form meta) expr
    (let ((environment*
            (syntactic-filter (syntactic-closure/environment form)
                              (syntactic-closure/free-names form)
                              environment))
          (form*
            (syntactic-closure/form form)))
      (classify form* environment*))))

;; returns: Ref AST | Macro
(define (classify-name expr environment allow-macro)
  (receive-expr* (name meta) expr
    (cond ((syntactic-lookup environment name)
         => (lambda (denotation)
              (cond ((variable? denotation)
                     (make-ref meta name denotation))
                    ((macro? denotation)
                     (if allow-macro
                       denotation
                       (classify-error/expr "Invalid usage macro as variable" expr)))
                    (else
                      (classify-error/expr "Invalid denotation:" expr)))))
          (else
            ;; free-vars
            (make-ref meta (name->symbol name) #f)))))

(define (classify-pair expr environment)
  (receive-expr* (form meta) expr
    (let ((operator (classify (car form) environment #t)))
      (if (macro? operator)
        (classify-macro operator expr environment) 
        (classify-apply operator expr environment)))))

;; returns: Ref Ast
(define (classify-apply operator expr environment)
  (receive-expr* (form meta) expr
    (make-app meta
              operator
              (classify* (cdr form) environment))))

;; returns: Form
(define (classify-macro macro expr environment)
  (let ((result
          ((macro/procedure macro) expr environment (macro/environment macro))))
    (if (ast? result)
      result
      (classify result environment))))

(define (classify-error . msg)
  (apply error msg))

(define (classify-error/expr msg expr)
  (receive-expr* (_ meta) expr
    (apply classify-error (append (meta/disclose meta) (list msg) (list (expr-strip-meta expr))))))
