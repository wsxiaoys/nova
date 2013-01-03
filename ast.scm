;; AST definition
(define-type ast
  extender: define-ast-type
  (meta ast/meta))

(define-ast-type lit
  (value lit/value))

(define-ast-type ref
  (name ref/name)          ;; name?
  (variable ref/variable)) ;; variable?, maybe #f

(define-ast-type set
  (variable set/variable) ;; ref?
  (value set/value)) ;; ast?

(define-ast-type def
  (variable def/variable) ;; ref?
  (value def/value)) ;; (promise of ast?)

(define-ast-type app
  (operator app/operator) ;; ast?
  (params app/params))  ;; list of ast?

(define-ast-type seq
  (body seq/body))  ;; list of ast?

(define-ast-type lam
  (params lam/params)            ;; list of ref?
  (body lam/body)                ;; list of ast?
  (environment lam/environment unprintable:)) ;; environment

(define-ast-type cnd
  (test cnd/test) ;; ast?
  (success cnd/success) ;; ast?
  (fail cnd/fail)) ;; ast?

(define (compile-ref ref)
  (if (not (ref/variable ref))
    (name->symbol (ref/name ref))
    (let ((name (variable/name (ref/variable ref)))
          (location (variable/location (ref/variable ref))))
      (cond ((number? location)
             (string->symbol
               (string-append (symbol->string (name->symbol name))
                              "."
                              (number->string location #d10))))
              ((name? location)
               (name->symbol location))
            (else
              (error "Variable has bogus location" location))))))

(define (ast->expr x #!optional (meta '(#(source1) "(generated)" 1 1)))
  (let* ((a2e (lambda (expr) (ast->expr expr (or (ast/meta x) meta))))
         (sexp
           (cond
             ((lam? x)
              `(##lambda ,(map* a2e (lam/params x))
                ,@(map a2e (lam/body x))))
             ((cnd? x) `(##if ,(a2e (cnd/test x)) ,(a2e (cnd/success x)) ,(a2e (cnd/fail x))))
             ((set? x) `(##set! ,(a2e (set/variable x)) ,(a2e (set/value x))))
             ((def? x) `(##define ,(a2e (def/variable x)) ,(a2e (force (def/value x)))))
             ((ref? x) (compile-ref x))
             ((seq? x) `(##begin ,@(map a2e (seq/body x))))
             ((lit? x)
              (let ((v (lit/value x)))
                (if (or (pair? v) (vector? v) (null? v) (symbol? v)) `',v v)))
             ((app? x) (cons (a2e (app/operator x)) (map a2e (app/params x))))
             (else
               (classify-error "Invalid ast type:" x)))))
    (make-expr sexp (or (ast/meta x) meta))))
