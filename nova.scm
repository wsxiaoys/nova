;; Required by bind-variable!
(define current-location-uid (make-parameter 0))
(define (allocate-location environment name)
  (if (not (syntactic-environment/parent environment))
      name
      (let ((uid (current-location-uid)))
        (current-location-uid (+ uid 1))
        uid)))
(define (map* proc lst)
  (let recur ((lst lst))
    (cond ((pair? lst)
           (cons (proc (car lst))
                 (recur (cdr lst))))
          ((null? lst) '())
          (else
            (proc lst)))))
(define (vector-map fn vec)
  (let* ((len (vector-length vec))
         (v (make-vector len)))
    (let loop ((i 0))
      (cond
       ((< i len)
        (vector-set! v
                     i
                     (fn (vector-ref vec i)))
        (loop (+ 1 i)))))
    v))
(define (eval-no-hook expr)
  (let ((hook ##expand-source))
    (dynamic-wind
        (lambda ()
          (set! ##expand-source (lambda (src) src)))
        (lambda ()
          (eval expr))
        (lambda ()
          (set! ##expand-source hook)))))

(include "expr.scm")
(include "ast.scm")
(include "denotation.scm")
(include "environment.scm")
(include "closure.scm")
(include "classify.scm")
(include "standard.scm")

(define top-level-operations
  (let ()
    (define (global-bindings environment)
      (syntactic-environment/data environment))
    (define (set-global-bindings! environment bindings)
      (set-syntactic-environment/data! environment bindings))
    (make-syntactic-operations
     (lambda (environment name)         ;lookup
       (cond ((assq name (global-bindings environment))
              => cdr)
             ((syntactic-closure? name)
              (syntactic-lookup (syntactic-closure/environment name)
                                (syntactic-closure/form name)))
             (else
               #f)))
     (lambda (environment name denotation) ;bind!
       (set-global-bindings! environment
                             (cons (cons name denotation)
                                   (global-bindings environment))))
     (lambda (environment)              ;seal!
       (void))
     (lambda (environment)              ;disclose
       environment ;ignore
       `(TOPLEVEL ,(map car (global-bindings environment)))))))

(define (make-top-level-environment)
  (let ((environment
         (make-syntactic-environment top-level-operations
                                     #f
                                     '())))
    environment))


;; Core forms
(define core-form-environment
  (make-top-level-environment))
(apply-macrology (macrology/define) core-form-environment)
(apply-macrology (macrology/set!) core-form-environment)
(apply-macrology (macrology/begin) core-form-environment)
(apply-macrology (macrology/lambda) core-form-environment)
(apply-macrology (macrology/if) core-form-environment)
(apply-macrology (macrology/quote) core-form-environment)
(apply-macrology (macrology/syntax-quote) core-form-environment)
(apply-macrology (macrology/define-syntax) core-form-environment)
(apply-macrology (macrology/let-syntax) core-form-environment)
(apply-macrology (macrology/letrec-syntax) core-form-environment)

(define (hook expr)
  (define ast #f)
  (define source #f)
  (define sexp #f)
  (set! ast (classify expr core-form-environment))
  ;(display "AST: ")
  ;(pp ast) 

  (set! source (ast->expr ast))
  ;(display "Source: ")
  ;(pp source)
  
  (set! sexp (expr-strip-meta source))
  (display "Sexp: ")
  (pp sexp) 
  source)
(set! ##expand-source hook)

;(ast->sexp (classify '((lambda (x) 4) '(1 2 3 4)) core-form-environment)) 

