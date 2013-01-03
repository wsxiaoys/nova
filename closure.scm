(define-type syntactic-closure
  constructor: make-syntactic-closure-internal
  (environment syntactic-closure/environment unprintable:)
  (free-names syntactic-closure/free-names unprintable:)
  (form syntactic-closure/form))

(define (make-syntactic-closure environment free-names form)
  (if (closure-elision-safe? environment free-names form)
      form
      (make-syntactic-closure-internal environment free-names form)))

(define (closure-elision-safe? environment free-names form)
  environment                           ;ignore
  (cond ((memq form free-names)         ;++ Is EQ? the best comparator here?
         #t)
        ((syntactic-closure? form)
         (not (or (pair? free-names)
                  (name? (syntactic-closure/form form)))))
        ;; This is not valid unless the environment in which the
        ;; syntactic closure is used agrees on whether the datum is
        ;; self-evaluating or not.
        ;;   ((self-evaluating? form environment)
        ;;    #t)
        (else #f)))

(define (close-syntax form environment)
  (make-syntactic-closure environment '() form))

(define (name? object)
  (or (symbol? object)
      (alias? object)))

(define (alias? object)
  (and (syntactic-closure? object)
       (name? (syntactic-closure/form object))))

(define (name->symbol name)
  (let ((lose (lambda ()
                (name->symbol (error "Not a name:" name)))))
    (let loop ((name name))
      (cond ((syntactic-closure? name) (loop (syntactic-closure/form name)))
            ((symbol? name) name)
            (else (lose))))))

(define (syntax->datum form)
  (if (let recur ((form form))
        (if (pair? form)
            (or (recur (car form))
                (recur (cdr form)))
            (syntactic-closure? form)))
      (let recur ((form form))
        (cond ((pair? form)
               (cons (recur (car form))
                     (recur (cdr form))))
              ((syntactic-closure? form)
               (recur (syntactic-closure/form form)))
              (else form)))
      form))
