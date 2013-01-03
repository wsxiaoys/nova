(define-type syntactic-environment
  (operations syntactic-environment/operations unprintable:)
  (parent syntactic-environment/parent unprintable:)
  (data syntactic-environment/data set-syntactic-environment/data! unprintable:))

(define-type syntactic-operations
  (lookup syntactic-operations/lookup)
  (bind! syntactic-operations/bind!)
  (seal! syntactic-operations/seal!)
  (disclose syntactic-operations/disclose))

(define (null-syntactic-environment parameters . context)
  (make-syntactic-environment null-syntactic-operations #f context))

(define null-syntactic-operations
  (let ()
    (define (lose operation)
      (error "Null syntactic environment:" operation))
    (make-syntactic-operations
     (lambda (environment name)
       (lose `(syntactic-lookup ,environment ,name)))
     (lambda (environment name denotation)
       (lose `(syntactic-bind! ,environment ,name ,denotation)))
     (lambda (environment)
       (lose `(syntactic-seal! ,environment)))
     (lambda (environment)
       `(NULL ,@(syntactic-environment/data environment))))))

;;; `Sealing' changes the state of the environment to indicate that no
;;; further changes will be made to it.  In the case of splicing
;;; environments, this means that all new bindings will be made in the
;;; parent; 

(define (syntactic-seal! environment)
  ((syntactic-operations/seal! (syntactic-environment/operations environment))
   environment))

(define (syntactic-lookup environment name)
  ((syntactic-operations/lookup (syntactic-environment/operations environment))
   environment name))

(define (syntactic-bind! environment name denotation)
  (if (not (name? name))
    (classify-error "Cannot bind non-identifier " name denotation))
  ((syntactic-operations/bind! (syntactic-environment/operations environment))
   environment name denotation))

(define (disclose-syntactic-environment environment)
  ((syntactic-operations/disclose
    (syntactic-environment/operations environment))
   environment))

;; TODO: allocate-location
(define (bind-variable! name environment)
  (let ((variable (make-variable name (allocate-location environment name))))
    (syntactic-bind! environment name variable)
    variable))

(define (name=? environment-a name-a environment-b name-b)
  (if (and (name? name-a) (name? name-b))
    (let ((denotation-a (syntactic-lookup environment-a name-a))
          (denotation-b (syntactic-lookup environment-b name-b)))
      (cond ((and denotation-a denotation-b)
             (denotation=? denotation-a denotation-b))
            ((and (not denotation-a) (not denotation-b))
             (eq? (name->symbol name-a)
                  (name->symbol name-b)))
            (else #f)))
    #f))

;;;; Extended Environments
;;; Extended environments are simply frames in the environment tree
;;; in which local bindings can be introduced.
(define (syntactic-extend environment)
  (make-syntactic-environment extended-syntactic-operations
                              environment
                              '()))

(define extended-syntactic-operations
  (let ()
    (define (local-bindings environment)
      (syntactic-environment/data environment))
    (define (set-local-bindings! environment bindings)
      (set-syntactic-environment/data! environment bindings))
    (make-syntactic-operations
     (lambda (environment name)         ;lookup
       (cond ((assq name (local-bindings environment))
              => cdr)
             (else
              (syntactic-lookup (syntactic-environment/parent environment)
                                name))))
     (lambda (environment name denotation) ;bind!
       ;++ This should report more useful (and restartable) errors.
       (cond ((assq name (local-bindings environment))
              => (lambda (original-binding)
                   (error "Rebinding name:" environment name denotation
                          `(was ,(cdr original-binding)))))
             (else
              (set-local-bindings! environment
                                   (cons (cons name denotation)
                                         (local-bindings environment))))))
     (lambda (environment)              ;seal!
       (void))
     (lambda (environment)              ;disclose
       `(EXTENDED ,(map car (local-bindings environment)))))))

;;;; Filtered Environments
;;; Filtered environments are used to classify the form of a syntactic
;;; closure; they make the free names of syntactic closures work.

(define (syntactic-filter closing-environment free-names free-environment)
  (if (or (not (pair? free-names))
          (eq? closing-environment free-environment))
      closing-environment
      (make-syntactic-environment
       filtered-syntactic-operations
       closing-environment
       (cons free-environment free-names))))

(define filtered-syntactic-operations
  (let ()
    (define (closing-environment environment)
      (syntactic-environment/parent environment))
    (define (free-environment environment)
      (car (syntactic-environment/data environment)))
    (define (free-names environment)
      (cdr (syntactic-environment/data environment)))
    (define (choose-parent environment name)
      ((if (memq name (free-names environment))
           free-environment
           closing-environment)
       environment))
    (make-syntactic-operations
     (lambda (environment name)         ;lookup
       (syntactic-lookup (choose-parent environment name) name))
     (lambda (environment name denotation) ;bind!
       (syntactic-bind! (choose-parent environment name) name denotation))
     (lambda (environment)              ;seal!
       (void))
     (lambda (environment)              ;disclose
       `(FILTERED ,(free-names environment))))))

;;;; Splicing Environments

;;; These are for implementing LET-SYNTAX and LETREC-SYNTAX.

(define (syntactic-splicing-extend environment)
  (make-syntactic-environment splicing-syntactic-operations
                              environment
                              (cons #f '())))

(define splicing-syntactic-operations
  (let ()
    (define (sealed? environment)
      (car (syntactic-environment/data environment)))
    (define (seal! environment)
      (set-car! (syntactic-environment/data environment) #t))
    (define (local-bindings environment)
      (cdr (syntactic-environment/data environment)))
    (define (set-local-bindings! environment bindings)
      (set-cdr! (syntactic-environment/data environment) bindings))
    (make-syntactic-operations
     (lambda (environment name)         ;lookup
       (cond ((assq name (local-bindings environment))
              => cdr)
             (else
              (syntactic-lookup (syntactic-environment/parent environment)
                                name))))
     (lambda (environment name denotation) ;bind!
       (cond ((assq name (local-bindings environment))
              => (lambda (original-binding)
                   (error "Rebinding name:" environment name denotation
                          `(was ,(cdr original-binding)))))
             ((sealed? environment)
              (syntactic-bind! (syntactic-environment/parent environment)
                               name
                               denotation))
             (else
              (set-local-bindings! environment
                                   (cons (cons name denotation)
                                         (local-bindings environment))))))
     (lambda (environment)              ;seal!
       (seal! environment))
     (lambda (environment)              ;disclose
       `(SPLICING ,(map car (local-bindings environment)))))))

;;;; Macrologies

(define (apply-macrology macrology environment)
  (macrology environment))

(define null-macrology
  (lambda (environment)
    environment                         ;ignore
    (values)))

(define (make-macrology receiver)
  (lambda (environment)
    (define (define-macro name procedure)
      (syntactic-bind! environment name (make-macro environment procedure)))
    (receiver define-macro)))

(define (compose-macrologies . macrologies)
  (reduce compose-macrology null-macrology macrologies))

(define (compose-macrology macrology-a macrology-b)
  (lambda (environment)
    (macrology-a environment)
    (macrology-b environment)))
