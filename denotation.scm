(define-type macro
  (environment macro/environment unprintable:)
  (procedure macro/procedure unprintable:))

(define-type variable
  (name variable/name)
  (location variable/location))

(define (denotation=? denotation-a denotation-b)
  (or (eq? denotation-a denotation-b)
      (and (variable? denotation-a)
           (variable? denotation-b)
           (eqv? (variable/location denotation-a)
                 (variable/location denotation-b)))))
