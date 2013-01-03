;;; Expression methods
(define (expr? obj) ;; returns false for expressions without 'attached notes'
  (and (vector? obj)
       (##fixnum.= (##vector-length obj) 4)
       (let ((v0 (##vector-ref obj 0)))
	 (and (vector? v0)
	      (##fixnum.= (##vector-length v0) 1)
	      (let ((v00 (##vector-ref v0 0)))
		(case v00
		  ((source1 source2) #t)
		  (else #f)))))))

(define (expr/form expr)
  (vector-ref expr 1))

(define (expr/meta expr)
  (define (pos/line pos)
    (+ 1 (bitwise-and pos 65535)))
  (define (pos/col pos)
    (+ 1 (quotient pos 65536)))
  (let ((type (vector-ref expr 0))
        (file (or (vector-ref expr 2) "(generated)"))
        (pos (or (vector-ref expr 3) 0)))
    (list type file (pos/line pos) (pos/col pos))))

(define (expr*/form expr)
  (if (expr? expr)
    (expr/form expr) 
    expr))

(define (expr*/meta expr)
  (if (expr? expr)
    (expr/meta expr)
    #f))

(define-macro (receive-expr* sym expr . body)
  `(receive ,sym
     (if (expr? ,expr)
         (values (expr/form ,expr) (expr/meta ,expr))
         (values ,expr #f))
     ,@body))

(define (expr-strip-meta expr)
  (receive-expr* (v _) expr
    (cond ((pair? v)
           (map* expr-strip-meta v))
          ((vector? v)
           (vector-map expr-strip-meta v))
          (else v))))

(define (make-expr form meta)
  (define (make-pos line col)
    (+ (##fixnum.- line 1)
       (* (##fixnum.- col 1) 65536)))
  (vector (car meta) form (cadr meta) (make-pos (caddr meta) (cadddr meta))))

(define (meta/disclose meta)
  `(File: ,(cadr meta) Line: ,(caddr meta) Col: ,(cadddr meta)))
