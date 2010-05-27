(require srfi/1) ;; lists
(require srfi/4) ;; homogeneous-numeric-vectors, u8vector
(require srfi/8) ;; receive
(require srfi/9) ;; records
(require srfi/13) ;; strings
(require scheme/pretty)

(print-struct #t)
(define previous-inspector (current-inspector))
(current-inspector (make-inspector))

;; define-record-types go here

(current-inspector previous-inspector)

;;---------------------------------------------------------------------------
;; Parser & support

(require "../../ometa-scheme/ometa.scm")
(ometa-library-path "../../ometa-scheme")

(define (same-line? a b)
  (cond
   ((eq? a 'none) #f)
   ((eq? b 'none) #f)
   (else (= (cadr a) (cadr b)))))

(define (same-column? a b)
  (= (caddr a) (caddr b)))

(define (below-and-rightward? a b)
  (or (eq? b 'none)
      (and (> (cadr a) (cadr b))
	   (> (caddr a) (caddr b)))))

(define (hoist-application v)
  (let loop ((v v)
	     (acc '()))
    (case (car v)
      ((appval*) (loop (caddr v) (cons (cadr v) acc)))
      ((appseq*) `(appseq ,@(reverse (cons (cadr v) acc)) ,(caddr v)))
      (else `(apply ,@(reverse (cons v acc)))))))

(define (convert-application v)
  (case (car v)
    ((appval* appseq*) (hoist-application v))
    (else v)))

;;---------------------------------------------------------------------------

(define read-etng* (load-ometa "r4.g"))

(define (slurp filename)
  (call-with-input-file filename
    (lambda (port)
      (let ((chunks (let loop ()
		      (let ((chunk (read-string 65536 port)))
			(if (eof-object? chunk)
			    '()
			    (cons chunk (loop)))))))
	(string-concatenate chunks)))))

(define (t str)
  (read-etng*
   'filetop
   str
   (lambda (result next err) (pretty-print `((result ,result) (err ,err))) (void))
   report-ometa-error))

;; (t (slurp "testdata.r4"))
(t "a b c d")
(t "a b || c d")
(t "a\n b\n || c d")
(t "a\n b\n || c\n     d")
(t "g f || msg")
(t "g (f || msg)")
