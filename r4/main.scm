(require srfi/1) ;; lists
(require srfi/4) ;; homogeneous-numeric-vectors, u8vector
(require srfi/8) ;; receive
(require srfi/9) ;; records
(require srfi/13) ;; strings
(require srfi/69) ;; hash-tables
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

(load "compiler.scm")

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

(define (parse-etng-file-text str)
  (read-etng*
   'filetop
   str
   (lambda (result next err) result)
   report-ometa-error))

(define (print-etng v)
  (pretty-print v))

(define (print-etng-error exn)
  ((error-display-handler) (exn-message exn) exn)
  (newline))

(define collect-block
  (let ()
    (define (finish-block acc)
      (let ((result (string-join (reverse acc) "\n")))
	(if (string=? result "")
	    eof
	    result)))
    (lambda (port)
      (let loop ((acc '()))
	(if (pair? acc)
	    (display "...... ")
	    (display ">>R4>> "))
	(flush-output)
	(let ((line (read-line port)))
	  (cond
	   ((eof-object? line) (finish-block acc))
	   ((string=? line "") (if (pair? acc) (finish-block acc) (loop acc)))
	   (else (loop (cons line acc)))))))))

(define (load-etng-file e filename)
  (display "Loading ")
  (display filename)
  (display "...")
  (newline)
  (flush-output)
  (e (parse-etng-file-text (slurp filename))))

(define (etng-repl)
  (let* ((env (make-etng-scope #f))
	 (e (eval-etng env)))
    (load-etng-file e "boot.r4")
    (let read-block ()
      (let ((block-text (collect-block (current-input-port))))
	(if (eof-object? block-text)
	    'eof-reached
	    (let eval-block ((input block-text))
	      (read-etng* 'exprtop
			  input
			  (lambda (result next err)
			    (if (eq? result 'eof)
				(read-block)
				(begin
				  (pretty-print result)
				  (with-handlers ((exn:fail? print-etng-error))
				    (print-etng (e result))
				    (newline))
				  (when (and next (not (eq? next input)))
				    (eval-block next)))))
			  (lambda (err)
			    (display (format-ometa-error err))
			    (newline)
			    (read-block)))))))))

(etng-repl)
