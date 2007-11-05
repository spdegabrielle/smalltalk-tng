(define-struct stream (stepper0 state0))

(define (stream-stepper stream-or-list)
  (if (or (null? stream-or-list)
	  (pair? stream-or-list))
      list-stream-stepper
      (stream-stepper0 stream-or-list)))

(define (stream-state stream-or-list)
  (if (or (null? stream-or-list)
	  (pair? stream-or-list))
      stream-or-list
      (stream-state0 stream-or-list)))

(define (stream-maker stepper)
  (lambda (state)
    (make-stream stepper state)))

(define (list-stream-stepper l done skip yield)
  (if (null? l)
      (done)
      (yield (car l) (cdr l))))

(define list->stream (stream-maker list-stream-stepper))

(define (stream->list stream)
  (sfoldr cons '() stream))

(define (smap f stream)
  (let ((stepper (stream-stepper stream)))
    (make-stream (lambda (state done skip yield)
		   (stepper state
			    done
			    skip
			    (lambda (elt new-state) (yield (f elt) new-state))))
		 (stream-state stream))))

(define (sfilter pred stream)
  (let ((stepper (stream-stepper stream)))
    (make-stream (lambda (state done skip yield)
		   (stepper state
			    done
			    skip
			    (lambda (elt new-state) (if (pred elt)
							(yield elt new-state)
							(skip new-state)))))
		 (stream-state stream))))

(define (sfoldr kons knil stream)
  (let ((stepper (stream-stepper stream)))
    (let loop ((state (stream-state stream)))
      (stepper state
	       (lambda () knil)
	       (lambda (new-state) (loop new-state))
	       (lambda (elt new-state) (kons elt (loop new-state)))))))

(define (sfoldl kons knil stream)
  (let ((stepper (stream-stepper stream)))
    (let loop ((knil knil)
	       (state (stream-state stream)))
      (stepper state
	       (lambda () knil)
	       (lambda (new-state) (loop new-state))
	       (lambda (elt new-state) (loop (kons elt knil) new-state))))))

(define-struct szip-state (cell left right))

(define (szip left right)
  (let ((left-stepper (stream-stepper left))
	(right-stepper (stream-stepper right)))
    (make-stream (lambda (state done skip yield)
		   (let ((cell (szip-state-cell state)))
		     (cond
		      ((null? cell)
		       (right-stepper
			(szip-state-right state)
			done
			(lambda (new-right)
			  (skip (make-szip-state '() (szip-state-left state) new-right)))
			(lambda (elt new-right)
			  (skip (make-szip-state (list elt) (szip-state-left state) new-right)))))
		      (else
		       (left-stepper
			(szip-state-left state)
			done
			(lambda (new-left)
			  (skip (make-szip-state cell new-left (szip-state-right state))))
			(lambda (elt new-left)
			  (yield (cons elt cell)
				 (make-szip-state '() new-left (szip-state-right state)))))))))
		 (make-szip-state '() (stream-state left) (stream-state right)))))

(define-struct sconcatmap-state (first-stepper first-state remaining-streams))

(define (sconcatmap f streams)
  (let ((remaining-streams-stepper (stream-stepper streams)))
    (make-stream (lambda (state done skip yield)
		   (let ((first-stepper (sconcatmap-state-first-stepper state)))
		     (if first-stepper
			 (first-stepper (sconcatmap-state-first-state state)
					(lambda ()
					  (skip (make-sconcatmap-state
						 #f #f
						 (sconcatmap-state-remaining-streams state))))
					(lambda (new-first-state)
					  (skip (make-sconcatmap-state
						 first-stepper new-first-state
						 (sconcatmap-state-remaining-streams state))))
					(lambda (elt new-first-state)
					  (yield elt
						 (make-sconcatmap-state
						  first-stepper new-first-state
						  (sconcatmap-state-remaining-streams state)))))
			 (remaining-streams-stepper (sconcatmap-state-remaining-streams state)
						    done
						    (lambda (new-remaining-streams)
						      (skip (make-sconcatmap-state
							     #f #f
							     new-remaining-streams)))
						    (lambda (first new-remaining-streams)
						      (let ((first-stream (f first)))
							(skip (make-sconcatmap-state
							       (stream-stepper first-stream)
							       (stream-state first-stream)
							       new-remaining-streams))))))))
		 (make-sconcatmap-state #f #f (stream-state streams)))))

(define (sconcatenate streams)
  (sconcatmap (lambda (stream) stream) streams))
