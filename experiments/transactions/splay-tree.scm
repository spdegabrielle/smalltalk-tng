;; Splay trees, partly from Chris Okasaki's "Purely Functional Data
;; Structures" but mostly from Tom Lord's "Hackerlab" C library
;; implementation.
;;
;; The Hackerlab implementation is a bit buggy (eg. the implementation
;; of delete-root and raise) so its been useful mainly as an
;; implementation template - I've had to reverify the actual
;; algorithms from scratch to make sure they're correct here.

;; Algebraic data type; we use '() as the empty tree.
(define-record-type bst-node
  (make-bst-node element left right)
  bst-node?
  (element bst-node-element)
  (left bst-node-left)
  (right bst-node-right))

(define (bst? t)
  (or (null? t)
      (bst-node? t)))

(define (binary-curry f a)
  (lambda (b)
    (f a b)))

;; Suggested API, from Tom Lord's Hackerlab splay trees:
;;
;; (splay-tree-singleton x)
;; (splay-tree-find-raw predcmp tree)
;; (splay-tree-find-min tree)
;; (splay-tree-find-max tree)
;; (splay-tree-raise predcmp tree)
;; (splay-tree-raise-min tree)
;; (splay-tree-raise-max tree)
;; (splay-tree-insert-after tree x)
;; (splay-tree-insert-before tree x)
;; (splay-tree-delete-root tree)

(define (splay-tree-singleton x)
  (make-bst-node x '() '()))

;; Non-splaying search: returns a bst node, or #f
(define (splay-tree-find-raw predcmp tree)
  (let walk ((tree tree))
    (if (null? tree)
	#f
	(let ((order (predcmp (bst-node-element tree))))
	  (cond
	   ((negative? order) (walk (bst-node-left tree)))
	   ((positive? order) (walk (bst-node-right tree)))
	   (else tree))))))

(define (splay-tree-find-min tree)
  (if (null? tree)
      #f
      (let walk ((tree tree))
	(let ((left (bst-node-left tree)))
	  (if (null? left)
	      tree
	      (walk left))))))

(define (splay-tree-find-max tree)
  (if (null? tree)
      #f
      (let walk ((tree tree))
	(let ((right (bst-node-right tree)))
	  (if (null? right)
	      tree
	      (walk right))))))

(define (splay-tree-raise cmp-pivot tree)
  (if (null? tree)
      tree
      (let walk ((tree tree))
	(let* ((element (bst-node-element tree))
	       (left (bst-node-left tree))
	       (right (bst-node-right tree))
	       (order (cmp-pivot element)))
	  (cond
	   ((negative? order)
	    (if (null? left)
		tree
		(let* ((element2 (bst-node-element left))
		       (left2 (bst-node-left left))
		       (right2 (bst-node-right left))
		       (order2 (cmp-pivot element2)))
		  (cond
		   ((and (negative? order2) (bst-node? left2))
		    (let ((new2 (walk left2)))
		      (make-bst-node (bst-node-element new2)
				     (bst-node-left new2)
				     (make-bst-node element2
						    (bst-node-right new2)
						    (make-bst-node element right2 right)))))
		   ((and (positive? order2) (bst-node? right2))
		    (let ((new2 (walk right2)))
		      (make-bst-node (bst-node-element new2)
				     (make-bst-node element2 left2 (bst-node-left new2))
				     (make-bst-node element (bst-node-right new2) right))))
		   (else (make-bst-node element2 left2 (make-bst-node element right2 right)))))))
	   ((positive? order)
	    (if (null? right)
		tree
		(let* ((element2 (bst-node-element right))
		       (left2 (bst-node-left right))
		       (right2 (bst-node-right right))
		       (order2 (cmp-pivot element2)))
		  (cond
		   ((and (negative? order2) (bst-node? left2))
		    (let ((new2 (walk left2)))
		      (make-bst-node (bst-node-element new2)
				     (make-bst-node element left (bst-node-left new2))
				     (make-bst-node element2 (bst-node-right new2) right2))))
		   ((and (positive? order2) (bst-node? right2))
		    (let ((new2 (walk right2)))
		      (make-bst-node (bst-node-element new2)
				     (make-bst-node element2
						    (make-bst-node element left left2)
						    (bst-node-left new2))
				     (bst-node-right new2))))
		   (else (make-bst-node element2 (make-bst-node element left left2) right2))))))
	   (else tree))))))

(define (splay-tree-raise-min tree)
  (splay-tree-raise (lambda (v) -1) tree))

(define (splay-tree-raise-max tree)
  (splay-tree-raise (lambda (v) 1) tree))

(define (splay-tree-insert-after tree x)
  (cond
   ((null? tree) (splay-tree-singleton x))
   (else (make-bst-node x
			(make-bst-node (bst-node-element tree)
				       (bst-node-left tree)
				       '())
			(bst-node-right tree)))))

(define (splay-tree-insert-before tree x)
  (cond
   ((null? tree) (splay-tree-singleton x))
   (else (make-bst-node x
			(bst-node-left tree)
			(make-bst-node (bst-node-element tree)
				       '()
				       (bst-node-right tree))))))

;; Well, I invented this algorithm. Chances are it's inefficient, or
;; it doesn't work, or both.
(define (splay-tree-delete-root tree)
  (if (null? tree)
      (error "Cannot delete root of empty splay tree")
      (let* ((left (bst-node-left tree))
	     (right (bst-node-right tree))
	     (new-left (splay-tree-raise-max left)))
	(if (null? new-left)
	    right
	    (if (null? (bst-node-right new-left))
		(let ((new-tree (make-bst-node (bst-node-element new-left)
					       (bst-node-left new-left)
					       right)))
		  ;;(pretty-print (list 'DEL
		  ;;(bst->alist tree)
		  ;;(bst->alist new-tree)))
		  new-tree)
		(error "Invariant violation: need null right on max-raised splay tree"
		       (list (bst->alist left)
			     (bst->alist new-left))))))))

(define (splay-tree-insert cmp-pivot tree x)
  (if (null? tree)
      (splay-tree-singleton x)
      (let ((new-tree (splay-tree-raise cmp-pivot tree)))
	(if (positive? (cmp-pivot (bst-node-element new-tree)))
	    (splay-tree-insert-after new-tree x)
	    (splay-tree-insert-before new-tree x)))))

(define (splay-tree-insert/replace cmp-pivot tree x)
  (if (null? tree)
      (splay-tree-singleton x)
      (let* ((new-tree (splay-tree-raise cmp-pivot tree))
	     (order (cmp-pivot (bst-node-element new-tree))))
	(cond
	 ((negative? order) (splay-tree-insert-before new-tree x))
	 ((positive? order) (splay-tree-insert-after new-tree x))
	 (else (make-bst-node x (bst-node-left new-tree) (bst-node-right new-tree)))))))

(define (splay-tree-find predcmp tree k-found k-notfound)
  (if (null? tree)
      (k-notfound tree)
      (let ((new-tree (splay-tree-raise predcmp tree)))
	(if (zero? (predcmp (bst-node-element new-tree)))
	    (k-found new-tree)
	    (k-notfound new-tree)))))

(define (splay-tree-delete predcmp tree k-found . opt-k-notfound)
  (let ((k-notfound (if (null? opt-k-notfound) k-found (car opt-k-notfound))))
    (if (null? tree)
	(k-notfound tree)
	(let ((new-tree (splay-tree-raise predcmp tree)))
	  (if (zero? (predcmp (bst-node-element new-tree)))
	      (k-found (splay-tree-delete-root new-tree))
	      (k-notfound new-tree))))))

(define (bst->list t)
  (let walk ((t t)
	     (acc '()))
    (if (null? t)
	acc
	(walk (bst-node-left t)
	      (cons (bst-node-element t)
		    (walk (bst-node-right t) acc))))))

(define (bst->alist t)
  (let ((v (let walk ((t t))
	     (if (null? t)
		 (cons 0 t)
		 (let ((l (walk (bst-node-left t)))
		       (r (walk (bst-node-right t))))
		   (list (+ (max (car l) (car r)) 1)
			 (bst-node-element t)
			 (cdr l)
			 (cdr r)))))))
    `((height ,(car v))
      (tree ,(cdr v)))))

(define (bst-height t)
  (if (null? t)
      0
      (+ (max (bst-height (bst-node-left t))
	      (bst-height (bst-node-right t)))
	 1)))

(define (bst-size t)
  (if (null? t)
      0
      (+ (bst-size (bst-node-left t))
	 (bst-size (bst-node-right t))
	 1)))

(define (splay-tree-tests)
  (define (test)
    (let ((remove (lambda (i t)
		    (splay-tree-delete (binary-curry - i) t
				       (lambda (t)
					 (pretty-print (list 'FOUND i (bst->alist t)))
					 t)
				       (lambda (t)
					 (pretty-print (list 'NOTFOUND i (bst->alist t)))
					 t)))))
      (let* ((t '())
	     (t (do ((i 0 (+ i 1))
		     (t t (splay-tree-insert (lambda (b) (- (- 50 i) b))
					     (splay-tree-insert - t i)
					     (- 50 i))))
		    ((= i 10)
		     (pretty-print (list 'FINALINS (bst->alist t)))
		     t)
		  (pretty-print (list 'INTERIM i (bst->alist t)))))
	     (t (do ((i 0 (+ i 2))
		     (t t (remove i (remove (- 50 i 1) t))))
		    ((> i 50)
		     (pretty-print (list 'FINALDEL (bst->alist t)))
		     t))))
	'done)))
  (require 'srfi-1)
  (define (test2)
    (let ((t (time (do ((i 0 (+ i 1))
			(t '() (let ((v (random 10000)))
				 (splay-tree-insert (lambda (b) (- v b)) t v))))
		       ((= i 10000) t)))))
      (pretty-print (bst-height t))
      (let* ((oldt t)
	     (t (time (do ((i 0 (+ i 1))
			   (t t (splay-tree-find (binary-curry - (random 10000))
						 t
						 (lambda (t) t)
						 (lambda (t) t))))
			  ((= i 50000) t)))))
	(pretty-print (bst-height t))
	(pretty-print (eq? t oldt))
	(time
	 (let loop ((t t))
	   (if (null? t)
	       (pretty-print (bst-height t))
	       (let ((new-t (splay-tree-raise-min t)))
		 (loop (splay-tree-delete-root new-t)))))))))
  (test2))
