;; An implementation of transactional world state, after Henry Baker's
;; ideas in "Worlds in Collision: A Mostly Functional Model of
;; Concurrency Control and Recovery" (Unpublished memo, 1990).

;; NOTE: Not safe for preemptive concurrent use, yet - no locking
;; protocols have been implemented. However, this code is safe for use
;; in non-preemptive (coroutining) systems.

;; Ada database package specification, [Baker90] p6:
; package database is
;   type page is private;
;   subtype index is 0 .. (N-1);
;   type world is private;
;   null_world: constant world;
;   function lookup(i: index; w: world) return page;
;   function update(i: index; p: page; w: world) return world;
;   procedure assign_world(w: out world; w: world);
;   procedure nupdate(i: index; p: page; w: in out world);
; end database;

;;---------------------------------------------------------------------------
;; World Trees

(define-record-type world
  (make-world binding parent)
  world?
  (binding world-binding set-world-binding!)
  (parent world-parent set-world-parent!))

(define *database* (make-hash-table))
(define *default-page* #f)

(define (read-database i)
  (hash-table-ref *database* i *default-page*))

(define (write-database! i p)
  (hash-table-set! *database* i p))

(define (deep-binding)
  (let ()
    (define *null-world* '())

    (define (lookup i w)
      (let walk ((w w))
	(cond
	 ((or (null? w) (null? (world-parent w))) (read-database i))
	 ((eq? (car (world-binding w)) i) (cdr (world-binding w)))
	 (else (walk (world-parent w))))))

    (define (update i p w)
      (make-world (cons i p) w))

    (define (move-to w)
      w)

    (values *null-world*
	    lookup
	    update
	    move-to)))

(define (shallow-binding)
  (let ()
    (define *null-world* (make-world 'no-binding '()))

    (define (onestep nw ow)
      (set-world-parent! nw '())
      (set-world-parent! ow nw)
      (let* ((nbinding (world-binding nw))
	     (old-page (read-database (car nbinding))))
	(write-database! (car nbinding) (cdr nbinding))
	(set-cdr! nbinding old-page)
	(set-world-binding! nw (world-binding ow))
	(set-world-binding! ow nbinding))
      nw)

    (define (reroot w)
      (cond
       ((null? (world-parent w)) w)
       (else (onestep w (reroot (world-parent w))))))

    (define (lookup i w)
      (assert (null? (world-parent w)) "failed shallow assumption in lookup")
      (read-database i))

    (define (update i p w)
      (assert (null? (world-parent w)) "failed shallow assumption in update")
      (onestep (make-world (cons i p) w) w))

    (define (move-to w)
      (reroot w))

    (values *null-world*
	    lookup
	    update
	    move-to)))

(define (lazy-shallow-binding)
  (let ()
    (define *null-world* (make-world 'no-binding '()))

    (define (onestep nw ow)
      (set-world-parent! nw '())
      (set-world-parent! ow nw)
      (let* ((nbinding (world-binding nw))
	     (old-page (read-database (car nbinding))))
	(write-database! (car nbinding) (cdr nbinding))
	(set-cdr! nbinding old-page)
	(set-world-binding! nw (world-binding ow))
	(set-world-binding! ow nbinding))
      nw)

    (define (reroot w)
      (cond
       ((null? (world-parent w)) w)
       (else (onestep w (reroot (world-parent w))))))

    (define (lookup i w)
      (cond
       ((null? (world-parent w)) (read-database i))
       ((eq? (car (world-binding w)) i) (lookup i (reroot w)))
       (else (lookup i (world-parent w)))))

    (define (update i p w)
      (reroot (make-world (cons i p) w)))

    (define (move-to w)
      (reroot w))

    (values *null-world*
	    lookup
	    update
	    move-to)))

;;---------------------------------------------------------------------------
;; Transactions

(define-values (*null-world*
		lookup
		update
		move-to)
  (lazy-shallow-binding))

(define-syntax nupdate
  (syntax-rules ()
    ((_ i p w)
     (set! w (move-to (update i p w))))))

(define *current-world* *null-world*)

(define (non-nested-transaction body)
  (let* ((start-world (move-to *current-world*))
	 (commit (lambda (end-world)
		   (if (eq? *current-world* start-world)
		       (begin
			 (set! *current-world* end-world)
			 #t)
		       #f))))
    (body start-world commit)))
