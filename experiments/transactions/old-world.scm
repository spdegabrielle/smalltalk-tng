;; An implementation of transactional world state, after Henry Baker's
;; ideas in "Worlds in Collision: A Mostly Functional Model of
;; Concurrency Control and Recovery" (Unpublished memo, 1990).

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

;; This implementation:
;; - page == object
;; - index == symbol
;; - world == opaque map from index to page

(require 'splay-tree)

(define-record-type world
  (make-world map)
  world?
  (map world-map set-world-map!))

(define (entry-cmp ea eb)
  (let ((a (car ea))
	(b (car eb)))
    (cond
     ((eq? a b) 0)
     ((string<? (symbol->string a) (symbol->string b)) -1)
     (else 1))))

(define (entry-cmp1 a)
  (let ((sa (symbol->string a)))
    (lambda (eb)
      (let ((b (car eb)))
	(cond
	 ((eq? a b) 0)
	 ((string<? sa (symbol->string b)) -1)
	 (else 1))))))

(define (deep-binding)
  ;; switching from world-to-world: O(1)
  ;; lookup: O(M)
  ;; update/world-create: O(1)
  (let ()
    (define null-world (make-world '()))

    (define (lookup i w default-page)
      (cond
       ((assq i (world-map w)) => cdr)
       (else default-page)))

    (define (update i p w)
      (make-world (cons (cons i p) (world-map w))))

    (define (assign-world w1 w2)
      (set-world-map! w1 (world-map w2)))

    (define (nupdate i p w)
      (assign-world w (update i p w)))

    (values null-world
	    lookup
	    update
	    assign-world
	    nupdate)))

(define (deep-binding/splay-tree)
  ;; switching from world-to-world: O(1)
  ;; lookup: O(log N)
  ;; update/world-create: O(log N)
  (let ()
    (define null-world (make-world '()))

    (define (lookup i w default-page)
      (let ((cmp (entry-cmp1 i)))
	(splay-tree-find cmp
			 (world-map w)
			 (lambda (new-map)
			   (set-world-map! w new-map)
			   (cdr (bst-node-element new-map)))
			 (lambda (new-map)
			   (set-world-map! w new-map)
			   default-page))))

    (define (update i p w)
      (let ((cmp (entry-cmp1 i)))
	(make-world (splay-tree-insert/replace cmp
					       (world-map w)
					       (cons i p)))))

    (define (assign-world w1 w2)
      (set-world-map! w1 (world-map w2)))

    (define (nupdate i p w)
      (assign-world w (update i p w)))

    (values null-world
	    lookup
	    update
	    assign-world
	    nupdate)))

(define (shallow-binding)
  (let ()
    (define null-world (make-world '()))

    (define (onestep nw ow)
      (set-cdr! (

    (define (lookup i w default-page)
      (cond
       ((assq i (world-map w)) => cdr)
       (else default-page)))

    (define (update i p w)
      (make-world (cons (cons i p) (world-map w))))

    (define (assign-world w1 w2)
      (set-world-map! w1 (world-map w2)))

    (define (nupdate i p w)
      (assign-world w (update i p w)))

    (values null-world
	    lookup
	    update
	    assign-world
	    nupdate)))