(require 'srfi-1)

;---------------------------------------------------------------------------

(define-record-type slot
  (make-slot* name index roles delegating? kind)
  slot?
  (name slot-name)			;; symbol
  (index slot-index set-slot-index!)	;; #f, or integer offset into object-slots vector
  (roles slot-roles set-slot-roles!)	;; list of roles
  (delegating? slot-delegating?)	;; boolean
  (kind slot-kind))			;; 'method, 'immutable, or 'mutable

(define-record-type object
  (make-object* layout slots)
  object?
  (layout object-layout set-object-layout!)
  (slots object-slots set-object-slots!))

(define-record-type role
  (make-role* positions requirements method)
  role?
  (positions role-positions set-role-positions!)
  (requirements role-requirements)
  (method role-method set-role-method!))

(define-record-type layout
  (make-layout** hash map)
  layout?
  (hash layout-hash)
  (map layout-map))

(define layout-hash-factory
  (let ((counter 0))
    (lambda ()
      (let ((v counter))
	(set! counter (bitwise-and (+ counter 1) #xFFFFFF)) ;; some arbitrary wraparound
	v))))

(define (make-layout*)
  (make-layout** (layout-hash-factory)
		 (make-hash-table eq?)))

(define (layout-ref layout slot-name def)
  (hash-table-ref (layout-map layout) slot-name def))

(define (layout-set! layout slot-name value)
  (hash-table-set! (layout-map layout) slot-name value))

(define (layout-for-each layout fn)
  (hash-table-for-each fn (layout-map layout)))

(define (layout-remove! layout slot-name)
  (hash-table-remove! (layout-map layout) slot-name))

;---------------------------------------------------------------------------

(define *literal-objects* 'uninitialised-literal-objects)

(define (flush-literal-objects-table!)
  (set! *literal-objects* (make-hash-table eq?)))

(define (object-for-literal! x)
  (or (hash-table-ref *literal-objects* x #f)
      (let* ((ob (make-object* (make-layout*) (vector)))
	     (t (traits-for-primitive x)))
	(add-slot! ob 'traits (traits-for-primitive x) #t 'immutable)
	(hash-table-set! *literal-objects* x ob)
	ob)))

(define (install-object-for-literal! x ob)
  (unless (hash-table-ref *literal-objects* x #f)
    (hash-table-set! *literal-objects* x ob)))

(define (for-each-literal-object fn)
  (hash-table-for-each fn *literal-objects*))

(define (ensure-object! x)
  (if (object? x)
      x
      (object-for-literal! x)))

(define (object-or-false x)
  (if (object? x)
      x
      (hash-table-ref *literal-objects* x #f)))

(define-syntax ensure-object-var/create!
  (syntax-rules ()
    ((_ var)
     (begin
       (unless (object? var)
	 (set! var (object-for-literal! var)))))))

(define-syntax ensure-object-var/traits
  (syntax-rules ()
    ((_ var)
     (begin
       (unless (object? var)
	 (set! var (or (hash-table-ref *literal-objects* var #f)
		       (traits-for-primitive var))))))))

;---------------------------------------------------------------------------

(define (clone-object o)
  (let* ((o-slots (object-slots o))
	 (new-slots (make-vector (vector-length o-slots))))
    (vector-copy! o-slots new-slots)
    (make-object* (object-layout o)
		  new-slots)))

(define (clone-layout layout)
  (let ((new-layout (make-layout*)))
    (layout-for-each layout
		     (lambda (slot-name slot)
		       (layout-set! new-layout slot-name slot)))
    new-layout))

(define (clone-slot slot)
  (make-slot* (slot-name slot)
	      (slot-index slot)
	      (slot-roles slot)
	      (slot-delegating? slot)
	      (slot-kind slot)))

(define (merge-slot-kinds current new)
  (if (kind-is-non-slot? current)
      new
      current))

(define (kind-is-slot? kind)
  (not (eq? kind 'method)))

(define (kind-is-non-slot? kind)
  (eq? kind 'method))

(define (add-slot! o name value delegating? kind . should-add-accessors)
  (ensure-object-var/create! o)
  (debug 1 "Add-slot: "o" "name" "value" "delegating?" "kind" "should-add-accessors)
  (let* ((layout (clone-layout (object-layout o)))
	 (old-slot (layout-ref layout name #f))
	 (slot (if old-slot
		   (make-slot* name
			       (slot-index old-slot)
			       (slot-roles old-slot)
			       (or (slot-delegating? old-slot) delegating?)
			       (merge-slot-kinds (slot-kind old-slot) kind))
		   (make-slot* name
			       #f
			       '()
			       delegating?
			       kind))))
    (layout-set! layout name slot)
    (set-object-layout! o layout)
    (let ((index (slot-index slot)))
      (if (not index)
	  (let* ((new-index (vector-length (object-slots o)))
		 (new-slots (make-vector (+ new-index 1))))
	    (vector-copy! (object-slots o) new-slots)
	    (vector-set! new-slots new-index value)
	    (set-slot-index! slot new-index)
	    (set-object-slots! o new-slots))
	  (vector-set! (object-slots o) index value)))
    (when (and (or (null? should-add-accessors) (car should-add-accessors))
	       (kind-is-slot? kind)
	       (or (not old-slot)
		   (kind-is-non-slot? (slot-kind old-slot))))
      (add-accessors! o name (eq? kind 'immutable)))
    o))

(define (mutator-name-for name)
  (string->symbol (string-append (symbol->string name) ":")))

(define (add-accessors! o name immutable?)
  (debug 2 "Adding getter for "name" on "o)
  (add-roles!* name #f (make-getter-method-for name) (list o))
  (if (not immutable?)
      (let ((mutator-name (mutator-name-for name)))
	(debug 2 "Adding setter for "name" ("mutator-name") on "o)
	(add-roles!* mutator-name #f (make-setter-method-for name mutator-name)
		     ;; You'd think we'd need a two-element list here,
		     ;; but we don't. This is because of the sparse
		     ;; encoding of roles. Essentially, setter methods
		     ;; only have a constraint on their first
		     ;; argument, so we don't have to bother about the
		     ;; second argument at all.
		     (list o))))
  o)

(define (collect-bitset filter specialisers)
  (fold-left/index (lambda (index specialiser acc)
		     (if (filter specialiser)
			 (set-bit acc index)
			 acc))
		   *empty-bitset* specialisers))

(define (not-no-role? specialiser)
  (not (eq? specialiser *no-role*)))

(define (add-roles!* name clone-existing-slot? method specialisers)
  (let ((requirements (collect-bitset not-no-role? specialisers)))
    (for-each/index
     (lambda (index specialiser)
       (when (not-no-role? specialiser)
	 (add-role! specialiser name clone-existing-slot? index requirements method)))
     specialisers)))

(define (add-role! o name clone-existing-slot? index requirements method)
  (ensure-object-var/create! o)
  (let* ((layout (clone-layout (object-layout o)))
	 (old-slot (layout-ref layout name #f))
	 (slot (if old-slot
		   (if clone-existing-slot? (clone-slot old-slot) old-slot)
		   (make-slot* name
			       #f
			       '()
			       #f
			       'method))))
    (layout-set! layout name slot)
    (set-object-layout! o layout)
    (let update-roles ((roles (slot-roles slot)))
      (if (null? roles)
	  (set-slot-roles! slot
			   (cons (make-role* (set-bit *empty-bitset* index) requirements method)
				 (slot-roles slot)))
	  (let ((role (car roles)))
	    (if (eq? (role-method role) method)
		(set-role-positions! role (set-bit (role-positions role) index))
		(update-roles (cdr roles))))))
    (invalidate-method-cache!)
    o))

(define remove-slot!
  (let ()
    (define (splice-out-slot-value! o removed-index)
      (let* ((old-slots (object-slots o))
	     (old-slots-length (vector-length old-slots))
	     (new-slots (make-vector (- old-slots-length 1))))
	(do ((i 0 (+ i 1)))
	    ((= i removed-index))
	  (vector-set! new-slots i (vector-ref old-slots i)))
	(do ((i (+ removed-index 1) (+ i 1)))
	    ((= i old-slots-length))
	  (vector-set! new-slots (- i 1) (vector-ref old-slots i)))
	(set-object-slots! o new-slots)
	(vector-ref old-slots removed-index)))

    (define (fixup-other-slot-indices! layout removed-index)
      (layout-for-each layout
		       (lambda (slot-name slot)
			 (if (> (slot-index slot) removed-index)
			     (set-slot-index! slot (- (slot-index slot) 1))))))

    (define (remove-mutator-role! layout name)
      (let* ((mutator-name (mutator-name-for name))
	     (mutator-slot (layout-ref layout mutator-name #f)))
	(if mutator-slot
	    (let* ((new-slot (clone-slot mutator-slot)))
	      (set-slot-roles! new-slot
			       ;; Ought this to just remove *one*? Can there ever be more than one?
			       (filter (lambda (role)
					 (not (eq? (get-slot (role-method role) 'accessor) name)))
				       (slot-roles new-slot)))
	      (if (null? (slot-roles new-slot))
		  (layout-remove! layout mutator-name)
		  (layout-set! layout mutator-name new-slot))))))

    (lambda (o name)
      (and-let* ((o (object-or-false o)))
	(let* ((layout (clone-layout (object-layout o)))
	       (removed-slot (layout-ref layout name #f))
	       (removed-index (slot-index removed-slot)))
	  (set-object-layout! o layout)
	  (let ((old-value (splice-out-slot-value! o removed-index)))
	    (fixup-other-slot-indices! layout removed-index)
	    (layout-remove! layout name)
	    (if (eq? (slot-kind removed-slot) 'mutable)
		(remove-mutator-role! layout name))
	    (invalidate-method-cache!)
	    old-value))))))

(define replace-method!
  (let ()
    (define (find-specific-method name specialisers)
      (let ((requirements (collect-bitset not-no-role? specialisers)))
	(let loop ((found-methods #f)
		   (specialisers specialisers)
		   (index 0))
	  (cond
	   ((null? found-methods) #f)
	   ((null? specialisers)
	    (cond
	     ((not found-methods) #f)
	     ((pair? (cdr found-methods))
	      (error 'too-many-matches-candidates (list name specialisers)))
	     (else (car found-methods))))
	   (else
	    (and-let* ((specialiser (object-or-false (car specialisers))))
	      (if (not-no-role? specialiser)
		  (and-let* ((slot (layout-ref (object-layout specialiser) name #f)))
		    (let ((new-methods (map role-method
					    (filter (lambda (role)
						      (and (bit-set? (role-positions role)
								     index)
							   (bitset=? (role-requirements role)
								     requirements)))
						    (slot-roles slot)))))
		      (loop (if found-methods
				(lset-intersection eq? found-methods new-methods)
				new-methods)
			    (cdr specialisers)
			    (+ index 1))))
		  (loop found-methods (cdr specialisers) (+ index 1)))))))))

    (lambda (name specialisers new-method)
      (and-let* ((found-method (find-specific-method name specialisers)))
	(for-each (lambda (specialiser)
		    (let ((specialiser (object-or-false specialiser)))
		      (when (not-no-role? specialiser)
			(let* ((slot (clone-slot (layout-ref (object-layout specialiser)
							     name #f))))
			  (layout-set! (object-layout specialiser) slot)
			  (for-each (lambda (role)
				      (if (eq? (role-method role) found-method)
					  (set-role-method! role new-method)))
				    (slot-roles slot))))))
		  specialisers)
	found-method))))

(define (has-slot? object-or-primitive name)
  (let ((o (object-or-false object-or-primitive)))
    (if o
	(layout-ref (object-layout o) name #f)
	(eq? name 'traits))))

(define (get-slot object-or-primitive name)
  (let ((o (object-or-false object-or-primitive)))
    (if o
	(and-let* ((slot (layout-ref (object-layout o) name #f))
		   (index (slot-index slot)))
	  (vector-ref (object-slots o) index))
	(and (eq? name 'traits)
	     (traits-for-primitive object-or-primitive)))))

(define (set-slot! object-or-primitive name value)
  (and-let* ((o (object-or-false object-or-primitive))
	     (slot (layout-ref (object-layout o) name #f))
	     (index (slot-index slot)))
    (let* ((slots (object-slots o))
	   (old-value (vector-ref slots index)))
      (vector-set! slots index value)
      old-value)))

(define (add-roles! name method specialisers)
  (if (replace-method! name specialisers method)
      method
      (let ((requirements (collect-bitset not-no-role? specialisers)))
	(for-each/index (lambda (index specialiser)
			  (when (not-no-role? specialiser)
			    (add-role! specialiser name #f index requirements method)))
			specialisers))))

;; (define-method! (union symbol string)
;;                 (list-of symbol)
;;                 (list-of object) - use *no-role* in this list if needed
;;                 procedure)
;;  -> method
;;
(define (define-method! name formal-names formal-specialisers body)
  (let* ((selector (if (string? name) (string->symbol name) name))
	 (method (make-method* selector formal-names body)))
    (add-roles!* selector #t method formal-specialisers)
    (invalidate-method-cache!)
    method))

;---------------------------------------------------------------------------

(define-record-type method-cache-entry
  (make-method-cache-entry selector layouts method)
  method-cache-entry?
  (selector method-cache-entry-selector)
  (layouts method-cache-entry-layouts)
  (method method-cache-entry-method))

(define *method-cache-length* 512)

(define *method-cache* 'uninitialised-method-cache)
(define (invalidate-method-cache!)
  (set! *method-cache* (make-vector *method-cache-length* '())))
(invalidate-method-cache!)

(define (object-layout-for-cache x)
  (object-layout (cond
		  ((object? x) x)
		  ((hash-table-ref *literal-objects* x #f))
		  (else (traits-for-primitive x)))))

(define (object-layout-hash-for-cache x)
  (layout-hash (object-layout-for-cache x)))

(define (probe-for-cache selector args)
  (bitwise-and (bitwise-xor (hash selector *method-cache-length*)
			    (object-layout-hash-for-cache (vector-ref args 0)))
	       (- *method-cache-length* 1)))

(define (check-method-cache selector args)
  (let* ((probe (probe-for-cache selector args))
	 (entry (vector-ref *method-cache* probe)))
    (and (method-cache-entry? entry)
	 (eq? (method-cache-entry-selector entry) selector)
	 (let ((n (vector-length args))
	       (layouts (method-cache-entry-layouts entry)))
	   (and (= n (vector-length layouts))
		(let loop ((i 0))
		  (cond
		   ((= i n) (method-cache-entry-method entry))
		   ((eq? (object-layout-for-cache (vector-ref args i)) (vector-ref layouts i))
		    (loop (+ i 1)))
		   (else #f))))))))

(define (cache-method! method selector args)
  (let* ((n (vector-length args))
	 (layouts (make-vector n))
	 (probe (probe-for-cache selector args)))
    (do ((i 0 (+ i 1)))
	((= i n))
      (vector-set! layouts i (object-layout-for-cache (vector-ref args i))))
    (vector-set! *method-cache* probe (make-method-cache-entry selector
							       layouts
							       method))))

;---------------------------------------------------------------------------

(define-record-type bitset
  (make-bitset* bits)
  bitset?
  (bits bitset-bits))

(define *bitset-capacity* 31)

(define-record-printer (bitset b out)
  (for-each (lambda (x) (display x out))
            (list "#<bitset "(bitset->list b)">")))

(define (bit-set? bitset n)
  (if (>= n *bitset-capacity*) (error 'bitset-capacity-exceeded-in-bit-set?))
  (not (zero? (bitwise-and (bitset-bits bitset) (arithmetic-shift 1 n)))))

(define (set-bit bitset n)
  (if (>= n *bitset-capacity*) (error 'bitset-capacity-exceeded-in-set-bit))
  (make-bitset* (bitwise-ior (bitset-bits bitset) (arithmetic-shift 1 n))))

(define (clear-bit bitset n)
  (if (>= n *bitset-capacity*) (error 'bitset-capacity-exceeded-in-clear-bit))
  (make-bitset* (bitwise-and (bitset-bits bitset) (bitwise-not (arithmetic-shift 1 n)))))

(define (bitset=? b1 b2)
  (= (bitset-bits b1)
     (bitset-bits b2)))

(define (bitset->list b)
  (filter (lambda (n) (bit-set? b n))
	  (iota *bitset-capacity*)))

(define (list->bitset l)
  (fold (lambda (bit acc) (set-bit acc bit))
	*empty-bitset*
	l))

(define *empty-bitset* (make-bitset* 0))

;---------------------------------------------------------------------------
;; Rank vectors.
;;
;; Representation: bitfield, 28 bits wide; bits numbered >= 28 must be zero.
;;  - room for 7 arguments numbered 0 through 6, inclusive
;;  - bits [4n, 4n+3] are the delegation-depth at the (6-n)th argument
;; Thus:
;;   33222222222211111111110000000000
;;   10987654321098765432109876543210
;;   --------------------------------
;;   xxxx0000111122223333444455556666

(define *illegal-rank-vector-bits* (arithmetic-shift -1 (* 4 7)))
(define *max-rank-vector* (bitwise-not *illegal-rank-vector-bits*))

(define (rank-vector-update rv delegation-depth arg-index)
  (let ((result (if (or (> delegation-depth 15)
			(> arg-index 6))
		    (error 'out-of-range-in-rank-vector-update (list delegation-depth arg-index))
		    (let ((offset (* 4 (- 6 arg-index))))
		      (bitwise-ior (bitwise-and rv
						*max-rank-vector*
						(bitwise-not (arithmetic-shift #xF offset)))
				   (arithmetic-shift delegation-depth offset))))))
    (debug 4 "rank-vector-update "(number->string rv 16)" "delegation-depth" "arg-index
	   " --> "(number->string result 16))
    (if (not (zero? (bitwise-and rv *illegal-rank-vector-bits*)))
	(error "Illegal rank vector"))
    result))

(define rank-vector<?
  (lambda (a b)
    (let ((result (< a b)))
      (debug 4 "rank-vector<? "(number->string a 16)" "(number->string b 16)" --> "result)
      result)))
(define rank-vector>?
  (lambda (a b)
    (let ((result (> a b)))
      (debug 4 "rank-vector>? "(number->string a 16)" "(number->string b 16)" --> "result)
      result)))

;---------------------------------------------------------------------------

(define (role-active-at-position? role position)
  (bit-set? (role-positions role) position))

(define (role-requirements-filled? role positions)
  (bitset=? positions (role-requirements role)))

;---------------------------------------------------------------------------

(define (dispatch ignored-method selector args)
  (or (and (not ignored-method)
	   (check-method-cache selector args))
      (if ignored-method
	  (dispatch* ignored-method selector args)
	  (and-let* ((method (dispatch* ignored-method selector args)))
	    (cache-method! method selector args)
	    method))))

(define (dispatch* ignored-method selector args)
  (debug 3 --> 0 "Dispatch "selector" "(vector-length args))
  (let* ((num-args (vector-length args))
	 (most-specific-method #f)
; 	 (DEBUG-ids (make-hash-table eq?))
; 	 (DEBUG-counter 10000)
; 	 (DEBUG-id (lambda (x) (or (hash-table-ref DEBUG-ids x)
; 				   (let ((c DEBUG-counter))
; 				     (set! DEBUG-counter (+ c 1))
; 				     (hash-table-set! DEBUG-ids x c)
; 				     c))))
	 (accessor-target #f)
	 (candidate-status (make-hash-table eq?))
	 (delegations-seen (make-hash-table eq?))
	 (rank-table (make-hash-table eq?))
	 (rank-vector-for (lambda (method deft) (hash-table-ref rank-table method deft))))
    (do ((arg-index 0 (+ arg-index 1)))
	((= arg-index num-args))
      (let search-delegates ((delegates (list (vector-ref args arg-index)))
			     (delegation-depth 0))
	(unless (null? delegates)
	  (let* ((maybe-primitive-delegate (car delegates))
		 (delegate (let ((d maybe-primitive-delegate))
			     (ensure-object-var/traits d)
			     d))
		 (remaining-delegates (cdr delegates))
		 (delegate-layout (object-layout delegate))
		 (selected-slot (layout-ref delegate-layout selector #f)))
;	    (debug 1 --> 0 "Inspecting "maybe-primitive-delegate" resolving to "delegate)
;	    (debug 6 --> 0 "Inspecting "(DEBUG-id delegate)
;		   " slot "selected-slot" depth "delegation-depth" arg index "arg-index)
	    (when selected-slot
	      (for-each (lambda (role)
			  (when (role-active-at-position? role arg-index)
			    (let* ((method (role-method role))
				   (rankvec0 (rank-vector-for method *max-rank-vector*))
				   (rankvec (rank-vector-update
					     rankvec0 delegation-depth arg-index))
				   (positions0 (hash-table-ref
						candidate-status method *empty-bitset*))
				   (positions (set-bit positions0 arg-index)))
			      (hash-table-set! rank-table method rankvec)
			      (hash-table-set! candidate-status method positions)
			      (when (and (role-requirements-filled? role positions)
					 (or (not ignored-method)
					     (rank-vector>? rankvec
							    (rank-vector-for ignored-method -1)))
					 (or (not most-specific-method)
					     (rank-vector<? rankvec
							    (rank-vector-for
							     most-specific-method -1))))
				(cond
				 ((eq? (get-slot method 'accessor) *nil*)
				  (set! accessor-target #f))
				 ((= arg-index 0)
				  (set! accessor-target maybe-primitive-delegate)))
				(set! most-specific-method method)))))
			(slot-roles selected-slot)))
	    (layout-for-each delegate-layout
	     (lambda (slot-name slot)
	       (and-let* ((_		(slot-delegating? slot))
			  (new-delegate	(vector-ref (object-slots delegate) (slot-index slot)))
			  (_		(not (eq? new-delegate *nil*)))
			  (seen-in-positions (hash-table-ref delegations-seen new-delegate
							     *empty-bitset*))
			  (_		(not (bit-set? seen-in-positions arg-index))))
		 (hash-table-set! delegations-seen new-delegate
				  (set-bit seen-in-positions arg-index))
;		 (debug 6 --> 0 "Delegating via "slot-name" of "(DEBUG-id delegate)
;			" to "(DEBUG-id new-delegate) " at level "delegation-depth" pos "arg-index)
		 (set! remaining-delegates (cons new-delegate remaining-delegates)))))
	    (search-delegates remaining-delegates (+ delegation-depth 1))))))
    (if most-specific-method
	(begin
	  (when accessor-target
	    (vector-set! args 0 accessor-target))
	  most-specific-method)
	#f)))
