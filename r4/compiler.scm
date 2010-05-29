'(define ast-node-patterns
   (<block> (block <expr> ...))
   (<expr> (begin <expr> ...)
	   (apply <rator> <rand> ...)
	   (appseq <rator> <rand> ... <seqrand>)
	   (function ((<patexp> <block>) ...))
	   (list <expr> ...)
	   <block>
	   (binding <identifier>)
	   (varref <identifier>)
	   (lit <literal>)
	   (%assemble ((<identifier> <identifier>) ...) <scheme-expression>)))

;;---------------------------------------------------------------------------
(define previous-inspector (current-inspector))
(current-inspector (make-inspector))

(define-record-type etng-object
  (make-etng-object env self-name clauses)
  etng-object?
  (env etng-object-env)
  (self-name etng-object-self-name)
  (clauses etng-object-clauses))

(define-record-type etng-merge
  (make-etng-merge base derived)
  etng-merge?
  (base etng-merge-base)
  (derived etng-merge-derived))

(define-record-type etng-macro
  (make-etng-macro handler)
  etng-macro?
  (handler etng-macro-handler))

(define-record-type etng-symbol
  (make-etng-symbol namespace name)
  etng-symbol?
  (namespace etng-symbol-namespace)
  (name etng-symbol-name))

(define-record-type etng-binding
  (make-etng-binding identifier)
  etng-binding?
  (identifier etng-binding-identifier))

(define-record-type etng-scope
  (make-etng-scope* parent bindings)
  etng-scope?
  (parent etng-scope-parent)
  (bindings etng-scope-bindings))

(current-inspector previous-inspector)
;;---------------------------------------------------------------------------

(define (make-etng-scope parent)
  (make-etng-scope* parent (make-hash-table equal?)))

(define (etng-scope-parent* env n)
  (if (zero? n)
      env
      (etng-scope-parent* (etng-scope-parent env) (- n 1))))

(define (etng-scope->list env)
  (if (not env)
      '()
      (cons (hash-table-keys (etng-scope-bindings env))
	    (etng-scope->list (etng-scope-parent env)))))

(define (etng->printable v)
  (cond
   ((etng-object? v) `(OBJECT ,(etng-scope->list (etng-object-env v))
			      ,@(etng-object-clauses v)))
   ((pair? v) (cons (etng->printable (car v))
		    (etng->printable (cdr v))))
   (else v)))

(define (etng-define! env name private? value)
  ;;(pretty-print `(etng-define! ,(etng-scope->list env) ,name ,private? ,(etng->printable value)))
  (hash-table-set! (etng-scope-bindings env)
		   name
		   (cons private? value)))

(define (etng-perform-definition! env private? form)
  (match form
    (`((varref ,identifier))
     (etng-define! env identifier private? (make-etng-symbol (if private?
								 (etng-private-namespace)
								 (etng-public-namespace))
							     identifier)))
    (`((varref ,identifier) ,ast)
     (etng-define! env identifier private? ((eval-etng env) ast)))
    (`((varref ,identifier) . ,asts)
     (etng-define! env identifier private? ((eval-etng env) `(apply ,@asts))))))

(define (etng-object->macro o)
  (make-etng-macro o))

(define (make-recursive-object name o)
  (make-etng-object (etng-object-env o)
		    name
		    (etng-object-clauses o)))

(define (fresh-private-namespace)
  (gensym 'private-namespace))

(define etng-proxies (make-hash-table))
(define etng-public-namespace (make-parameter "http://eighty-twenty.org/etng/r4/ns/builtin#"))
(define etng-private-namespace (make-parameter (fresh-private-namespace)))
(define etng-export-base (make-parameter #f))

(define (etng-export env)
  (let ((table (make-hash-table)))
    (let loop ((env env))
      (if (eq? env (etng-export-base))
	  table
	  (begin
	    (hash-table-walk (etng-scope-bindings env)
			     (lambda (key value)
			       (when (not (car value))
				 hash-table-set! table key (cdr value))))
	    (loop (etng-scope-parent env)))))))

(define (etng-install-proxy! name proxy)
  (hash-table-set! etng-proxies (string->symbol name) proxy))

(define (etng-send-via-named-proxy receiver proxyname message)
  (etng-send* receiver (hash-table-ref etng-proxies proxyname) message))

(define (etng-symbol=? a b)
  (or (eq? a b)
      (and (equal? (etng-symbol-namespace a)
		   (etng-symbol-namespace b))
	   (eq? (etng-symbol-name a)
		(etng-symbol-name b)))))

(define (etng-match ns p v)
  ;;(pretty-print `(etng-match ,ns ,p ,v))
  (cond
   ((pair? p) (and (pair? v)
		   (etng-match ns (car p) (car v))
		   (etng-match ns (cdr p) (cdr v))))
   ((etng-symbol? p) (and (etng-symbol? v)
			  (etng-symbol=? p v)))
   ((etng-object? p)
    ;; Use matcher-combinators instead of this setup.
    ;; Will permit bindings with subpatterns, and, or etc etc.
    (error "Not yet implemented: custom matching" p v))
   ((etng-binding? p)
    ;; private, because otherwise e.g. platform is public
    (etng-define! ns (etng-binding-identifier p) #t v)
    #t)
   (else (equal? p v))))

(define (etng-lookup o message)
  (if (etng-merge? o)
      (or (etng-lookup (etng-merge-derived o) message)
	  (etng-lookup (etng-merge-base o) message))
      (let loop ((clauses (etng-object-clauses o)))
	(and (pair? clauses)
	     (let* ((clause (car clauses))
		    (pattern (car clause))
		    (body-block (cadr clause))
		    (self-name (etng-object-self-name o))
		    (ns (make-etng-scope (etng-object-env o))))
	       (if (etng-match ns pattern message)
		   (lambda (receiver)
		     (when self-name
		       (etng-define! ns self-name #t receiver))
		     ((eval-etng ns) body-block))
		   (loop (cdr clauses))))))))

(define (etng-send* receiver via message)
  ;;(pretty-print `(etng-send* ,receiver ,via ,message))
  (cond
   ((or (etng-object? via) (etng-merge? via))
    (let ((thunk (or (etng-lookup via message)
		     (error "Does not understand" receiver via message))))
      (thunk receiver)))
   ((etng-symbol? via) (etng-send-via-named-proxy receiver 'symbol message))
   ((symbol? via) (etng-send-via-named-proxy receiver 'identifier message))
   ((number? via) (etng-send-via-named-proxy receiver 'number message))
   ((string? via) (etng-send-via-named-proxy receiver 'string message))
   ((pair? via) (etng-send-via-named-proxy receiver 'pair message))
   ((null? via) (etng-send-via-named-proxy receiver 'null message))
   ((vector? via) (etng-send-via-named-proxy receiver 'tuple message))
   ((not via) (etng-send-via-named-proxy receiver 'false message))
   ((eq? via #t) (etng-send-via-named-proxy receiver 'true message))
   ((hash-table? via) (etng-send-via-named-proxy 'hash-table message))
   (else (error "Illegal primitive object" receiver via message))))

(define (etng-send receiver argseq)
  ;;(pretty-print `(etng-send ,receiver ,argseq))
  (etng-send* receiver receiver argseq))

(define (eval-etng env)
  (define (search-env env id)
    (let loop ((env env))
      (cond
       ((not env) (error "Unbound ETNG variable" id))
       ((hash-table-exists? (etng-scope-bindings env) id)
	(cdr (hash-table-ref (etng-scope-bindings env) id)))
       (else (loop (etng-scope-parent env))))))

  (define (do-n f xs)
    (cond
     ((null? xs) #f)
     ((null? (cdr xs)) (f (car xs)))
     (else (f (car xs))
	   (do-n f (cdr xs)))))

  (define (e ast)
    (case (car ast)
      ((block) (do-n e (cdr ast)))
      ;; If we want (: tmp 123; tmp) to be a scope of its own:
      ;; ((block) (do-n (eval-etng (make-etng-scope env)) (cdr ast)))
      ((begin) (do-n e (cdr ast)))
      ((apply) (let ((rator (e (cadr ast))))
		 (if (etng-macro? rator)
		     (etng-send (etng-macro-handler rator) (list env (cddr ast)))
		     (let ((rands (map e (cddr ast))))
		       (etng-send rator rands)))))
      ((appseq) (let ((rator (e (cadr ast)))
		      (rands (map e (drop-right (cddr ast) 1)))
		      (seqrand (e (last ast))))
		  (etng-send rator (append rands seqrand))))
      ((function) (make-etng-object
		   env
		   #f
		   (map (lambda (clause)
			  (list (e (car clause)) (cadr clause)))
			(cadr ast))))
      ((list) (map e (cdr ast))) ;; ugh. Should expand to (list e1 e2 ...) in source language
      ((binding) (make-etng-binding (cadr ast)))
      ((varref)
       ;;(pretty-print `(varref ,(cadr ast) ,(etng-scope->list env)))
       (search-env env (cadr ast)))
      ((lit) (cadr ast))
      ((%assemble) (let ((formals (cons 'the-environment (map car (cadr ast))))
			 (actuals (cons `',env (map (lambda (id)
						      `',(search-env env id))
						    (map cadr (cadr ast)))))
			 (expr (caddr ast)))
		     (let ((form `((lambda ,formals ,expr) ,@actuals)))
		       ;;(pretty-print `(%assemble ,form))
		       (eval form (current-namespace)))))
      (else (error "Invalid ETNG AST"))))

  e)
