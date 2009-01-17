-- -*- text -*-

toplevel = toplevel-item:v ~_ -> v;

toplevel-item =
	  {#paren #namespace :prefix ?(symbol? prefix) equal :urn ?(string? urn) ~_
	  	  -> `(define-namespace ,prefix ,urn)}
	| {#paren #namespace :urn ?(string? urn) ~_
	  	  -> `(declare-default-namespace ,urn)}
	| {#paren #define :q ?(qname-or-symbol? q) equal expr:exp ~_
		  -> `(define-value ,q ,exp)}
	| {#paren #define :q ?(qname-or-symbol? q) normal-method:def ~_
	  	  -> `(define-function ,q ,def)}
	| parse
;

parse =
	  ~(comma | semi)
	  :n
	  ( -> (or (pair? n) (error 'expected 'grouping)) grouping(n)
	  | ?(qname-or-symbol? n) -> `(ref ,n)
	  | ?(or (string? n) (number? n)) -> `(lit ,n) )
	| -> (error 'comma-and-semi-are-illegal-expressions)
;

grouping =
	  {#paren expr:e ~_ -> e}
	| {#brack methods:ms -> `(object ,@ms)}
	| {#brace methods:ms -> `(function ,@ms)}
;

expr =
	  :head ?(special-segment-head? head) special-segment(head)
	| tuple:elts -> (if (= (length elts) 1) (car elts) `(tuple ,@elts))
;

special-segment =
	  :head ?(equal? head QUOTE-QNAME) :n -> `(lit ,n)
	| :head ?(equal? head UNQUOTE-QNAME) -> (error 'naked-unquote)
	| #do expr:e1 semis expr:e2
	  -> `(send (function (method ((discard)) ,e2)) ,e1)
	| #let pattern:p equal expr:e semis expr:body
	  -> `(send (function (method (,p) ,body)) ,e)
;

tuple =
	  send:s (comma send)*:ss -> (cons s ss)
	| ~_ -> '()
;

send =
	parse:receiver message*:messages
	->  (fold (lambda (msg rcvr) `(send ,rcvr ,msg)) receiver messages)
;

message = ~(arrow | equal) parse;

methods =
	  normal-method:m semis methods:ms -> (cons m ms)
	| constant-method:m semis methods:ms -> (cons m ms)
	| &_ expr:e semis ~_ -> (list `(method ((discard)) ,e))
	| semis ~_ -> '()
;

normal-method =
	(~&(arrow | equal) pattern)+:patterns arrow expr:body
	-> `(method ,patterns ,body)
;

constant-method =
	(~&(arrow | equal) pattern)+:patterns equal expr:body
	-> `(constant-method ,patterns ,body)
;

pattern = pattern-tuple-nonempty:elts -> (if (= (length elts) 1) (car elts) `(tuple ,@elts));

pattern-tuple-nonempty =
	pattern-element:e (comma pattern-element)*:es
	-> (cons e es)
;

pattern-tuple = pattern | -> `(tuple);

pattern-element =
	  ~(#do | #let)
	  :n
	  ( -> (or (pair? n) (error 'expected 'grouping)) pattern-grouping(n)
	  | ?(eq? n DISCARD) -> `(discard)
	  | ?(qname-or-symbol? n) -> `(bind ,n)
	  | ?(or (string? n) (number? n)) -> `(lit ,n)
	  )
;

pattern-grouping =
	  {#paren quote :n -> `(lit ,n)}
	| {#paren pattern-tuple:p ~_ -> p}
	| {#brace -> (error 'object-matching-not-supported)}
	| {#brack -> (error 'function-matching-not-supported)}
;

semis = (:x ?(eq? x SEMI))*;
semi = :x ?(eq? x SEMI) -> x;

quote = :x ?(equal? x QUOTE-QNAME) -> x;
comma = :x ?(eq? x COMMA) -> x;
arrow = :x ?(eq? x ARROW) -> x;
equal = :x ?(eq? x '=) -> x;
