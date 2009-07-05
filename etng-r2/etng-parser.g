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
	  ~(comma | semi | arrow | equal | pipe)
	  :n
	  ( grouping(n)
	  | ?(qname-or-symbol? n) -> `(ref ,n)
	  | ?(or (string? n) (number? n)) -> `(lit ,n) )
	| comma -> (error 'extra 'comma)
	| semi -> (error 'extra 'semi)
	| arrow -> (error 'extra 'arrow)
	| equal -> (error 'extra 'equal)
	| pipe -> (error 'extra 'pipe)
	| -> (error)
;

grouping =
	  {#paren expr:e ~_ -> e}
	| #rec {#brace methods:ms -> `(object self ,@ms)}
	| #rec :selfid {#brace methods:ms -> `(object ,selfid ,@ms)}
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
	| #'%assemble' {#paren assemble-bindings:bindings ~_} {#brace assemble-clauses:clauses ~_}
	  -> `(assemble ,bindings ,clauses)
;

assemble-bindings =
	  assemble-binding:b (comma assemble-binding)*:bs -> (cons b bs)
	| ~_ -> '()
;

assemble-binding = :n ?(qname-or-symbol? n) equal send:e -> (list n e);

assemble-clauses =
	  ({#paren quote :n ?(qname-or-symbol? n)} | -> (error 'expected 'quoted-language-name))
	  arrow :item &(semi | ~_)
	  semis assemble-clauses:more -> (cons (list n item) more)
	| ~_ -> '()
;

tuple =
	  send:s (comma send)*:ss -> (cons s ss)
	| ~_ -> '()
;

send =
	  parse:receiver message*:messages
	    -> (fold (lambda (msg rcvr) (msg rcvr)) receiver messages)
	| message+:messages
	    -> (let ((g (gensym 'pipe)))
	         `(function (method ((bind ,g)) ,(fold (lambda (msg rcvr) (msg rcvr))
		 	    	    	   	       `(ref ,g)
						       messages))))
;

message =
	  parse:p -> (lambda (rcvr) `(send ,rcvr ,p))
	| pipe parse:p -> (lambda (msg) `(send ,p ,msg))
;

send =
	parse:receiver message*:messages
	->  (fold (lambda (msg rcvr) `(send ,rcvr ,msg)) receiver messages)
;

message = parse;

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
	  ( pattern-grouping(n)
	  | ?(eq? n DISCARD) -> `(discard)
	  | ?(qname-or-symbol? n) -> `(bind ,n)
	  | ?(or (string? n) (number? n)) -> `(lit ,n)
	  )
;

pattern-grouping =
	  {#paren quote :n -> `(lit ,n)}
	| {#paren pattern-tuple:p ~_ -> p}
	| {#brace -> (error 'object-matching-not-supported)}
	| {#brack -> (error 'list-matching-not-supported)}
;

semis = (:x ?(eq? x SEMI))*;
semi = :x ?(eq? x SEMI) -> x;

quote = :x ?(equal? x QUOTE-QNAME) -> x;
comma = :x ?(eq? x COMMA) -> x;
arrow = :x ?(eq? x ARROW) -> x;
equal = :x ?(eq? x '=) -> x;
pipe = :x ?(eq? x PIPE) -> x;
