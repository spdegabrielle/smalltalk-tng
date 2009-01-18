-- -*- text -*-

toplevel =
	  &_:v {#define-namespace} -> v -- no code in here
	| &_:v {#declare-default-namespace} -> v -- neither
	| {#define-value :qname expr:exp ~_} -> `(define-value ,qname ,exp)
	| {#define-function :qname method:meth ~_} -> `(define-function ,qname ,meth)
	| expr
;

expr =
	  {#ref :name ~_} -> `(ref ,name)
	| {#lit :literal ~_} -> `(lit ,literal)
	| {#object method*:methods ~_} -> `(object ,@methods)
	| {#function method*:methods ~_} -> `(function ,@methods)
	| {#tuple expr*:elts ~_} -> `(tuple ,@elts)
	| {#send expr:receiver expr:message ~_} -> `(send ,receiver ,message)
	| {#assemble {assemble-binding*}:bindings {assemble-clause*}:clauses}
	  -> `(assemble ,bindings ,clauses)
;

assemble-binding = {:name expr:init} -> `(,name ,init);
assemble-clause = {:language :item} -> `(,language ,item);

method =
	  {#method {pattern*}:patterns expr:body ~_} -> `(method ,patterns ,body)
;

pattern =
	  {#discard} -> `(discard)
	| {#bind :name ~_} -> `(bind ,name)
	| {#lit :literal ~_} -> `(lit ,literal)
	| {#tuple pattern*:elts ~_} -> `(tuple ,@elts)
;
