-- -*- text -*-

pass = toplevel;

expr =
	  {#object method*:methods ~_} -> (convert-constant-methods 'object methods)
	| {#function method*:methods ~_} -> (convert-constant-methods 'function methods)
;

method =
	  {#constant-method {pattern*}:patterns expr:body ~_} -> `(constant-method ,patterns ,body)
;
