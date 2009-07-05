-- -*- text -*-

pass = toplevel;

expr =
	  {#object :selfid method*:methods ~_} ->
		(convert-constant-methods `(object ,selfid) methods)
	| {#function method*:methods ~_} ->
		(convert-constant-methods '(function) methods)
;

method =
	  {#constant-method {pattern*}:patterns expr:body ~_} -> `(constant-method ,patterns ,body)
;
