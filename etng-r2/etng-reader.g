-- -*- text -*-

sexp-toplevel = (ws $;)* ( (~toplevel-stop sexp)*:ss toplevel-stop -> ss
	      	    	 | ws ~_ -> 'eof);

toplevel-stop = ws $; ;

sexp =
	  ws (   $. sexp:s -> `(paren ,QUOTE-QNAME ,s)
	       | $` sexp:s -> `(paren ,UNQUOTE-QNAME ,s)
	       | $( sexp*:ss ws $) -> `(paren ,@ss)
	       | $[ sexp*:ss ws $] -> `(brack ,@ss)
	       | ${ sexp*:ss ws $} -> `(brace ,@ss)
	       )
	| leaf
;

leaf = qname | id | word | string;

qname =
	  id:lhs $: id:rhs -> (make-qname lhs rhs)
	| ws     $: id:rhs -> (make-qname EMPTY-SYMBOL rhs)
;

id =
	ws ( id1:i -> (string->symbol (string-concatenate (list-interleave "'" i)))
	   | $; -> (string->symbol ";")
	   | $, -> (string->symbol ",")
	   | id-alpha:a (id-alpha | digit)*:r
	       -> (string->symbol (list->string (cons a r)))
	   | id-punct+:p -> (string->symbol (list->string p))
	   )
;

id1 =
	  id-subunit:i id1:is -> (cons i is)
	| id-subunit:i -> (list i)
;

id-subunit = $' (:c ?(not (eqv? c #\')) ->c)*:cs $' -> (list->string cs);

word = positive-word | ws $- positive-word:w -> (invert-sign w);

positive-word = ws digit+:d -> (string->number (list->string d));

string = ws string1:s -> (string-concatenate (list-interleave "\"" s));

string1 =
	  string-subunit:s string1:ss -> (cons s ss)
	| string-subunit:s -> (list s)
;

string-subunit = $" (:c ?(not (eqv? c #\")) ->c)*:cs $" -> (list->string cs);

id-alpha = :c ?(char-etng-id-alpha? c) -> c;
id-punct = :c ?(char-etng-id-punct? c) -> c;
digit = :c ?(char-numeric? c) -> c;

ws =
	  (:c ?(char-whitespace? c))+ ws
	| $- $- (:c ?(not (eol-char? c)))*
	     	(:c ?(eol-char? c))
		ws
	|
;
