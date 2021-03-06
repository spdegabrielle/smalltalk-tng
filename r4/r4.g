-- -*- text -*-

filetop = block:b spaces ~_ ->b;
exprtop = blockline | spaces ~_ ->`eof;

position = spaces &_ @;

block = position:p (position:p1 ?(same-column? p1 p) blockline)+:es ( token(";") | )
        ->`(block ,@es);

blockline =
  position:p
  combination:s
    ( token(";") position:kp ?(same-line? p kp) combination )*:ss ( token(";") | )
  ->(if (null? ss) s `(begin ,s ,@ss)) ;

combination =
  position:topleft expr:r samelineexps:args1
  ( ( appseq:seqexp ->`(appseq ,r ,@args1 ,seqexp)
    |               ->(make-apply-node r args1) ):app
    commachain(app)
  | position:argpos ?(or (below-and-rightward? argpos topleft) (same-line? argpos topleft))
    ( position:p ?(same-column? p argpos) blockline)*:args2
    ( position:p ?(same-column? p argpos) appseq:seqexp ->`(appseq ,r ,@args1 ,@args2 ,seqexp)
    |                                                  	->(make-apply-node r (append args1 args2)) )
  | ->(make-apply-node r args1) ) ;

samelineexps = @:linestart ( position:p ?(same-line? p linestart) expr)* ;

commachain :r =
  token(",") spaces samelineexps:args
  ( appseq:seqexp       ->`(appseq ,r ,@args ,seqexp)
  | ?(not (null? args)) ->(make-apply-node r args) ):app
  ( commachain(app)
  | ->app );

appseq = token("||") combination ;

expr = position:p
       expr1:lhs
       ( funbody:body
       	 ( position:nextclause
	   ?(or (same-column? nextclause p) (same-line? nextclause p))
	   expr:restexp ?(eq? (car restexp) 'function)
	   ->`(function ((,lhs ,body) ,@(cadr restexp)))
	 | ->`(function ((,lhs ,body))) )
       | ->lhs );

funbody = token("->") block;

expr1 = assemble
      | token("[") expr*:es token("]") ->`(list ,@es)
      | token("(") block:b token(")") -> b
      | token(":") name-no-spaces:b ->`(binding ,b)
      | name:n ->`(varref ,n)
      | (onumber | ostring(#\")):n ->`(lit ,n)
      ;

assemble = token("%assemble") scheme-term:bs token("scheme") token("->") scheme-term:e
	   -> `(|%assemble| ,bs ,e); 

onumber = spaces (digit)+:ip
	  ( $. (digit)+:fp -> (string->number (string-append (list->string ip)
							     "." (list->string fp)))
	  |                -> (string->number (list->string ip)));

ostring ochar:quo =
	spaces
	ochar:s1 -> (or (eqv? s1 quo) (error 'expected 'string-open-quote))
	( ($\ ($\ | ochar:c -> (if (eqv? c quo) c (error 'expected 'escaped-quote))))
	| ochar:c -> (if (eqv? c quo) (error 'expected 'string-char) c))*:cs
	ochar:s2 -> (if (eqv? s2 quo)
		    	(list->string cs)
			(error 'expected 'string-close-quote));

ochar = :c -> (if (char? c) c (error 'expected 'char?));

okeyword :xs :val = spaces <{xs}> ~name-subsequent('(#\- #\? #\! #\* #\+ #\/ #\= #\: #\')) -> val;

token :xs = spaces <{xs}>;

name = spaces name-no-spaces;
name-no-spaces = ($| $| ->(error 'illegal-identifier) | ~($| $|) scheme-symbol-no-spaces);

generic-name :initial-chars :subsequent-chars =
	name-initial(initial-chars):x name-subsequent(subsequent-chars)*:xs
	-> (string->symbol (list->string (cons x xs)));

name-initial :initial-chars =
	ochar:c -> (if (or (char-alphabetic? c)
		       	   (memv c initial-chars))
		       c
		       (error 'expected 'name-initial));

name-subsequent :subsequent-chars =
	ochar:x -> (if (or (char-alphabetic? x)
		       	   (char-numeric? x)
			   (memv x subsequent-chars))
		       x
		       (error 'expected 'name-subsequent));

digit = ochar:c -> (if (char-numeric? c) c (error 'expected 'char-numeric?));

spaces = ((ochar:c ?(char-whitespace? c) ->c) | ->(error 'expected 'char-whitespace?))+ spaces;
spaces = $- $- (:x -> (if (memv x '(#\return #\newline)) (error 'expected 'non-eol) x))* spaces;
spaces = -> #t;

scheme-term = scheme-atom;
scheme-term = scheme-quoted("'", 'quote);
scheme-term = scheme-quoted("`", 'quasiquote);
scheme-term = scheme-quoted(",", 'unquote);
scheme-term = scheme-quoted(",@", 'unquote-splicing);
scheme-term = token("(") scheme-sequence:xs token(")") -> xs;

scheme-quoted :quo :sym = spaces <{quo}> scheme-term:x -> `(,sym ,x);

scheme-atom = okeyword("#t", #t);
scheme-atom = okeyword("#f", #f);
scheme-atom = onumber;
scheme-atom = ostring(#\");
scheme-atom = token("#\\") ( token("return") ->#\return
	      		   | token("newline") ->#\newline
			   | ochar);
scheme-atom = scheme-symbol;
scheme-atom = ostring(#\|):s -> (string->symbol s);

scheme-symbol = spaces scheme-symbol-no-spaces;
scheme-symbol-no-spaces =
  generic-name('(#\- #\+ #\= #\_ #\| #\/ #\? #\< #\> #\* #\& #\^ #\% #\$ #\@ #\! #\~ #\: #\.),
               '(#\- #\+ #\= #\_ #\| #\/ #\? #\< #\> #\* #\& #\^ #\% #\$ #\@ #\! #\~ #\: #\. #\'));

scheme-sequence = scheme-term:a token(".") scheme-term:d -> (cons a d);
scheme-sequence = scheme-term:a scheme-sequence:d -> (cons a d);
scheme-sequence = -> '();
