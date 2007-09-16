: toHexDigit
	15 AND DUP 9 > IF 55 ELSE 48 THEN + ;
: hexbyteemit
	DUP 16 / toHexDigit EMIT toHexDigit EMIT ;
: hexwordemit
	DUP 16777216 / hexbyteemit
	DUP 65536 / hexbyteemit
	DUP 256 / hexbyteemit
	hexbyteemit ;
: HEXEMIT
	BEGIN DUP 0 <> WHILE 1- SWAP DUP @b hexbyteemit 1+ SWAP REPEAT ;
: emitzNonNull
	BEGIN DUP @b DUP WHILE EMIT 1+ REPEAT DROP DROP ;
: EMITZ
	DUP 0= IF DROP ." (null)" ELSE emitzNonNull THEN ;
: ARGC S0 @ @ ;
: ARGV S0 @ 4+ ;
: ARGN 4 * ARGV + @ ;
: emitSingleArg
	SWAP DUP . SWAP EMITZ CR ;
: EMITARGS
	0 BEGIN DUP ARGN DUP 0 <> WHILE emitSingleArg 1+ REPEAT DROP DROP ;

CR CR

EMITARGS
0 SYSEXIT
