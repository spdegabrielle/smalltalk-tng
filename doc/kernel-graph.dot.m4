digraph SlateKernel {
	_traits(Root); _simple(Root);

	_Node(tr_Traits, label="tr. Traits", shape=box);
	_delegate(tr_Traits, tr_Root, root);

	_traits(Oddball); _simple(Oddball);
	_delegate(tr_Oddball, tr_Root, root);

	_traits(Character);
	_delegate(tr_Character, tr_Oddball, oddball);

	_traits(Symbol);
	_delegate(tr_Symbol, tr_Oddball, oddball);

	_oddball(nil);
	_oddball(NoRole);

	_traits(Boolean);
	_delegate(tr_Boolean, tr_Oddball, oddball);
	_simple2(True, Boolean);
	_simple2(False, Boolean);

	_traits(Derivable); _simple(Derivable);
	_delegate(tr_Derivable, tr_Root, root);

	_traits(Cloneable); _simple(Cloneable);
	_delegate(tr_Cloneable, tr_Derivable, derivable);

	_traits(Number); _simple(Number);
	_delegate(tr_Number, tr_Derivable, derivable);

	_traits(Integer);
	_delegate(tr_Integer, tr_Number, number);

	_traits(Float);
	_delegate(tr_Float, tr_Number, number);

	_traits(Array);
	_delegate(tr_Array, tr_Cloneable, collection);

	_traits(ByteArray);
	_delegate(tr_ByteArray, tr_Cloneable, collection);

	_traits(WordArray);
	_delegate(tr_WordArray, tr_Cloneable, collection);

	_traits(String);
	_delegate(tr_String, tr_Cloneable, collection);

	_traits(Method); _simple(Method);
	_delegate(tr_Method, tr_Cloneable, cloneable);
	_slot(pr_Method, [code], code);
	_slot(pr_Method, [arguments], arguments);
	_slot(pr_Method, [accessor], accessor);
	_slot(pr_Method, [selector], selector);
}