dnl---------------------------------------------------------------------------
dnl Basic macros
dnl---------------------------------------------------------------------------
dnl
define(`_Node', `"$1"ifelse($#,1,,` [shift($@)]')')dnl
define(`_Link', `"$1" -> "$2"ifelse($#,2,,` [shift(shift($@))]')')dnl
dnl
define(`_Label', `$1, label="$2"')
dnl
define(`_Subgraph', `subgraph cluster_$1 { shift($@) }')dnl
dnl
dnl---------------------------------------------------------------------------
dnl Styles
dnl---------------------------------------------------------------------------
dnl
define(`_Process', `_Node($*, shape=box)')dnl
define(`_Datastore', `_Node($*, shape=ellipse)')dnl
define(`_External', `_Node($*, shape=box, style=filled)')dnl
dnl
define(`_BoldLink', `_Link($*, style=bold)')dnl
define(`_DotLink', `_Link($*, style=dotted)')dnl
define(`_DashLink', `_Link($*, style=dashed)')dnl
dnl
dnl---------------------------------------------------------------------------
dnl Prototypes and Traits
dnl---------------------------------------------------------------------------
dnl
define(`_proto', `_Node(pr_$*, label="$1", shape=ellipse)')dnl
define(`_traits', `_Node(tr_$*, label="tr. $1", shape=box, style=filled)')dnl
define(`_delegate', `_Link($1, $2, label="$3", style=bold)')dnl
define(`_slot', `_Link($1, $2, label="$3")')dnl
define(`_simple', `_proto($1); _delegate(pr_$1, tr_$1, traits)')dnl
define(`_simple2', `_proto($1); _delegate(pr_$1, tr_$2, traits)')dnl
define(`_oddball', `_simple2($1, Oddball)')dnl
