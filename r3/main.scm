"true"; exec csi -no-init -batch -quiet -R syntax-case "$0" "$@"

(require 'srfi-1)
(require 'srfi-13)
(load "json-scheme/portable-packrat.scm")
(load "../lib/pregexp-20050502/pregexp.scm")

(load "parsetng.scm")
(load "printtng.scm")
(load "evaltng.scm")

(repl-ThiNG)
