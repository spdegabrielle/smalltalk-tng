2018-07-13 22:30:40 tonyg on hop: 0 tinyBenchmarks 411861 bytecodes/sec; 65707 sends/sec

Added simplest possible method lookup cache; the result:
2018-07-13 22:43:12 tonyg on hop: 0 tinyBenchmarks 859845 bytecodes/sec; 106388 sends/sec

For comparison, on the same machine (hop), SqueakJS running Mini Squeak 2.2
in Firefox Quantum 61.0.1 gets 38787878 bytecodes/sec; 698205 sends/sec

2018-07-14 16:52:47 tonyg on leap: 0 tinyBenchmarks 2228412 bytecodes/sec; 270007 sends/sec

Split out perform-primitive into separate routine
2018-07-14 17:05:40 tonyg on leap: 0 tinyBenchmarks 2386634 bytecodes/sec; 287921 sends/sec

First measurable instance of JIT
2018-07-14 23:29:08 tonyg on leap: 0 tinyBenchmarks, five runs:
    5131494 bytecodes/sec; 603554 sends/sec
    5009392 bytecodes/sec; 706843 sends/sec
    5161290 bytecodes/sec; 646828 sends/sec
    5151320 bytecodes/sec; 641582 sends/sec
    5028284 bytecodes/sec; 705873 sends/sec

Remove spurious indirection ("resume-jit-context"):
2018-07-14 23:48:56 tonyg on leap: 0 tinyBenchmarks 7944389 bytecodes/sec; 929888 sends/sec

Remove spurious arglist construction/destruction, introduce MICs:
2018-07-15 00:43:31 tonyg on leap: 0 tinyBenchmarks
    19277108 bytecodes/sec; 2778454 sends/sec
    19417475 bytecodes/sec; 2738288 sends/sec

Inline primitive definitions:
2018-07-15 01:22:29 tonyg on leap: 0 tinyBenchmarks
at startup:
    24060150 bytecodes/sec; 3184661 sends/sec
    23970037 bytecodes/sec; 3243944 sends/sec
in a workspace:
    19765287 bytecodes/sec; 3482507 sends/sec
    19476567 bytecodes/sec; 3422322 sends/sec

2018-07-15 11:12:26 tonyg on hop: 0 tinyBenchmarks 7816316 bytecodes/sec; 1313400 sends/sec

2018-07-15 12:30:53 Reading some of "the classics" [1,2] to refresh my
memory. The first of the two [1] presents the techniques of
*customization*, *inlining*, and *splitting*:

 - *Customization* means compiling a specific version of each method
   for each distinct concrete receiver class. In
   `jit-SmallWorld-2015.rkt`, this would correspond to including a
   `class` argument to `compile-native-proc`.

   Their example of `#min:` is instructive. Consider `#min:` inherited
   by, say, `SmallInt` and `String`:

       min: x
         ^ (self < x) ifTrue: [self] ifFalse: [x]

   By using customization, the system can directly link to (at least)
   or inline (better) the `SmallInt`- and `String`-specific
   implementations of `#<`.

 - *Inlining* comprises both inlining of a method implementation into
   a call site, where the receiver class of the inlined method is
   statically known, and open-coding of primitives at call sites. In
   `jit-SmallWorld-2015.rkt`, primitives are open-coded, but other
   methods are not yet inlined, because no type information is
   propagated inside the compiler.

 - *Splitting*—properly *message splitting*—is the hoisting of pieces
   of a continuation into alternative branches leading up to that
   continuation. It corresponds to a kind of
   CPS-transformation-induced binding-time improvement: it's the kind
   of thing that happens if you do the CPS transformation, and then
   specialize it ("customize" it!) based on type information gleaned
   from each branch.

   In the `#min:` example, after inlining and open-coding the
   primitive `#<` for `SmallInt` receivers, we have two branches: one
   where the comparison yielded the `true` object, and one where it
   yielded `false`. Without splitting, the subsequent
   `#ifTrue:ifFalse:` can't be inlined effectively (though [IDEA] a
   partial evaluator able to represent *alternatives*—*unions*—might
   be able to do well! The result would be (U True False) and a simple
   cascade of tests could recover quite a bit of efficiency). By
   splitting, hoisting the `#ifTrue:ifFalse:` send into the branches
   producing the `true` and `false` values, we get to inline the
   constant blocks that are its arguments.

They also mention [1] other techniques:

 - "type prediction" based on hand-coded heuristics about common uses
   of certain selectors. Our `jit-SmallWorld-2015.rkt` (and the pure
   interpreter it descends from, and the SmallWorld implementations of
   Budd and Allen that *that* descends from) all benefit from this:
   the `SendUnary` and `SendBinary` opcodes (10 and 11) include
   special checks for `SmallInt` instances. Reading the introduction
   to [2] suggests that this kind of type prediction will be obviated
   by dynamic type feedback. We shall see.

 - "extended splitting" is essentially even more like CPS-based
   binding-time improvement: whole chains of operations, larger
   segments of continuations, are hoisted above merge nodes in the
   control flow graph.

 - with some small additional effort (?), the optimizations combine to
   hoist some loop invariants from a loop body into its header,
   apparently.

They describe their "adaptive recompilation" technique of the time.
Two compilers are used; the non-optimizing compiler adds code to
increment a per-method (presumably, a *customized* method?) counter on
each entry to the unoptimized routine. Later checking of the counters
identifies hot spots.

QUESTIONS: How does it figure out which receiver-type to optimize for?
How many counters per implementation are there - one per
receivertype/implementation pair, or just one for the implementation
itself?

On to the second paper [2]. At the end of the introduction, they claim
that their new system runs 1.5 times faster than their old system,
despite the simplicity of the new system, because of the power of type
feedback.

 - The built-in PICs

[1] Ungar, David, Randall B. Smith, Craig Chambers, and Urs Hölzle.
“Object, Message, and Performance: How They Coexist in Self.” Computer
25, no. 10 (1992): 53–64. https://doi.org/10.1109/2.161280.

[2] Hölzle, Urs, and David Ungar. “Optimizing Dynamically-Dispatched
Calls with Run-Time Type Feedback.” In Proceedings of the ACM SIGPLAN
1994 Conference on Programming Language Design and Implementation,
p:326–336. ACM New York, NY, USA, 1994.
http://portal.acm.org/citation.cfm?id=178478.
