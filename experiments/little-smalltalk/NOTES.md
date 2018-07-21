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

2018-07-15 12:30:53 Reading some of "the classics" [1,2,3] to refresh
my memory. The first paper [1] presents the techniques of
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

It also mentions other techniques:

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

The authors briefly describe their "adaptive recompilation" technique
of the time. Two compilers are used; the non-optimizing compiler adds
code to increment a per-method (presumably, a *customized* method?)
counter on each entry to the unoptimized routine. Later checking of
the counters identifies hot spots.

QUESTIONS: How does it figure out which receiver-type to optimize for?
How many counters per implementation are there - one per
receivertype/implementation pair, or just one for the implementation
itself?

On to the second paper [2], on *type feedback*, the use of information
about concrete implementation types gathered at runtime to improve the
outcome of recompilation. At the end of the introduction, the paper
claims that the new system runs 1.5 times faster than the old system,
despite the simplicity of the new system, because of the power of type
feedback.

 - The built-in PICs are used to gather type profile information at
   each call site. A list of receiver types, with optional invocation
   counts, for each send site is all that is required for the type
   feedback technique.

 - A fairly simple mechanism (p.3) is used to find "hot" methods.
   Then, to decide what to recompile (not simply the hot method!), the
   system walks the call chain. A caller is recompiled if it "performs
   many calls to unoptimized or small methods", or if it creates
   closures, since these often encode control.

 - It recompiles multiple activation records at once, in general: it
   is able to inline already-active routines into a recompiled
   "great-grand-caller". The paper remarks that this is the inverse of
   "dynamic deoptimization" [3].

 - The results of recompilation are checked, and if the improvement
   (in terms of non-inlined sends) is zero, a recompiled method is
   marked so that future optimization attempts don't waste work
   re-recompiling it.

 - "Small" methods are inlined, so long as the overall method isn't
   growing "too large". Estimates of the cost of inlining depend not
   only on the size of the source text to be inlined *inter alia*, but
   on previously-compiled object code for the to-be-inlined method.

Ultimately, customization and type feedback seem very similar.
Customization allows static knowledge of the receiver type for
self-sends. Type feedback allows (probabilistic) static knowledge of
the receiver type for other sends.

Moving on to the third paper [3]. ((TBD))

[1] Ungar, David, Randall B. Smith, Craig Chambers, and Urs Hölzle.
“Object, Message, and Performance: How They Coexist in Self.” Computer
25, no. 10 (1992): 53–64. https://doi.org/10.1109/2.161280.

[2] Hölzle, Urs, and David Ungar. “Optimizing Dynamically-Dispatched
Calls with Run-Time Type Feedback.” In Proceedings of the ACM SIGPLAN
1994 Conference on Programming Language Design and Implementation,
p:326–336. ACM New York, NY, USA, 1994.
http://portal.acm.org/citation.cfm?id=178478.

[3] Hölzle, Urs, Craig Chambers, and David Ungar. ‘Debugging Optimized
Code with Dynamic Deoptimization’. In Proceedings of the ACM SIGPLAN
1992 Conference on Programming Language Design and Implementation. San
Francisco, California, 1992. https://doi.org/10.1145/143095.143114.

2018-07-18 18:15:23 On leap:
 - from startup: 19115890 bytecodes/sec; 2378566 sends/sec
 - in a workspace: 15267175 bytecodes/sec; 2258839 sends/sec

... but later measurement shows it running a bit faster (see table
below), so basically benchmarking on this laptop is difficult and
futile and I shouldn't be doing it.

Command:
    racket jit-SmallWorld-2015.rkt t.st
and t.st contains:
    EVAL [ 0 tinyBenchmarks. Transcript show: 0 tinyBenchmarks ] value

At rev 377:8accd6d3f51d (startup):  6884681 bytecodes/sec;  856333 sends/sec
At rev 378:2a35e7fcba59 (startup):  7168458 bytecodes/sec;  896649 sends/sec
At rev 379:e5e063ac93ef (startup): 20618556 bytecodes/sec; 2569214 sends/sec
At rev 392:618244a1ee07 (startup): 19358741 bytecodes/sec; 2825302 sends/sec
At rev 394:97ec29b53c47 (startup): 18648018 bytecodes/sec; 2573311 sends/sec
At rev 395:3979401d44c1 (startup): 17887087 bytecodes/sec; 2562715 sends/sec
At rev 396:3bfb9afdbd9d (startup): 18823529 bytecodes/sec; 2684483 sends/sec

2018-07-19 19:17:27 On leap, running the Java SmallWorld
`0 tinyBenchmarks`, I get 2388059 bytecodes/sec; 299492 sends/sec.
