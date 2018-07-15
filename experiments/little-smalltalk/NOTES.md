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
