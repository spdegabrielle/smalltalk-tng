%.ps: %.dot
	dot -Tps $< > $@

%.dot: %.dot.m4
	m4 dotmacros.m4 $< > $@

%.view: %.ps
	gv $<

%.edit: %.ps
	gimp $<
