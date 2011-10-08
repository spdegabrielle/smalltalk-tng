%.ps: %.dot
	dot -Tps $< > $@

%.dot: %.dot.m4
	m4 dotmacros.m4 $< > $@

%.ps: %.neato
	neato -Tps $< > $@

%.neato: %.neato.m4
	m4 dotmacros.m4 $< > $@

%.view: %.ps
	gv $<

%.edit: %.ps
	gimp $<

%.pdf: %.ps
	ps2pdf $<
