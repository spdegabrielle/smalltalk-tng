PDFNAME=ThiNG_talk
TARGETS=$(PDFNAME).pdf

TEXINPUTS:=$(TEXINPUTS):./prosper:./prosper/img

LATEX=TEXINPUTS=$(TEXINPUTS) latex
DVIPS=TEXINPUTS=$(TEXINPUTS) dvips

%.eps: %.dot
	dot -Tps -o $@ $<

%.view: %.eps
	gv $<

%.view: %.ps
	gv $<

%.pdfview: %.pdf
	acroread $<

%.xpdfview: %.pdf
	xpdf $<

%.ps: %.dvi
	$(DVIPS) -o $@ $<

%.pdf: %.ps
	ps2pdf13 $<

%.dvi: %.tex
	-$(LATEX) "\\batchmode\\input{$<}"
	-$(LATEX) "\\batchmode\\input{$<}"
	$(LATEX) $<
#	Three is a magic number.

%.force: %.tex
	-$(LATEX) "\\batchmode\\input{$<}"
	-$(LATEX) "\\batchmode\\input{$<}"
	-$(LATEX) "\\batchmode\\input{$<}"
	$(DVIPS) -o $*.ps $*.dvi
	gv $*.ps

all: $(TARGETS)

clean:
	rm -f $(PDFNAME).{aux,bbl,blg,idx,log,toc,dvi,ps,pdf,out}

.PRECIOUS: %.pdf %.ps %.dvi
