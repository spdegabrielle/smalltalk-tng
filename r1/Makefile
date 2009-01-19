cleaner1=tr '\n' ' ' | tr -s ' ' | sed -e 's/ $$//' -e 's/ / '
cleaner2=' /g'

# sdlCFLAGS=-C $(shell sdl-config --cflags | $(cleaner1)-C$(cleaner2))
# sdlLDADD=-L $(shell sdl-config --libs | $(cleaner1)-L$(cleaner2)) \
# 	-L -lSDL_ttf \
# 	-L -lSDL_image \
# 	-L -lSDL_gfx

CSC=csc -syntax -O3 -lambda-lift -no-trace -keyword-style none -prologue macros.scm
CSCCC=$(CSC) $(sdlCFLAGS)
CSCLD=$(CSC) $(sdlCFLAGS) $(sdlLDADD)

TARGETS = \
	util.so \
	oo.so \
	kernel.so \
	packrat.so \
	parsetng.so \
	interp.so \
	image.so

all: $(TARGETS)

%.so: %.scm
	$(CSCLD) -s -o $@ $<
#	strip $@

clean:
	rm -f $(TARGETS)
	rm -f STACKTRACE

%: %.scm
	$(CSCLD) -o $@ $<

kernel.scm: root-hooks.scm kernel-methods.scm
ui.scm: sdl-events.scm
