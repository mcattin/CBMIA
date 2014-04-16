DOC=$(wildcard *.doc)
TEX=$(wildcard *.tex)
PDFD=$(DOC:.doc=.pdf)
PDFT=$(TEX:.tex=.pdf)


.PHONY: all
all: $(PDFD) $(PDFT)

%.pdf : %.doc
	unoconv -f pdf $<

%.pdf : %.tex
	pdflatex $<
	pdflatex $<
	evince $@ &

clean:
	rm -f $(PDFD) $(PDFT)
	rm -rf *.aux *.dvi *.log *.toc

show:
	$(info all doc: $(DOC))
	$(info all tex: $(TEX))
	$(info all pdf: $(PDFD) $(PDFT))
