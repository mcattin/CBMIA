SVG=$(wildcard *.svg)
PNG=$(wildcard *.png)
PDF=$(SVG:.svg=.pdf)
EPS=$(SVG:.svg=.eps) $(PNG:.png=.eps)


.PHONY: all
all: $(PDF)

%.pdf : %.svg
	inkscape --without-gui $< -A $@

%.eps : %.svg
	inkscape --without-gui $< -E $@

%.eps : %.png
	inkscape --without-gui $< -E $@

clean:
	rm -f $(PDF)
	rm -f $(EPS)
#	rm -f EDA-01444-V1_fp.pdf
#	rm -f cvora_fp.pdf

show:
	$(info all svg: $(SVG))
	$(info all pdf: $(PDF))
	$(info all eps: $(EPS))

#EDA-01444-V1_fp.pdf:
#	wget https://edms.cern.ch/file/782366/1/EDA-01444-V1_fp.pdf

#cvora_fp.pdf: EDA-01444-V1_fp.pdf
	# extract first page
#	pdftk A=EDA-01444-V1_fp.pdf cat A1 output cvora_fp.pdf
	# crop around front panel
	#   bbox origin is in bottom left corner of the page
	#   bbox unit is [points]
#	pdfcrop --bbox '400 290 500 970' cvora_fp.pdf cvora_fp.pdf
