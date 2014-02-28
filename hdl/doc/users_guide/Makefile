FILE=cbmia_users_guide

all:
	pdflatex $(FILE).tex
	pdflatex $(FILE).tex
	evince $(FILE).pdf

clean:
	rm -rf *.aux *.dvi *.log $(FILE).pdf