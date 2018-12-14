SUBDIRS=code

.PHONY	: dvi pdf

all: dvi pdf

dvi: paper.dvi

pdf: paper.pdf

paper.dvi: paper.tex bib.bib
	latexmk  paper.tex

paper.pdf: paper.tex bib.bib
	latexmk -pdf paper.tex

clean	:
	rm -f $(addprefix paper., bbl pdf dvi log aux blg out)


