LATEX_OPTIONS=-src-specials
PDFLATEX_OPTIONS=

BIBTEXDB   = references.bib
GENERATED  = dvi pdf log blg bbl aux toc ind idx ilg out


LATEX=latex ${LATEX_OPTIONS}
PDFLATEX= pdflatex ${PDFLATEX_OPTIONS}

.DEFAULT_GOAL=dvi

.PHONY : dvi pdf all clean


dvi	: main.dvi
pdf	: main.pdf
all	: dvi pdf

main.dvi: main.bbl
	${LATEX} main.tex
	${LATEX} main.tex

main.pdf: main.bbl
	${PDFLATEX} main.tex
	${PDFLATEX} main.tex

main.aux: main.tex
	${LATEX} main.tex

main.bbl: main.aux references.bib
	bibtex main

clean	:
	rm -f $(addprefix main.,${GENERATED}) *.aux