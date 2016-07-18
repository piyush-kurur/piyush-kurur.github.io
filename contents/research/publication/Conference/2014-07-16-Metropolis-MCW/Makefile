BIBTEXDB   = references.bib
GENERATED  = dvi pdf log blg bbl aux toc ind idx ilg out

.DEFAULT_GOAL=dvi

.PHONY : dvi pdf all clean


dvi	: main.dvi
pdf	: main.pdf
all	: dvi pdf

main.dvi: main.bbl
	latex main.tex
	latex main.tex

main.pdf: main.bbl
	pdflatex main.tex
	pdflatex main.tex

main.aux: main.tex
	latex main.tex

main.bbl: main.aux references.bib
	bibtex main

clean	:
	rm -f $(addprefix main.,${GENERATED})

archive:
	git archive --prefix='min-codeword/' \
		--format=tar.gz HEAD -o min-codeword.tar.gz
