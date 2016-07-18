.PHONY:	div pdf dist clean

.DEFAULT_GOAL :=  dvi
all	: dvi pdf

dvi	:	intMult.dvi

pdf	: 	intMult.pdf

intMult.dvi	: intMult.tex intMult.bbl preamble.tex
	latex intMult.tex
	latex intMult.tex

intMult.pdf	: intMult.tex intMult.bbl preamble.tex
	pdflatex intMult.tex
	pdflatex intMult.tex


dist	: intMult.tex preamble.tex references.bib Makefile
	darcs dist --dist-name intMult`date +%Y%b%d`

clean	:
	rm -f $(addprefix intMult., dvi aux log blg bbl pdf out)
	rm -f intMult.tar.gz


intMult.bbl : intMult.tex references.bib
	latex intMult.tex
	bibtex intMult

arxiv.tar : intMult.bbl intMult.tex preamble.tex
	tar -c intMult.bbl intMult.tex preamble.tex -f arxiv.tar

