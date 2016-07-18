SUBDIRS=code

.PHONY	: dvi pdf

all: dvi pdf

dvi: paper.dvi

pdf: paper.pdf

paper.dvi: paper.tex paper.bbl
	latex paper.tex
	latex paper.tex

paper.pdf: paper.tex paper.bbl 
	pdflatex paper.tex
	pdflatex paper.tex

paper.bbl: paper.tex reference.bib
	latex paper.tex
	bibtex paper

clean	:
	rm -f $(addprefix paper., pdf dvi log aux blg bbl out)


