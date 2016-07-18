SUBDIRS=code

.PHONY	: dvi pdf

all: dvi pdf

dvi: paper.dvi

pdf: paper.pdf

paper.dvi: paper.tex
	latex paper.tex
	latex paper.tex

paper.pdf: paper.tex
	pdflatex paper.tex
	pdflatex paper.tex

clean	:
	rm -f $(addprefix paper., pdf dvi log aux blg bbl out)


