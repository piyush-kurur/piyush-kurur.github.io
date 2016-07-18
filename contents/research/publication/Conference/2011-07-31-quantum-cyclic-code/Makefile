SUBDIRS=

.PHONY		: dvi pdf

all	: dvi pdf subdirs

subdirs :
	$(foreach dir,${SUBDIRS}, make -C ${dir};)


dvi		: paper.dvi

pdf		: paper.pdf

paper.dvi	: paper.tex paper.bbl
	latex paper.tex
	latex paper.tex

paper.pdf	: paper.tex paper.bbl $(addprefix density., even odd total)
	pdflatex paper.tex
	pdflatex paper.tex

paper.bbl	: paper.tex bibdata.bib
	latex paper.tex
	bibtex paper

clean	:
	rm -f $(addprefix paper., pdf dvi log aux blg bbl out)
	rm -f $(addprefix density., even odd total)
	rm -f density
density	: 
	make -C code/src-ccode/ density
	cp code/src-ccode/density .
density.even : density
	awk '{print $1, $2}' < density | awk -f process.awk > density.even
density.odd : density
	awk '{print $1, $3}' < density | awk -f process.awk > density.odd
density.total : density
	awk '{print $1, $4}' < density | awk -f process.awk > density.total
