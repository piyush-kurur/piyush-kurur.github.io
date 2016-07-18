.PHONY: all clean figs pdf

all: pdf

pdf: NonStab.pdf

NonStab.dvi: figs NonStab.tex NonStab.bib
	latexmk NonStab.tex

NonStab.pdf: NonStab.dvi
	dvipdf NonStab.dvi

clean: figs-clean
	rm -f $(addprefix NonStab., pdf ps dvi aux log blg bbl fls fdb_latexmk)

figs:
	make -C figs all

figs-clean:
	make -C figs clean
NonStab.tar	: NonStab.tex NonStab.bib *.pstex*
	tar -c NonStab.tex NonStab.bib *.pstex*  -f NonStab.tar
