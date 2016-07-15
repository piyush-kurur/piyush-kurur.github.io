.PHONY: all dvi pdf clean
all : dvi pdf
	touch thesis.dvi thesis.pdf
dvi :	thesis.dvi
pdf :	thesis.pdf
thesis.pdf: thesis.bbl thesis.ind thesis.gls
	pdflatex thesis.tex
	pdflatex thesis.tex
	touch thesis.bbl thesis.ind thesis.gls


thesis.dvi:  thesis.bbl thesis.ind thesis.gls
	latex thesis.tex
	latex thesis.tex
	touch thesis.bbl thesis.ind thesis.gls

auxfile: thesis.tex
	touch auxfile
	latex thesis.tex

thesis.idx thesis.glo thesis.aux: auxfile

thesis.bbl	: thesis.aux
	bibtex thesis
thesis.ind	: thesis.idx
	makeindex thesis
thesis.gls	: thesis.glo
	makeindex thesis.glo -s nomencl.ist -o thesis.gls

clean	:
	rm auxfile
	rm -f $(addprefix thesis., pdf dvi aux bbl blg)
	rm -f $(addprefix thesis., idx ind ilg glo gls log toc)
