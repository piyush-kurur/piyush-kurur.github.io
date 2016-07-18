.PHONY: all dvi pdf clean
all	: dvi pdf
dvi	: grep.dvi
pdf	: grep.pdf

grep.dvi	: grep.tex grep.bbl
		latex grep.tex 
		latex grep.tex 
grep.ps		: grep.dvi
	dvips grep.dvi
grep.pdf	: grep.ps
		ps2pdf grep.ps grep.pdf
grep.aux	: grep.tex 
	latex grep.tex
grep.bbl	: bibdata.bib grep.aux
		bibtex grep
dist    : grep.tex bibdata.bib
	darcs dist --dist-name grep`date +%Y%b%d`

clean	:
	rm -f *.dvi *.aux *.log *.bbl *.blg grep.ps grep.pdf
