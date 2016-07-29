LHS2TEX_STYLE= poly
LATEX_OPTIONS=-src-specials
PDFLATEX_OPTIONS=

BIBTEXDB   = references.bib
GENERATED  = dvi pdf log blg bbl aux toc ind idx ilg \
             out tex fls ptb tdo fdb_latexmk

LATEX=latex ${LATEX_OPTIONS}

LHS2TEX = lhs2TeX --${LHS2TEX_STYLE}

.DEFAULT_GOAL=pdf

.PHONY : dvi pdf all clean


dvi	: main.dvi
pdf	: main.pdf
all	: dvi pdf

main.dvi: main.tex references.bib
	latexmk main.tex
main.pdf: main.tex references.bib
	latexmk -pdf main.tex

main.tex: main.lhs
	${LHS2TEX} main.lhs > main.tex

clean	:
	rm -f $(addprefix main.,${GENERATED})

sha1:
	sha1sum main.pdf
