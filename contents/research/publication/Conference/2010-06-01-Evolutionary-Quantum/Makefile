NAME = paper

GRAPHICS =

FILES = defines.tex abstract.tex  introduction.tex conclusion.tex quantum.tex markov.tex rsh.tex onemax.tex leadingones.tex discrepancy.tex


BIBFILE = $(NAME).bib

CLEAN_FILES = $(GRAPHICS:.eps=.jpg) $(NAME).pdf *.snm *.toc *.out *.nav *.log *.dvi *.ps *.aux *.bbl *.blg *.1 *.mpx *.bak *~

.PHONY:	clean open update pdf

all: pdf
pdf: $(NAME).pdf

%.pdftex: %.fig
	fig2dev -L pdftex $< $(<:.fig=.pdftex)

%.pdftex_t: %.pdftex
	fig2dev -L pdftex_t -F -p $(<:.fig=.pdftex) $< $@

%.jpg: %.eps
	convert $< $@


$(NAME).pdf: $(GRAPHICS:.eps=.jpg) $(FILES) $(BIBFILE) $(NAME).tex
	pdflatex -draftmode -halt-on-error -file-line-error $(NAME)
	bibtex $(NAME)
	bibtex $(NAME)
	pdflatex -draftmode -halt-on-error -file-line-error $(NAME)
	pdflatex -halt-on-error -file-line-error $(NAME)
	pdflatex -halt-on-error -file-line-error $(NAME)

clean:
	rm -f  $(CLEAN_FILES)

open:	$(NAME).pdf
	xpdf -remote myServer -z 125 $< &

update:	$(NAME).pdf	
	xpdf -remote myServer -reload
