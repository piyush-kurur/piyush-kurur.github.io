NAME = JohannsenKururLengler-QEAs

GRAPHICS =


FILES = defines.tex


BIBFILE = paper.bib

CLEAN_FILES = $(GRAPHICS:.eps=.jpg) $(NAME).pdf *.snm *.toc *.out *.nav *.log *.dvi *.ps *.aux *.bbl *.blg *.1 *.mpx *.bak *~

.PHONY:	clean open update pdf

you: $(NAME).pdf

%.pdftex: %.fig
	fig2dev -L pdftex $< $(<:.fig=.pdftex)

%.pdftex_t: %.pdftex
	fig2dev -L pdftex_t -F -p $(<:.fig=.pdftex) $< $@

%.jpg: %.eps
	convert $< $@

pdf: $(NAME).pdf

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
