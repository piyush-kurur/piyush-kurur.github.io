LATEX_GEN= aux glob fls out fdb_latexmk toc pdf log tdo blg bbl
.PHONY: all build clean latest-verse watch

all: main.pdf


contents.tex: contents.v
	coqdoc --latex --body-only --short contents.v

main.pdf: contents.tex

clean:
	rm -f $(addprefix main., ${LATEX_GEN})
	rm -f $(addprefix contents., ${LATEX_GEN})
	rm -f contents.tex coqdoc.sty


main.pdf: main.tex contents.tex references.bib
	latexmk -pdf main.tex
