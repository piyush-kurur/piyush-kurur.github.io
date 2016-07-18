SUBDIRS=code

.PHONY	: dvi pdf dist

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
	rm -f $(addprefix submission.., pdf dvi log aux blg bbl out)
	rm -f nilpotenceTest*.tar.gz
	rm -f submission.tar.gz
	rm -f submission.tex


dist:
	darcs dist --dist-name=nilpotenceTest-`date +'%Y-%b-%d'`

submission.tex : paper.tex paper.bbl
	grep -v '\\bibliography' < paper.tex | \
		grep -v '\\end{document}' > submission.tex
	cat paper.bbl >> submission.tex
	echo '\end{document}' >> submission.tex


submission.pdf: submission.tex
	pdflatex submission.tex
	pdflatex submission.tex
	pdflatex submission.tex

submission.tar.gz: submission.tex submission.pdf
	tar cz submission.tex submission.pdf \
		--transform 's|submission\(.*\)|submission/paper\1|' \
		-f submission.tar.gz