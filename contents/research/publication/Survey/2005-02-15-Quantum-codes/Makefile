GENEXTS=dvi pdf aux blg bbl log
.PHONY: all clean pdf dvi
all: pdf dvi

pdf: lecture.pdf
dvi: lecture.dvi
lecture.pdf : lecture.tex
	pdflatex lecture.tex
	pdflatex lecture.tex
lecture.dvi : lecture.tex
	latex lecture.tex
	latex lecture.tex
clean:
	rm -f $(addprefix lecture.,${GENEXTS})
