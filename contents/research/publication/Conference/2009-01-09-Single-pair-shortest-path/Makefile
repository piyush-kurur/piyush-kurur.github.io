MASTER      = SSSP
CLEAN_FILES = *.aux *.log *.bbl *.blg *.bak \
		$(MASTER).dvi $(MASTER).ps $(MASTER).pdf .dvi .pdf 
TEXSOURCES = *.tex
PAPER = letter
FIGURES = figure-2.pstex_t

all: ps pdf

%.dvi: %.tex $(TEXSOURCES) $(FIGURES)
	latex $<
	( grep citat $(*F).aux >/dev/null && bibtex $(*F) ) || true
	latex $<
	latex $<
	latex --src $<

%.ps: %.dvi
	dvips -t $(PAPER) $<

%.pdf: %.ps
	ps2pdf14 -dPDFSETTINGS=/printer -dMaxSubsetPct=100 -dSubsetFonts=true -dEmbedAllFonts=true -sPAPERSIZE=$(PAPER) $<

latex dvi: $(MASTER).dvi

ps postscript dvips: $(MASTER).ps

pdf: $(MASTER).pdf

clean:
	rm -f $(CLEAN_FILES)
	rm -f figure-2.pstex figure-2.pstex_t

figure-2.pstex : figure-2.fig
	fig2dev -L pstex figure-2.fig figure-2.pstex

figure-2.pstex_t : figure-2.pstex figure-2.fig
	fig2dev -L pstex_t -p figure-2.pstex \
		figure-2.fig figure-2.pstex_t