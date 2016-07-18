FIGS=basicgates Cfn dotproduct encode matvect uniformC

%.pstex_t: %.pstex %.fig
	fig2dev -L pstex_t -p figs/$^ $@
%.pstex: %.fig
	fig2dev -L pstex $< $@

.PHONY: all clean

all: $(addsuffix .pstex, ${FIGS}) $(addsuffix .pstex_t, ${FIGS})

clean:
	rm -f $(addsuffix .pstex, ${FIGS})
	rm -f $(addsuffix .pstex_t, ${FIGS})
