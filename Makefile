.PHONY: all stylesheets deploy
.PHONY: clean dist-clean site rebuild

all: stylesheets site
	./site build

rebuild:
	./site rebuild

stylesheets:
	compass compile

clean:  site
	compass clean
	./site clean

dist-clean: clean
	rm -f $(addprefix site, .o .hi)
	rm -f site

site: site.hs
	ghc --make -Wall site.hs

deploy: all
	./site deploy