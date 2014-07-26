# The targets that are provided by this makefile.

# The webpage targets. They get activated only when the content
# changes not when the compass files are tweaked or the hakyll source
# changes.
.PHONY: build	    # builds the webpage. This will *not* regenerate
		    # the css files from the compass nor will it build
		    # hakyll expecutable.

.PHONY: deploy      # deploys the webpage
.PHONY: clean       # cleans  the webpage
.PHONY: rebuild     # rebuilds the webpage

# Targets that build the hakyll source and compass files.
.PHONY: stylesheets # compiles the compass files to css
.PHONY: site        # compiles the actual hakyll executable.
.PHONY: compile     # compiles stylesheets and site
.PHONY: dist-clean  # cleans up every thing.

build:
	./site build

compile: stylesheets site


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

deploy-cse: build
	./site deploy
deploy: deploy-cse deploy-extern

deploy-extern: deploy-cse
	export COMMIT=`git rev-list HEAD --max-count=1`;\
	make -C ../piyush-kurur.github.com deploy
