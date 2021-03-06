# The targets that are provided by this makefile.
# The hakyll targets.
HAKYLL_TARGETS= watch build rebuild clean
.PHONY: ${HAKYLL_TARGETS}

# list of publications
PUBS := $(wildcard contents/research/publication/**/*.md)
PUBDIRS := $(patsubst %.md, %, ${PUBS})
# Compass targets.
.PHONY: stylesheets stylesheets-clean stylesheets-rebuild

.PHONY: deploy deploy-cse deploy-extern site    # deploys the webpage

.PHONY: dist-clean  # cleans up every thing.

# Rules for hakyll building.

build:   stylesheets publications
rebuild: stylesheets-rebuild publications
clean:   stylesheets-clean publications-clean

${HAKYLL_TARGETS}: site
	stack exec site $@

# Rules for compass operations.
stylesheets:
	rvm `cat .ruby-version` do bundler exec compass compile

stylesheets-clean:
	rvm `cat .ruby-version` do bundler exec compass clean

stylesheets-rebuild: stylesheets-clean stylesheets


# RULES for publications

publications:
	$(foreach dir,${PUBDIRS}, make -C ${dir} pdf;)

publications-clean:
	$(foreach dir,${PUBDIRS}, make -C ${dir} clean;)

dist-clean: clean
	compass clean
	rm -f $(addprefix site, .o .hi)
	rm -f site

site:
	stack build



# Deployment Rules.
deploy-cse: build
	stack exec site deploy
deploy: deploy-cse deploy-extern

deploy-extern: deploy-cse
	banner External deploy disabled.
#	export COMMIT=`git rev-list HEAD --max-count=1`;\
#	make -C ../piyush-kurur.github.com deploy
