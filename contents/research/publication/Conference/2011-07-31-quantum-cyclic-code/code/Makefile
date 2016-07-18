
.PHONY: all build configure haddock
all	: configure build haddock

configure :
	runhaskell ./Setup.lhs configure
build	: 
	runhaskell ./Setup.lhs build

haddock :
	runhaskell ./Setup.lhs haddock

