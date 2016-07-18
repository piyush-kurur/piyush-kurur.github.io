PRIME=2
RANGE=100000
exponents : exponents.c
	gcc -Wall -O2 -std=c99 exponents.c -o exponents

density  : exponents
	./exponents ${PRIME} ${RANGE} > density

density.ps : density gplot
	gnuplot gplot

clean	: 
	rm -f exponents  density density.ps
