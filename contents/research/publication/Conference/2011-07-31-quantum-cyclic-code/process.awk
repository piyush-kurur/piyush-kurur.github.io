#!/bin/awk -F
BEGIN   { xscale=10000; yscale=10000.0; 
    print "0", "0";
        }
/^[^#]/ { 
    if( $1 % xscale == 0){
	printf "%d\t%f\n", $1/xscale, $2/yscale;
    }
  }
    