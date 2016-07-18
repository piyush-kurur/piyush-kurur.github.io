#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>
#include <limits.h>
/* Code to plot all even and odd exponents */

void plot_exponent(int,int);
int get_power(int,int);
int gcd(int,int);

int main(int argc, char **argv)
{
  int q,n;
  printf("#Maximum integer is %d data incorrect is the argument is larger\n",INT_MAX);
  printf("#m\teven\todd\ttotal\n");
  fprintf(stderr,"Maximum integer is %d data incorrect is the argument is larger\n",INT_MAX);
  if (argc < 3){
    fprintf(stderr,"usage: %s q n",argv[0]);
    return 1;
  }
  q = atoi(argv[1]);
  n = atoi(argv[2]);
  
  plot_exponent(q,n);

  return 0;
}

void plot_exponent(int q, int n){
  int t;
  int count =0;
  int ecount=0;
  int ocount=0;
  for(int m = 5; m <= n; m++){
    if( gcd(q,m) == 1 && (t = get_power(q,m)) > 0){
      count ++;
      if( t % 2 == 0) {ecount ++;}
      else            {ocount ++;}
    }
    printf("%d\t%d\t%d\t%d\n",m,ecount,ocount,count);
  }
}

int gcd(int a, int b){
  int temp;
  int rem;
  if ( a < b ) {
    temp = a;
    a = b;
    b = temp;
  }
  while( a % b != 0){
    rem = a % b;
    a = b;
    b = rem;
  }
  return b;
}
  

int get_power(int q, int m)
{
  int q_t; int t;
  q_t =  q % m;
  t   = 1;
  do {
    if( (q_t + 1) % m == 0){return t;}
    q_t = (q * q_t) % m;
    t++;
  } while( q_t != q );

  return -1;
}
