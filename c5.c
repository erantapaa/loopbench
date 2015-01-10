#include <stdio.h>
#include <math.h>

int atoi(const char *str);

/*
    compile with: gcc -std=c11 -lm -O2 c_primes.c -o c_orig
*/

int isPrime(int n) {
    if (n < 2)
        return 0;
    else if (n == 2)
        return 1;
    else if (n % 2 == 0)
        return 0;
    int uL = sqrt(n);
    int i = 3;
    while (i <= uL) {
        if (n % i == 0)
            return 0;
        i+=2;
    }
    return 1;
}

int isPrime2(int n) {
  int m = n % 2 == 0 ? (4194304-3) : (4194304-17);
  return isPrime(m);
}

int main(int argc, char** argv) {
  int e = 22;
  if (argc > 1) {
    e = atoi(argv[1]);
  }
  int noPrimes = 0, limit = (1<<e);
  for (int n = 0; n <= limit; n++) {
      if (isPrime2(n))
          noPrimes++;
  }
  printf("Number of primes in the interval [0,%d]: %d\n", limit, noPrimes);
  return 0;
}
