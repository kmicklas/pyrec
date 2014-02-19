#include <stdlib.h>
#include <stdio.h>
#include <gmp.h>

typedef struct {
	mpf_t num;
} pNumber;

pNumber *loadPyretNumber(double *n) {
	printf("Loading number: %f\n", *n);
	pNumber *ret = malloc(sizeof(pNumber));
	mpf_init_set_d(ret->num, *n);
	gmp_printf("Loaded: %F\n", ret->num);
	return ret;
}

pNumber *pyret_main();

int main() {
	gmp_printf("%F\n", pyret_main()->num);
}
