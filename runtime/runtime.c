#include <stdlib.h>
#include <stdio.h>
#include <gmp.h>

typedef struct {
	mpf_t num;
} pNumber;

pNumber *newPyretNumber() {
	pNumber *ret = malloc(sizeof(pNumber));
	mpf_init(ret->num);
	return ret;
}

pNumber *loadPyretNumber(double *n) {
	pNumber *ret = newPyretNumber();
	printf("Loading number: %f\n", *n);
	mpf_set_d(ret->num, *n);
	gmp_printf("Loaded: %Ff\n", ret->num);
	return ret;
}

pNumber *pyretPlus(pNumber *a, pNumber *b) {
	pNumber *ret = newPyretNumber();
	mpf_add(ret->num, a->num, b->num);
	return ret;
}

pNumber *pyretMinus(pNumber *a, pNumber *b) {
	pNumber *ret = newPyretNumber();
	mpf_sub(ret->num, a->num, b->num);
	return ret;
}

pNumber *pyretTimes(pNumber *a, pNumber *b) {
	pNumber *ret = newPyretNumber();
	mpf_mul(ret->num, a->num, b->num);
	return ret;
}

pNumber *pyretDivide(pNumber *a, pNumber *b) {
	pNumber *ret = newPyretNumber();
	mpf_div(ret->num, a->num, b->num);
	return ret;
}

pNumber *pyret_main();

int main() {
	gmp_printf("%Ff\n", pyret_main()->num);
}
