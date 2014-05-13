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

pNumber *pyrecLoadNumber(double n) {
	pNumber *ret = newPyretNumber();
	mpf_set_d(ret->num, n);
	return ret;
}

pNumber *pyrecPlus(pNumber *a, pNumber *b) {
	pNumber *ret = newPyretNumber();
	mpf_add(ret->num, a->num, b->num);
	return ret;
}

pNumber *pyrecMinus(pNumber *a, pNumber *b) {
	pNumber *ret = newPyretNumber();
	mpf_sub(ret->num, a->num, b->num);
	return ret;
}

pNumber *pyrecTimes(pNumber *a, pNumber *b) {
	pNumber *ret = newPyretNumber();
	mpf_mul(ret->num, a->num, b->num);
	return ret;
}

pNumber *pyrecDivide(pNumber *a, pNumber *b) {
	pNumber *ret = newPyretNumber();
	mpf_div(ret->num, a->num, b->num);
	return ret;
}

void pyrecReturn(pNumber *v) {
	gmp_printf("%Ff\n", v->num);
	exit(0);
}

void userMain();

int main() {
	userMain();
}
