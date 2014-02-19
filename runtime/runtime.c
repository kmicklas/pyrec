#include <stdlib.h>
#include <gmp.h>

typedef struct {
	mpf_t num;
} pNumber;

pNumber *loadPyretNumber(double n) {
	pNumber *ret = malloc(sizeof(pNumber));
	mpf_init(ret->num);
	mpf_set_d(ret->num, n);
	return ret;
}

pNumber *pyret_main();

int main() {
	gmp_printf("%F\n", pyret_main()->num);
}
