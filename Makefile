CC = clang
CFLAGS = -c -emit-llvm

AS = llvm-as
#AFLAGS =

LD = llvm-link
LDFLAGS = -lgmp

%.bc: %.arr pyrec
	./pyrec bitcode < $< > $@

%-linked.bc: runtime.bc %.bc
	$(LD) -o $@ $^

%:    %-linked.bc
	$(CC) $(LDFLAGS) -o $@ $<

pyrec:
	cabal install --bindir=.

clean:
	rm -f pyrec *.lbc *.bc *.ll

.PHONY: clean
