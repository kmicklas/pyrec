CC = clang
CFLAGS = -c -emit-llvm

AS = llvm-as
#AFLAGS =

LD = llvm-link

LDFLAGS = -lgmp

%.ll: %.arr pyrec
	./pyrec < $< > $@

%.bc: runtime/%.c
	$(CC) $(CFLAGS) -o $@ $<

%.bc: %.ll
	$(AS) $(AFLAGS) -o $@ $<

%.lbc: runtime.bc %.bc
	$(LD) -o $@ $^

%:    %.lbc
	$(CC) $(LDFLAGS) -o $@ $<

pyrec:
	cd haskell; \
	cabal install --bindir=..



clean:
	rm -f pyrec *.lbc *.bc *.ll program

.PHONY: clean
