CC = clang
CFLAGS = -c -emit-llvm

AS = llvm-as
#AFLAGS =

LD = llvm-link

LDFLAGS = -lgmp

program: program.bc
program.bc: runtime.bc out.bc

out.ll: pyrec
	./pyrec > out.ll

%.bc: runtime/%.c
	$(CC) $(CFLAGS) -o $@ $<

%.bc: %.ll
	$(AS) $(AFLAGS) -o $@ $<

%.bc: %.bc
	$(LD) -o $@ $^

%:    %.bc
	$(CC) $(LDFLAGS) -o $@ $<

pyrec:
	cd haskell; \
	cabal install --bindir=..



clean:
	rm -f pyrec *.bc *.ll program

.PHONY: clean
