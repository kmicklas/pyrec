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

%-linked.bc: runtime.bc %.bc
	$(LD) -o $@ $^

%:    %-linked.bc
	$(CC) $(LDFLAGS) -o $@ $<

pyrec:
	cabal install --bindir=.

clean:
	rm -f pyrec *.lbc *.bc *.ll program

.PHONY: clean
