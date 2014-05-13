CC = clang
CFLAGS = -c -emit-llvm

AS = llvm-as
#AFLAGS =

LD = llvm-link

LDFLAGS = -lgmp

%-user.o: %.arr pyrec
	./pyrec object-file < $< > $@

%:    %-user.o runtime/runtime.c
	$(CC) $(LDFLAGS) -o $@ $^

pyrec:
	cabal install --bindir=.

clean:
	rm -f pyrec *.lbc *.bc *.ll program

.PHONY: clean
