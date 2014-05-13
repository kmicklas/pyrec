include rust.mk

BUILD       ?= .
RUNTIME_SRC ?= .

RUST_OUT_DIR = .
REV          = $(shell git rev-parse --short @{0})

AS = llvm-as
#AFLAGS =

LD = llvm-link
LDFLAGS = -lgmp

$(eval $(call RUST_CRATE,RTS,src/petool.rs,--emit=ir))

$(BUILD)/rust.bc:   $(RTS_OUT)
$(BUILD)/runtime.bc: arith.bc
#rust.bc
	$(LD) -o $@ $^

$(BUILD)/%.bc: %.ll
	$(AS) $(AFLAGS) -o $@ $<

rts-clean: $(RTS_CLEAN)

.PHONY: rts-clean

