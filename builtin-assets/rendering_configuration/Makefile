
TARGETS = $(patsubst %.prc,%.prcc,$(wildcard *.prc))

.SUFFIXES: .prc .prcc
.PHONY: all
all: $(TARGETS)
.prc.prcc:
	../../tools/target/debug/peridot-rendering-configuration-compiler $<
