
TARGETS = $(patsubst %.csh,%.pss,$(wildcard *.csh))

.SUFFIXES: .pss .csh
.PHONY: all
all: $(TARGETS)
.csh.pss:
	../../tools/target/release/peridot-shaderbuild2 $<
