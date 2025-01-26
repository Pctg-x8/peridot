
TARGETS = $(patsubst %.csh,%.pvp,$(wildcard *.csh))

.SUFFIXES: .pvp .csh
.PHONY: all
all: $(TARGETS)
.csh.pvp:
	../../../../tools/target/debug/peridot-shaderbuild $<

