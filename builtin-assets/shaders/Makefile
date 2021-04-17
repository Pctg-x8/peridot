
TARGETS = $(patsubst %.csh,%.pvp,$(wildcard *.csh))

.SUFFIXES: .pvp .csh
.PHONY: all
all: $(TARGETS)
.csh.pvp:
	../../target/release/peridot-shaderbuild $<

