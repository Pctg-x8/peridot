
TARGETS = $(patsubst %.prc,%.pa1-rendering-configuration,$(wildcard *.prc))

.SUFFIXES: .prc .pa1-rendering-configuration
.PHONY: all
all: $(TARGETS)
.prc.pa1-rendering-configuration:
	../../tools/target/debug/peridot-rendering-configuration-compiler $< -o $@
