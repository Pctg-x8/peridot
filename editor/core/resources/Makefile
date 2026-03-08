OUTPUTS = $(patsubst %.slang, %.spv, $(wildcard *.slang) $(wildcard **/*.slang))

.SUFFIXES: .slang .spv
.PHONY: all
all: $(OUTPUTS)

.slang.spv:
	slangc -o $@ $<
