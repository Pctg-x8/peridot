OUTPUTS = $(filter-out preview/common-bindings.spv, $(patsubst %.slang, %.spv, $(wildcard *.slang) $(wildcard **/*.slang)))

.SUFFIXES: .slang .spv
.PHONY: all
all: $(OUTPUTS)

.slang.spv:
	slangc -o $@ $<

preview/grid.spv: preview/grid.slang preview/common-bindings.slang
	slangc -o $@ $<

preview/origin_axes.spv: preview/origin_axes.slang preview/common-bindings.slang
	slangc -o $@ $<

preview/unlit_colored.spv: preview/unlit_colored.slang preview/common-bindings.slang
	slangc -o $@ $<

preview/rotation_handle.spv: preview/rotation_handle.slang preview/common-bindings.slang
	slangc -o $@ $<
