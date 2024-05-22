.SUFFIXES: .vsh .vspv .fsh .fspv

VERTEX_SHADERS = $(patsubst %.vsh,%.vspv,$(wildcard *.vsh))
FRAGMENT_SHADERS = $(patsubst %.fsh,%.fspv,$(wildcard *.fsh))

.vsh.vspv:
	@glslc -O -fshader-stage=vert -o $@ $<

.fsh.fspv:
	@glslc -O -fshader-stage=frag -o $@ $<

.PHONY: all
all: $(VERTEX_SHADERS) $(FRAGMENT_SHADERS)
