.SUFFIXES: .vsh .vspv .fsh .fspv .comp .cspv

VERTEX_SHADERS = $(patsubst %.vsh,%.vspv,$(wildcard *.vsh) $(wildcard */*.vsh))
FRAGMENT_SHADERS = $(patsubst %.fsh,%.fspv,$(wildcard *.fsh) $(wildcard */*.fsh))
COMPUTE_SHADERS = $(patsubst %.comp,%.cspv,$(wildcard *.comp) $(wildcard */*.comp))

.vsh.vspv:
	@glslc -O -fshader-stage=vert -o $@ $<

.fsh.fspv:
	@glslc -O -fshader-stage=frag -o $@ $<

.comp.cspv:
	@glslc -O -fshader-stage=comp -o $@ $<

.PHONY: all
all: $(VERTEX_SHADERS) $(FRAGMENT_SHADERS) $(COMPUTE_SHADERS)
