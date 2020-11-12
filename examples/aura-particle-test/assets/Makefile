
GLSLC = glslc
OUTPUTS = shaders/uv_v.spv shaders/blit_f.spv shaders/colored_vertex_v.spv shaders/colored_vertex_f.spv

default: $(OUTPUTS)

%_v.spv: %.vert
	$(GLSLC) -o $@ $^

%_f.spv: %.frag
	$(GLSLC) -o $@ $^
