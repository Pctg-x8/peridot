import shaders

type Camera = object
  vp {.rowMajor.}: Matrix4x4
  obj {.rowMajor.}: Matrix4x4

var
  camera {.uniform, descriptorSet: 0, binding: 0.}: Camera
  pos {.input, location: 0.}: Vector3
  uv {.input, location: 1.}: Vector2
  raster_position {.output, buildIn: Position.}: Vector4
  uv_v_out {.output: location: 0.}: Vector2

  uv_v_in {.input, location: 0.}: Vector2
  sv_target {.output, location: 0.}: Vector4
  tex {.uniform, descriptor: 0, binding: 1.}: Sampler2D

proc vsMain() {.stage: Vertex.} =
  raster_position = transpose(camera.vp) * transpose(camera.pos) * construct[Vector4](pos, 1.0'f32)
  uv_v_out = uv

proc fsMain() {.stage: Fragment.} =
  sv_target = texture(tex, uv_v_out)

