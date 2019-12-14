import shaders

type
  Camera = object
    vp {.rowMajor.}: Matrix4x4
    obj {.rowMajor.}: Matrix4x4

var
  camera {.uniform, descriptorSet: 0, binding: 0.}: Camera
  pos {.input, location: 0.}: Vector3
  uv {.input, location: 1.}: Vector2
  raster_position {.output, builtIn: Position.}: Vector4
  uv_v_out {.output, location: 0.}: Vector2

  uv_v_in {.input, location: 0.}: Vector2
  sv_target {.output, location: 0.}: Vector4

proc project(p: Vector3): Vector4 = camera.vp * camera.obj * construct[Vector4](pos, 1.0'f32)

proc vsMain() {.stage: Vertex.} =
  raster_position = project(pos)
  uv_v_out = uv

proc fsMain() {.stage: Fragment.} =
  sv_target = construct[Vector4](uv_v_in, 0.0'f32, 1.0'f32)
