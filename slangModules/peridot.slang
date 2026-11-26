module peridot;

namespace peridot {
public struct ObjectParameters {
  public float4x4 transformMatrix;
}

public struct CameraParameters {
  public float4x4 viewProjectionMatrix;
}

[vk::binding(0, 0)]
public ConstantBuffer<ObjectParameters> objectParameters;
[vk::binding(1, 0)]
public ConstantBuffer<CameraParameters> cameraParameters;

public struct VertexShaderContext {
  public func objectToClipSpace(float4 inObjectSpace)->float4 {
    return mul(cameraParameters.viewProjectionMatrix,
               mul(objectParameters.transformMatrix, inObjectSpace));
  }
}

public struct FragmentShaderContext {}

public enum BlendFactor {
  SrcAlpha = 0,
  DstAlpha = 1,
  InvSrcAlpha = 2,
  InvDstAlpha = 3,
  SrcColor = 4,
  DstColor = 5,
  InvSrcColor = 6,
  InvDstColor = 7,
  Constant = 8,
  ConstantOne = 9,
  ConstantZero = 10,
}

public enum BlendOp {
  Add = 0,
  Min = 1,
  Max = 2,
}

public enum BlendPreset {
  Disabled = 0,
  StraightAlpha = 1,
  PremultipliedAlpha = 2,
}
}

[__AttributeUsage(_AttributeTargets.Param)]
public struct peridot_vertexAttribute {}

[__AttributeUsage(_AttributeTargets.Var)]
public struct peridot_material_propertyAttribute {}

[__AttributeUsage(_AttributeTargets.Var)]
public struct peridot_property_nameAttribute {
  string name;
}

[__AttributeUsage(_AttributeTargets.Function)]
public struct peridot_blending_presetAttribute {
  peridot::BlendPreset preset;
}

[__AttributeUsage(_AttributeTargets.Function)]
public struct peridot_blending_preset_forAttribute {
  int attachmentIndex;
  peridot::BlendPreset preset;
}

[__AttributeUsage(_AttributeTargets.Function)]
public struct peridot_blendingAttribute {
  peridot::BlendFactor srcFactor;
  peridot::BlendFactor dstFactor;
  peridot::BlendOp op;
}

[__AttributeUsage(_AttributeTargets.Function)]
public struct peridot_blending_forAttribute {
  int attachmentIndex;
  peridot::BlendFactor srcFactor;
  peridot::BlendFactor dstFactor;
  peridot::BlendOp op;
}

[__AttributeUsage(_AttributeTargets.Function)]
public struct peridot_blending_colorAttribute {
  peridot::BlendFactor srcFactor;
  peridot::BlendFactor dstFactor;
  peridot::BlendOp op;
}

[__AttributeUsage(_AttributeTargets.Function)]
public struct peridot_blending_color_forAttribute {
  int attachmentIndex;
  peridot::BlendFactor srcFactor;
  peridot::BlendFactor dstFactor;
  peridot::BlendOp op;
}

[__AttributeUsage(_AttributeTargets.Function)]
public struct peridot_blending_alphaAttribute {
  peridot::BlendFactor srcFactor;
  peridot::BlendFactor dstFactor;
  peridot::BlendOp op;
}

[__AttributeUsage(_AttributeTargets.Function)]
public struct peridot_blending_alpha_forAttribute {
  int attachmentIndex;
  peridot::BlendFactor srcFactor;
  peridot::BlendFactor dstFactor;
  peridot::BlendOp op;
}

[__AttributeUsage(_AttributeTargets.Function)]
public struct peridot_blending_constAttribute {
  float r, g, b, a;
}
