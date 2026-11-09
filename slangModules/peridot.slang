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
}

[__AttributeUsage(_AttributeTargets.Param)]
public struct peridot_vertexAttribute {}

[__AttributeUsage(_AttributeTargets.Var)]
public struct peridot_material_propertyAttribute {}

[__AttributeUsage(_AttributeTargets.Var)]
public struct peridot_property_nameAttribute {
  string name;
}
