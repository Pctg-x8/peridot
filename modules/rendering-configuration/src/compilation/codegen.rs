use std::{borrow::Cow, collections::HashMap};

use peridot_semantic_shader::VertexInputSemantic;

use crate::{
    DescriptorTypeVk, FaceCulling, FrontFace, PolygonRasterizationMode, PropertyDestinationVk,
    PropertyMappingVk, PropertyType, RenderingOptionOverrides,
};

use super::{syntax, tokenizer::Identifier};

#[derive(Debug)]
pub struct RenderingConfiguration {
    pub properties: Vec<PropertyData>,
    pub passes: HashMap<String, PassData>,
}
impl RenderingConfiguration {
    pub fn new(s: Vec<syntax::ToplevelElement>) -> Self {
        let mut properties = Vec::new();
        let mut passes = HashMap::new();

        for elem in s {
            match elem {
                syntax::ToplevelElement::PropertiesBlock(ps) => {
                    struct Attributes {
                        immutable: bool,
                        per_draw_call: bool,
                    }
                    impl Default for Attributes {
                        fn default() -> Self {
                            Self {
                                immutable: false,
                                per_draw_call: false,
                            }
                        }
                    }

                    let mut attr = Attributes::default();
                    for p in ps.elements {
                        match p {
                            syntax::PropertiesBlockElement::Attribute(a) => {
                                if a.name.as_str() == "Immutable" {
                                    attr.immutable = true;
                                } else if a.name.as_str() == "PerDrawCall" {
                                    attr.per_draw_call = true;
                                } else {
                                    panic!("unknown attribute: {:?}", a.name);
                                }
                            }
                            syntax::PropertiesBlockElement::Property(p) => {
                                let update_frequency = if attr.immutable {
                                    PropertyUpdateFrequency::Immutable
                                } else if attr.per_draw_call {
                                    PropertyUpdateFrequency::PerDrawCall
                                } else {
                                    PropertyUpdateFrequency::default()
                                };

                                properties.push(PropertyData {
                                    name: p.name.as_str().into(),
                                    r#type: property_type_from_syntax(p.r#type),
                                    default: fold_expr(p.default),
                                    update_frequency,
                                });
                                attr = Attributes::default();
                            }
                        }
                    }
                }
                syntax::ToplevelElement::PassBlock(syntax::PassBlock::SimpleDerive {
                    name,
                    org_name,
                    ..
                }) => {
                    passes.insert(
                        name.as_str().into(),
                        PassData {
                            option_overrides: None,
                            deriving: Some(org_name.as_str().into()),
                            vertex_bindings: Vec::new(),
                            shader_code: None,
                        },
                    );
                }
                syntax::ToplevelElement::PassBlock(syntax::PassBlock::Standard {
                    name,
                    contents,
                    ..
                }) => {
                    let mut vertex_bindings = None;
                    let mut shader_code = None;
                    let mut option_overrides = None::<RenderingOptionOverrides>;

                    for c in contents {
                        match c {
                            syntax::PassBlockContent::VertexBindingsBlock { entries, .. } => {
                                if vertex_bindings.is_some() {
                                    panic!("duplicate vertex bindings block");
                                }

                                vertex_bindings = Some(
                                    entries
                                        .into_iter()
                                        .map(|(name, _, ty, _, semantic_name, _)| {
                                            PassVertexBindingData {
                                                name: name.as_str().into(),
                                                r#type: property_type_from_syntax(ty),
                                                semantic: parse_vi_semantic(&semantic_name),
                                            }
                                        })
                                        .collect(),
                                );
                            }
                            syntax::PassBlockContent::ShaderBlock { content, .. } => {
                                if shader_code.is_some() {
                                    panic!("duplicate shader block");
                                }

                                shader_code = Some(content.into());
                            }
                            syntax::PassBlockContent::RenderOptions { entries, .. } => {
                                let o = option_overrides.get_or_insert_default();
                                for (e, _) in entries {
                                    if e.as_str() == "PointPolygon" {
                                        if let Some(m) = o.mode {
                                            panic!("conflicting PolygonRasterizationMode: {m:?}");
                                        }

                                        o.mode = Some(PolygonRasterizationMode::Point);
                                    } else if e.as_str() == "LinedPolygon" {
                                        if let Some(m) = o.mode {
                                            panic!("conflicting PolygonRasterizationMode: {m:?}");
                                        }

                                        o.mode = Some(PolygonRasterizationMode::Line);
                                    } else if e.as_str() == "FilledPolygon" {
                                        if let Some(m) = o.mode {
                                            panic!("conflicting PolygonRasterizationMode: {m:?}");
                                        }

                                        o.mode = Some(PolygonRasterizationMode::Fill);
                                    } else if e.as_str() == "NoCulling" {
                                        if let Some(x) = o.culling {
                                            panic!("conflicting FaceCulling: {x:?}");
                                        }

                                        o.culling = Some(FaceCulling::None);
                                    } else if e.as_str() == "CullFront" {
                                        if let Some(x) = o.culling {
                                            panic!("conflicting FaceCulling: {x:?}");
                                        }

                                        o.culling = Some(FaceCulling::Front);
                                    } else if e.as_str() == "CullBack" {
                                        if let Some(x) = o.culling {
                                            panic!("conflicting FaceCulling: {x:?}");
                                        }

                                        o.culling = Some(FaceCulling::Back);
                                    } else if e.as_str() == "CullBoth" {
                                        if let Some(x) = o.culling {
                                            panic!("conflicting FaceCulling: {x:?}");
                                        }

                                        o.culling = Some(FaceCulling::Both);
                                    } else if e.as_str() == "CounterClockwiseAsFront" {
                                        if let Some(x) = o.front_face {
                                            panic!("conflicting FrontFace: {x:?}");
                                        }

                                        o.front_face = Some(FrontFace::CounterClockwise);
                                    } else if e.as_str() == "ClockwiseAsFront" {
                                        if let Some(x) = o.front_face {
                                            panic!("conflicting FrontFace: {x:?}");
                                        }

                                        o.front_face = Some(FrontFace::Clockwise);
                                    } else {
                                        panic!("unknown option: {}", e.as_str());
                                    }
                                }
                            }
                        }
                    }

                    passes.insert(
                        name.as_str().into(),
                        PassData {
                            option_overrides,
                            deriving: None,
                            vertex_bindings: vertex_bindings.unwrap_or_else(Vec::new),
                            shader_code,
                        },
                    );
                }
            }
        }

        Self { properties, passes }
    }

    // いったんターゲットをVulkanに限定する(他API対応もでてきたらそのときに対応する)
    pub fn gen_vk_prelude(
        &self,
    ) -> (
        String,
        HashMap<String, (PropertyType, PropertyMappingVk)>,
        Vec<DescriptorTypeVk>,
    ) {
        let mut specialized_constants = Vec::<(Cow<str>, &PropertyType)>::new();
        let mut combined_constants = Vec::new();
        let mut push_constant_block_members = Vec::new();
        let mut descriptor_sets = Vec::new();
        let mut descriptor_set_bindings = Vec::new();
        let mut realtime_buffer_members = Vec::new();
        let mut property_mapping = HashMap::new();

        for p in self.properties.iter() {
            match p.update_frequency {
                // compound typeはそのままspecialized constantsにできないのでスカラ型に分解
                PropertyUpdateFrequency::Immutable => match p.r#type {
                    PropertyType::Float2 => {
                        let r_dest =
                            PropertyDestinationVk::SpecConstant(specialized_constants.len());
                        specialized_constants.push((
                            format!("{name}_R", name = p.name).into(),
                            &PropertyType::Float,
                        ));
                        let g_dest =
                            PropertyDestinationVk::SpecConstant(specialized_constants.len());
                        specialized_constants.push((
                            format!("{name}_G", name = p.name).into(),
                            &PropertyType::Float,
                        ));
                        combined_constants.push(format!(
                            "static const float2 {name} = float2({name}_R, {name}_G);",
                            name = p.name
                        ));
                        match property_mapping.entry(p.name.clone()) {
                            std::collections::hash_map::Entry::Vacant(x) => {
                                x.insert((
                                    p.r#type.clone(),
                                    PropertyMappingVk::Splitted(vec![r_dest, g_dest]),
                                ));
                            }
                            std::collections::hash_map::Entry::Occupied(x) => {
                                panic!("property {} conflicting with {:?}", x.key(), x.get());
                            }
                        }
                    }
                    PropertyType::Float4 | PropertyType::RGB => {
                        let r_dest =
                            PropertyDestinationVk::SpecConstant(specialized_constants.len());
                        specialized_constants.push((
                            format!("{name}_R", name = p.name).into(),
                            &PropertyType::Float,
                        ));
                        let g_dest =
                            PropertyDestinationVk::SpecConstant(specialized_constants.len());
                        specialized_constants.push((
                            format!("{name}_G", name = p.name).into(),
                            &PropertyType::Float,
                        ));
                        let b_dest =
                            PropertyDestinationVk::SpecConstant(specialized_constants.len());
                        specialized_constants.push((
                            format!("{name}_B", name = p.name).into(),
                            &PropertyType::Float,
                        ));
                        let a_dest =
                            PropertyDestinationVk::SpecConstant(specialized_constants.len());
                        specialized_constants.push((
                            format!("{name}_A", name = p.name).into(),
                            &PropertyType::Float,
                        ));
                        combined_constants.push(format!(
                            "static const float4 {name} = float4({name}_R, {name}_G, {name}_B, {name}_A);", name = p.name
                        ));
                        match property_mapping.entry(p.name.clone()) {
                            std::collections::hash_map::Entry::Vacant(x) => {
                                x.insert((
                                    p.r#type.clone(),
                                    PropertyMappingVk::Splitted(vec![
                                        r_dest, g_dest, b_dest, a_dest,
                                    ]),
                                ));
                            }
                            std::collections::hash_map::Entry::Occupied(x) => {
                                panic!("property {} conflicting with {:?}", x.key(), x.get());
                            }
                        }
                    }
                    _ => {
                        specialized_constants.push(((&p.name).into(), &p.r#type));
                        match property_mapping.entry(p.name.clone()) {
                            std::collections::hash_map::Entry::Vacant(x) => {
                                x.insert((
                                    p.r#type.clone(),
                                    PropertyMappingVk::Direct(PropertyDestinationVk::SpecConstant(
                                        specialized_constants.len() - 1,
                                    )),
                                ));
                            }
                            std::collections::hash_map::Entry::Occupied(x) => {
                                panic!("property {} conflicting with {:?}", x.key(), x.get());
                            }
                        }
                    }
                },
                PropertyUpdateFrequency::PerDrawCall => {
                    push_constant_block_members.push((&p.name, &p.r#type));
                    match property_mapping.entry(p.name.clone()) {
                        std::collections::hash_map::Entry::Vacant(x) => {
                            x.insert((
                                p.r#type.clone(),
                                PropertyMappingVk::Direct(
                                    PropertyDestinationVk::PushConstantBlock(
                                        push_constant_block_members.len() - 1,
                                    ),
                                ),
                            ));
                        }
                        std::collections::hash_map::Entry::Occupied(x) => {
                            panic!("property {} conflicting with {:?}", x.key(), x.get());
                        }
                    }
                }
                PropertyUpdateFrequency::Dynamic => {
                    descriptor_sets.push((&p.name, &p.r#type));
                    match property_mapping.entry(p.name.clone()) {
                        std::collections::hash_map::Entry::Vacant(x) => {
                            x.insert((
                                p.r#type.clone(),
                                PropertyMappingVk::Direct(PropertyDestinationVk::DescriptorSet(
                                    descriptor_sets.len() - 1,
                                )),
                            ));
                        }
                        std::collections::hash_map::Entry::Occupied(x) => {
                            panic!("property {} conflicting with {:?}", x.key(), x.get());
                        }
                    }
                }
                PropertyUpdateFrequency::Realtime => {
                    realtime_buffer_members.push((&p.name, &p.r#type));
                    match property_mapping.entry(p.name.clone()) {
                        std::collections::hash_map::Entry::Vacant(x) => {
                            x.insert((
                                p.r#type.clone(),
                                PropertyMappingVk::Direct(PropertyDestinationVk::RealtimeBuffer(
                                    realtime_buffer_members.len() - 1,
                                )),
                            ));
                        }
                        std::collections::hash_map::Entry::Occupied(x) => {
                            panic!("property {} conflicting with {:?}", x.key(), x.get());
                        }
                    }
                }
            }
        }

        let mut code = String::new();

        // builtin prelude
        code.push_str(
            r#"namespace PeridotCameraParameters {
struct UniformBlock {
    float4x4 viewProjectionMatrix;
}
[vk::binding(0, 0)]
ConstantBuffer<UniformBlock> uniformBlock;

static inline float4x4 viewProjectionMatrix() {
    return uniformBlock.viewProjectionMatrix;
}
}
"#,
        );
        code.push_str(
            r#"namespace PeridotObjectParameters {
struct UniformBlock {
    float4x4 transformMatrix;
}
[vk::binding(0, 1)]
ConstantBuffer<UniformBlock> uniformBlock;

static inline float4x4 transformMatrix() {
    return uniformBlock.transformMatrix;
}
}
"#,
        );

        // material prelude
        code.push_str("namespace PeridotMaterialParameters {\n");
        for (n, (name, ty)) in specialized_constants.into_iter().enumerate() {
            code.push_str("[vk::constant_id(");
            code.push_str(&n.to_string());
            code.push_str(")]\nconst ");
            print_property_type(ty, &mut code);
            code.push_str(" ");
            code.push_str(&name);
            code.push_str(" = 0;\n");
        }

        for x in combined_constants {
            code.push_str(&x);
            code.push_str("\n");
        }

        if !push_constant_block_members.is_empty() {
            code.push_str("struct PerDrawCall {\n");
            for (name, ty) in push_constant_block_members {
                code.push_str("    ");
                print_property_type(ty, &mut code);
                code.push_str(" ");
                code.push_str(name);
                code.push_str(";\n");
            }
            code.push_str("}\n[vk::push_constant]\nPerDrawCall perDrawCall;\n");
        }

        let realtime_buffer_binding_index = descriptor_sets.len();
        for (n, (name, ty)) in descriptor_sets.into_iter().enumerate() {
            // set indexはあとで調整(直接うめこみたくはないな......)
            code.push_str("[vk::binding(");
            code.push_str(&n.to_string());
            code.push_str(", 2)]\n");
            print_property_type(ty, &mut code);
            code.push_str(" ");
            code.push_str(name);
            code.push_str(";\n");

            descriptor_set_bindings.push(match ty {
                PropertyType::Texture2D => DescriptorTypeVk::CombinedImageSampler,
                x => {
                    todo!("non-texture dynamic properties(constructs single uniform block): {x:?}")
                }
            });
        }

        if !realtime_buffer_members.is_empty() {
            code.push_str("struct RealtimeBuffer {\n");
            for (name, ty) in realtime_buffer_members {
                code.push_str("    ");
                print_property_type(ty, &mut code);
                code.push_str(" ");
                code.push_str(name);
                code.push_str(";\n");
            }
            code.push_str("}\n[vk::binding(");
            code.push_str(&realtime_buffer_binding_index.to_string());
            code.push_str(", 2)]\nConstantBuffer<RealtimeBuffer> realtime;\n");
        }

        code.push_str("}\n");
        (code, property_mapping, descriptor_set_bindings)
    }
}

#[derive(Debug)]
pub struct PropertyData {
    pub name: String,
    pub r#type: PropertyType,
    pub default: Value,
    pub update_frequency: PropertyUpdateFrequency,
}

#[derive(Debug)]
pub enum PropertyUpdateFrequency {
    Immutable,
    PerDrawCall,
    Dynamic,
    Realtime,
}
impl Default for PropertyUpdateFrequency {
    fn default() -> Self {
        PropertyUpdateFrequency::Dynamic
    }
}

#[derive(Debug)]
pub struct PassData {
    pub option_overrides: Option<RenderingOptionOverrides>,
    pub deriving: Option<String>,
    pub vertex_bindings: Vec<PassVertexBindingData>,
    pub shader_code: Option<String>,
}
impl PassData {
    pub fn gen_vk_code(&self) -> (String, HashMap<VertexInputSemantic, u32>) {
        let mut semantic_to_location_map = HashMap::with_capacity(self.vertex_bindings.len());

        let mut code = String::new();
        if let Some(ref d) = self.deriving {
            eprintln!("todo: deriving: {d}");
        }
        if !self.vertex_bindings.is_empty() {
            code.push_str("struct Vertex {\n");
            for (n, vb) in self.vertex_bindings.iter().enumerate() {
                match semantic_to_location_map.entry(vb.semantic.clone()) {
                    std::collections::hash_map::Entry::Vacant(x) => {
                        x.insert(n as _);
                    }
                    std::collections::hash_map::Entry::Occupied(x) => {
                        panic!(
                            "conflicting vertex semantic {:?} with location {}",
                            x.key(),
                            x.get()
                        );
                    }
                }

                code.push_str("    [vk::location(");
                code.push_str(&n.to_string());
                code.push_str(")]\n    ");
                print_property_type(&vb.r#type, &mut code);
                code.push_str(" ");
                code.push_str(&vb.name);
                code.push_str(" : ");
                print_vi_semantic(&vb.semantic, &mut code);
                code.push_str(";\n");
            }
            code.push_str("}\n\n");
        }
        if let Some(ref s) = self.shader_code {
            code.push_str(s);
        }

        (code, semantic_to_location_map)
    }
}

#[derive(Debug)]
pub struct PassVertexBindingData {
    pub name: String,
    pub r#type: PropertyType,
    pub semantic: VertexInputSemantic,
}

fn property_type_from_syntax(x: syntax::Type) -> PropertyType {
    match x {
        syntax::Type::Texture2D(_) => PropertyType::Texture2D,
        syntax::Type::RGB(_) => PropertyType::RGB,
        syntax::Type::UInt(_) => PropertyType::UInt,
        syntax::Type::Int(_) => PropertyType::Int,
        syntax::Type::Float2(_) => PropertyType::Float2,
        syntax::Type::Float4(_) => PropertyType::Float4,
    }
}

fn print_property_type(pt: &PropertyType, sink: &mut String) {
    match pt {
        // Texture: treated as combined image sampler
        PropertyType::Texture2D => sink.push_str("Sampler2D"),
        PropertyType::RGB => sink.push_str("float4"),
        PropertyType::UInt => sink.push_str("uint"),
        PropertyType::Int => sink.push_str("int"),
        PropertyType::Float => sink.push_str("float"),
        PropertyType::Float2 => sink.push_str("float2"),
        PropertyType::Float4 => sink.push_str("float4"),
    }
}

fn parse_vi_semantic(x: &Identifier) -> VertexInputSemantic {
    let l = x.as_str().to_uppercase();

    'try_parse: {
        if let Some(s) = l.strip_prefix("POSITION") {
            let index = if s.is_empty() {
                0
            } else if let Ok(x) = s.parse() {
                x
            } else {
                break 'try_parse;
            };

            return VertexInputSemantic::Position(index);
        }

        if let Some(s) = l.strip_prefix("NORMAL") {
            let index = if s.is_empty() {
                0
            } else if let Ok(x) = s.parse() {
                x
            } else {
                break 'try_parse;
            };

            return VertexInputSemantic::Normal(index);
        }

        if let Some(s) = l.strip_prefix("TANGENT") {
            let index = if s.is_empty() {
                0
            } else if let Ok(x) = s.parse() {
                x
            } else {
                break 'try_parse;
            };

            return VertexInputSemantic::Tangent(index);
        }

        if let Some(s) = l.strip_prefix("BINORMAL") {
            let index = if s.is_empty() {
                0
            } else if let Ok(x) = s.parse() {
                x
            } else {
                break 'try_parse;
            };

            return VertexInputSemantic::Binormal(index);
        }

        if let Some(s) = l.strip_prefix("TEXCOORD") {
            let index = if s.is_empty() {
                0
            } else if let Ok(x) = s.parse() {
                x
            } else {
                break 'try_parse;
            };

            return VertexInputSemantic::Texcoord(index);
        }

        if let Some(s) = l.strip_prefix("COLOR") {
            let index = if s.is_empty() {
                0
            } else if let Ok(x) = s.parse() {
                x
            } else {
                break 'try_parse;
            };

            return VertexInputSemantic::Color(index);
        }

        if let Some(s) = l.strip_prefix("MISC") {
            let index = if s.is_empty() {
                0
            } else if let Ok(x) = s.parse() {
                x
            } else {
                break 'try_parse;
            };

            return VertexInputSemantic::Misc(index);
        }
    }

    panic!("invalid semantic name: {}", x.as_str());
}

fn print_vi_semantic(s: &VertexInputSemantic, sink: &mut String) {
    match s {
        VertexInputSemantic::Position(index) => {
            sink.push_str("POSITION");
            sink.extend(index.to_string().chars());
        }
        VertexInputSemantic::Normal(index) => {
            sink.push_str("NORMAL");
            sink.extend(index.to_string().chars());
        }
        VertexInputSemantic::Tangent(index) => {
            sink.push_str("TANGENT");
            sink.extend(index.to_string().chars());
        }
        VertexInputSemantic::Binormal(index) => {
            sink.push_str("BINORMAL");
            sink.extend(index.to_string().chars());
        }
        VertexInputSemantic::Texcoord(index) => {
            sink.push_str("TEXCOORD");
            sink.extend(index.to_string().chars());
        }
        VertexInputSemantic::Color(index) => {
            sink.push_str("COLOR");
            sink.extend(index.to_string().chars());
        }
        VertexInputSemantic::Misc(index) => {
            sink.push_str("MISC");
            sink.extend(index.to_string().chars());
        }
    }
}

#[derive(Debug)]
pub enum Value {
    BuiltinTexture2DWhite,
    RGBA { r: f32, g: f32, b: f32, a: f32 },
    Tuple(Vec<Value>),
    Num(f32),
    Str(String),
}

fn fold_expr(x: syntax::Expression) -> Value {
    match x {
        syntax::Expression::NumLit(v) => Value::Num(v.as_str().parse().expect("invalid num")),
        syntax::Expression::StrLit(s) => Value::Str(s.as_str().into()),
        syntax::Expression::Wrapped(_, x, _) => fold_expr(*x),
        syntax::Expression::Tuple(_, xs, _) => {
            Value::Tuple(xs.into_iter().map(|x| fold_expr(x.0)).collect())
        }
        syntax::Expression::Use(_, s) if s.as_str() == "Texture2D.white" => {
            Value::BuiltinTexture2DWhite
        }
        syntax::Expression::Use(_, s) => panic!("unknown use: {:?}", s),
    }
}
