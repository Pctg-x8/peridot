use std::{borrow::Cow, collections::HashMap};

use crate::{PropertyDestinationVk, PropertyMappingVk, syntax};

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
                    }
                    impl Default for Attributes {
                        fn default() -> Self {
                            Self { immutable: false }
                        }
                    }

                    let mut attr = Attributes::default();
                    for p in ps.elements {
                        match p {
                            syntax::PropertiesBlockElement::Attribute(a) => {
                                if a.name.as_str() == "Immutable" {
                                    attr.immutable = true;
                                } else {
                                    panic!("unknown attribute: {:?}", a.name);
                                }
                            }
                            syntax::PropertiesBlockElement::Property(p) => {
                                let update_frequency = if attr.immutable {
                                    PropertyUpdateFrequency::Immutable
                                } else {
                                    PropertyUpdateFrequency::default()
                                };

                                properties.push(PropertyData {
                                    name: p.name.as_str().into(),
                                    r#type: Type::from_syntax(p.r#type),
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
                                                r#type: Type::from_syntax(ty),
                                                semantic_name: semantic_name.as_str().into(),
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
                        }
                    }

                    passes.insert(
                        name.as_str().into(),
                        PassData {
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
    pub fn gen_vk_prelude(&self) -> (String, HashMap<String, (Type, PropertyMappingVk)>) {
        let mut specialized_constants = Vec::<(Cow<str>, &Type)>::new();
        let mut combined_constants = Vec::new();
        let mut push_constant_block_members = Vec::new();
        let mut descriptor_sets = Vec::new();
        let mut realtime_buffer_members = Vec::new();
        let mut property_mapping = HashMap::new();

        for p in self.properties.iter() {
            match p.update_frequency {
                PropertyUpdateFrequency::Immutable => match p.r#type {
                    Type::Float2 => {
                        let r_dest =
                            PropertyDestinationVk::SpecConstant(specialized_constants.len());
                        specialized_constants
                            .push((format!("{name}_R", name = p.name).into(), &Type::Float));
                        let g_dest =
                            PropertyDestinationVk::SpecConstant(specialized_constants.len());
                        specialized_constants
                            .push((format!("{name}_G", name = p.name).into(), &Type::Float));
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
                    Type::Float4 | Type::RGB => {
                        let r_dest =
                            PropertyDestinationVk::SpecConstant(specialized_constants.len());
                        specialized_constants
                            .push((format!("{name}_R", name = p.name).into(), &Type::Float));
                        let g_dest =
                            PropertyDestinationVk::SpecConstant(specialized_constants.len());
                        specialized_constants
                            .push((format!("{name}_G", name = p.name).into(), &Type::Float));
                        let b_dest =
                            PropertyDestinationVk::SpecConstant(specialized_constants.len());
                        specialized_constants
                            .push((format!("{name}_B", name = p.name).into(), &Type::Float));
                        let a_dest =
                            PropertyDestinationVk::SpecConstant(specialized_constants.len());
                        specialized_constants
                            .push((format!("{name}_A", name = p.name).into(), &Type::Float));
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
                                    PropertyDestinationVk::PushConstantBlockOffset(
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
                                PropertyMappingVk::Direct(
                                    PropertyDestinationVk::RealtimeBufferOffset(
                                        realtime_buffer_members.len() - 1,
                                    ),
                                ),
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

        // material prelude
        code.push_str("namespace PeridotMaterialParameters {\n");
        for (n, (name, ty)) in specialized_constants.into_iter().enumerate() {
            code.push_str("[vk::constant_id(");
            code.push_str(&n.to_string());
            code.push_str(")]\nconst ");
            ty.print(&mut code);
            code.push_str(" ");
            code.push_str(&name);
            code.push_str(" = 0;\n");
        }

        for x in combined_constants {
            code.push_str(&x);
            code.push_str("\n");
        }

        if !push_constant_block_members.is_empty() {
            code.push_str("struct PerDraw {\n");
            for (name, ty) in push_constant_block_members {
                code.push_str("    ");
                ty.print(&mut code);
                code.push_str(" ");
                code.push_str(name);
                code.push_str(";\n");
            }
            code.push_str("}\n[vk::push_constant]\nPerDraw perDraw;\n");
        }

        let realtime_buffer_binding_index = descriptor_sets.len();
        for (n, (name, ty)) in descriptor_sets.into_iter().enumerate() {
            // set indexはあとで調整(直接うめこみたくはないな......)
            code.push_str("[vk::binding(");
            code.push_str(&n.to_string());
            code.push_str(", 2)]\n");
            ty.print(&mut code);
            code.push_str(" ");
            code.push_str(name);
            code.push_str(";\n");
        }

        if !realtime_buffer_members.is_empty() {
            code.push_str("struct RealtimeBuffer {\n");
            for (name, ty) in realtime_buffer_members {
                code.push_str("    ");
                ty.print(&mut code);
                code.push_str(" ");
                code.push_str(name);
                code.push_str(";\n");
            }
            code.push_str("}\n[vk::binding(");
            code.push_str(&realtime_buffer_binding_index.to_string());
            code.push_str(", 2)]\nRealtimeBuffer realtime;\n");
        }

        code.push_str("}\n");
        (code, property_mapping)
    }

    pub fn gen_vk_code_for_pass(&self, name: &str) -> (String, HashMap<String, usize>) {
        let Some(p) = self.passes.get(name) else {
            panic!("no pass: {name}");
        };

        let mut semantic_to_location_map = HashMap::with_capacity(p.vertex_bindings.len());

        let mut code = String::new();
        if let Some(ref d) = p.deriving {
            eprintln!("todo: deriving: {d}");
        }
        if !p.vertex_bindings.is_empty() {
            code.push_str("struct Vertex {\n");
            for (n, vb) in p.vertex_bindings.iter().enumerate() {
                match semantic_to_location_map.entry(vb.semantic_name.clone()) {
                    std::collections::hash_map::Entry::Vacant(x) => {
                        x.insert(n);
                    }
                    std::collections::hash_map::Entry::Occupied(x) => {
                        panic!(
                            "conflicting vertex semantic {} with location {}",
                            x.key(),
                            x.get()
                        );
                    }
                }

                code.push_str("    [vk::location(");
                code.push_str(&n.to_string());
                code.push_str(")]\n    ");
                vb.r#type.print(&mut code);
                code.push_str(" ");
                code.push_str(&vb.name);
                code.push_str(" : ");
                code.push_str(&vb.semantic_name);
                code.push_str(";\n");
            }
            code.push_str("}\n\n");
        }
        if let Some(ref s) = p.shader_code {
            code.push_str(s);
        }

        (code, semantic_to_location_map)
    }
}

#[derive(Debug)]
pub struct PropertyData {
    pub name: String,
    pub r#type: Type,
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
    pub deriving: Option<String>,
    pub vertex_bindings: Vec<PassVertexBindingData>,
    pub shader_code: Option<String>,
}

#[derive(Debug)]
pub struct PassVertexBindingData {
    pub name: String,
    pub r#type: Type,
    pub semantic_name: String,
}

#[derive(Debug, Clone)]
pub enum Type {
    Texture2D,
    RGB,
    Float,
    Float2,
    Float4,
}
impl Type {
    fn from_syntax(x: syntax::Type) -> Self {
        match x {
            syntax::Type::Texture2D(_) => Self::Texture2D,
            syntax::Type::RGB(_) => Self::RGB,
            syntax::Type::Float2(_) => Self::Float2,
            syntax::Type::Float4(_) => Self::Float4,
        }
    }

    fn print(&self, sink: &mut String) {
        match self {
            // Texture: treated as combined image sampler
            Self::Texture2D => sink.push_str("Sampler2D"),
            Self::RGB => sink.push_str("float4"),
            Self::Float => sink.push_str("float"),
            Self::Float2 => sink.push_str("float2"),
            Self::Float4 => sink.push_str("float4"),
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
