use std::{borrow::Cow, collections::HashMap};

use crate::syntax;

#[derive(Debug)]
pub struct RenderingConfiguration {
    pub properties: HashMap<String, PropertyData>,
    pub passes: HashMap<String, PassData>,
}
impl RenderingConfiguration {
    pub fn new(s: Vec<syntax::ToplevelElement>) -> Self {
        let mut properties = HashMap::new();
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

                                properties.insert(
                                    p.name.as_str().into(),
                                    PropertyData {
                                        r#type: Type::from_syntax(p.r#type),
                                        default: fold_expr(p.default),
                                        update_frequency,
                                    },
                                );
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
                            vertex_bindings: None,
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

                                let mut map = HashMap::new();
                                for (define_order, (name, _, ty, _, semantic_name, _)) in
                                    entries.into_iter().enumerate()
                                {
                                    match map.entry(name.as_str().into()) {
                                        std::collections::hash_map::Entry::Vacant(e) => {
                                            e.insert(PassVertexBindingData {
                                                define_order,
                                                r#type: Type::from_syntax(ty),
                                                semantic_name: semantic_name.as_str().into(),
                                            });
                                        }
                                        std::collections::hash_map::Entry::Occupied(_) => {
                                            panic!("duplicate vertex binding name: {:?}", name)
                                        }
                                    }
                                }

                                vertex_bindings = Some(map);
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
                            vertex_bindings,
                            shader_code,
                        },
                    );
                }
            }
        }

        Self { properties, passes }
    }

    // いったんターゲットをVulkanに限定する(他API対応もでてきたらそのときに対応する)
    pub fn gen_vk_prelude(&self) -> String {
        let mut specialized_constants = Vec::<(Cow<str>, &Type)>::new();
        let mut combined_constants = Vec::new();
        let mut push_constant_block_members = Vec::new();
        let mut descriptor_sets = Vec::new();
        let mut realtime_buffer_members = Vec::new();

        for (name, props) in self.properties.iter() {
            match props.update_frequency {
                PropertyUpdateFrequency::Immutable => match props.r#type {
                    Type::Float2 => {
                        specialized_constants.push((format!("{name}_R").into(), &Type::Float));
                        specialized_constants.push((format!("{name}_G").into(), &Type::Float));
                        combined_constants.push(format!(
                            "static const float2 {name} = float2({name}_R, {name}_G);"
                        ));
                    }
                    Type::Float4 | Type::RGB => {
                        specialized_constants.push((format!("{name}_R").into(), &Type::Float));
                        specialized_constants.push((format!("{name}_G").into(), &Type::Float));
                        specialized_constants.push((format!("{name}_B").into(), &Type::Float));
                        specialized_constants.push((format!("{name}_A").into(), &Type::Float));
                        combined_constants.push(format!(
                            "static const float4 {name} = float4({name}_R, {name}_G, {name}_B, {name}_A);"
                        ));
                    }
                    _ => {
                        specialized_constants.push((name.into(), &props.r#type));
                    }
                },
                PropertyUpdateFrequency::PerDrawCall => {
                    push_constant_block_members.push((name, &props.r#type));
                }
                PropertyUpdateFrequency::Dynamic => {
                    descriptor_sets.push((name, &props.r#type));
                }
                PropertyUpdateFrequency::Realtime => {
                    realtime_buffer_members.push((name, &props.r#type));
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
        code
    }

    pub fn gen_code_for_pass(&self, name: &str) -> String {
        let Some(p) = self.passes.get(name) else {
            panic!("no pass: {name}");
        };

        let mut code = String::new();
        if let Some(ref d) = p.deriving {
            eprintln!("todo: deriving: {d}");
        }
        if let Some(ref v) = p.vertex_bindings {
            let mut defs = v.iter().collect::<Vec<_>>();
            defs.sort_by_key(|a| &a.1.define_order);

            code.push_str("struct Vertex {\n");
            for (name, props) in defs {
                code.push_str("    ");
                props.r#type.print(&mut code);
                code.push_str(" ");
                code.push_str(name);
                code.push_str(" : ");
                code.push_str(&props.semantic_name);
                code.push_str(";\n");
            }
            code.push_str("}\n\n");
        }
        if let Some(ref s) = p.shader_code {
            code.push_str(s);
        }

        code
    }
}

#[derive(Debug)]
pub struct PropertyData {
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
    pub vertex_bindings: Option<HashMap<String, PassVertexBindingData>>,
    pub shader_code: Option<String>,
}

#[derive(Debug)]
pub struct PassVertexBindingData {
    pub define_order: usize,
    pub r#type: Type,
    pub semantic_name: String,
}

#[derive(Debug)]
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
