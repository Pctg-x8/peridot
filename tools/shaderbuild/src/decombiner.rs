//! Combined Shader Decombiner

use bedrock as br;
use regex::Regex;
use std::collections::BTreeMap;
use std::mem::{align_of, size_of};
use std::str::FromStr;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeclarationOps {
    VertexInput,
    VertexShader,
    FragmentShader,
    Varyings,
    SpecConstant,
    Uniform,
    PushConstant,
    Header,
    SamplerBuffer,
    Sampler(usize),
    SamplerCube,
}
impl DeclarationOps {
    fn parse(tok: &mut Tokenizer) -> Option<Self> {
        match tok.strip_ignores().strip_ident() {
            Some("FragmentShader") => Some(DeclarationOps::FragmentShader),
            Some("SamplerBuffer") => Some(DeclarationOps::SamplerBuffer),
            Some("SpecConstant") => Some(DeclarationOps::SpecConstant),
            Some("VertexShader") => Some(DeclarationOps::VertexShader),
            Some("PushConstant") => Some(DeclarationOps::PushConstant),
            Some("VertexInput") => Some(DeclarationOps::VertexInput),
            Some("Sampler1D") => Some(DeclarationOps::Sampler(1)),
            Some("Sampler2D") => Some(DeclarationOps::Sampler(2)),
            Some("Sampler3D") => Some(DeclarationOps::Sampler(3)),
            Some("SamplerCube") => Some(DeclarationOps::SamplerCube),
            Some("Varyings") => Some(DeclarationOps::Varyings),
            Some("Uniform") => Some(DeclarationOps::Uniform),
            Some("Header") => Some(DeclarationOps::Header),
            _ => None,
        }
    }
}

pub type ParseResult<T> = Result<T, ()>;

trait CharIterationExt: Iterator<Item = char> + Sized {
    fn count_with_bytes(self) -> (usize, usize) {
        self.fold((0, 0), |(c, b), cc| (c + 1, b + cc.len_utf8()))
    }
    fn count_with_bytes_while<P: Fn(char) -> bool>(self, pred: P) -> (usize, usize) {
        self.take_while(|&c| pred(c))
            .fold((0, 0), |(c, b), cc| (c + 1, b + cc.len_utf8()))
    }
}
impl<I: Iterator<Item = char>> CharIterationExt for I {}

pub struct Tokenizer<'s>(&'s str);
impl<'s> Tokenizer<'s> {
    pub fn new(s: &'s str) -> Self {
        Tokenizer(s)
    }

    fn strip_comment(&mut self) -> bool {
        if self.strip_prefix("//") {
            let (_, strip_bytes) = self.0.chars().count_with_bytes_while(|c| c != '\n');
            self.0 = &self.0[strip_bytes..];
            return true;
        } else {
            false
        }
    }
    fn strip_ignores(&mut self) -> &mut Self {
        while self.strip_prefix(" ")
            || self.strip_prefix("\n")
            || self.strip_prefix("\r")
            || self.strip_prefix("\t")
            || self.strip_comment()
        {}

        self
    }

    fn strip_prefix(&mut self, p: &str) -> bool {
        if self.0.starts_with(p) {
            self.0 = &self.0[p.len()..];
            true
        } else {
            false
        }
    }
    fn strip_ident(&mut self) -> Option<&'s str> {
        if self.0.starts_with(|c: char| c.is_digit(10)) {
            return None;
        }
        let (_, bytes) = self
            .0
            .chars()
            .count_with_bytes_while(|c| c.is_alphanumeric() || c == '_');
        if bytes == 0 {
            return None;
        }
        let slice = &self.0[..bytes];
        self.0 = &self.0[bytes..];
        trace!("Tokenizer::strip_ident: {}", slice);

        Some(slice)
    }
    pub fn ident_list(&mut self) -> Vec<&'s str> {
        self.strip_ignores();
        let mut v = if let Some(id) = self.strip_ident() {
            vec![id]
        } else {
            return Vec::new();
        };
        while self.strip_ignores().strip_prefix(",") {
            if let Some(id) = self.strip_ignores().strip_ident() {
                v.push(id);
            } else {
                break;
            }
        }

        v
    }

    /// : ...
    pub fn glsl_type_ascription(&mut self) -> ParseResult<&'s str> {
        self.strip_ignores();
        if !self.strip_prefix(":") {
            Err(())
        } else {
            self.glsl_represents_until_decl_end()
        }
    }
    /// : ... <term_char>
    pub fn glsl_type_ascription_until(&mut self, term_chars: &[char]) -> ParseResult<&'s str> {
        self.strip_ignores();
        if !self.strip_prefix(":") {
            Err(())
        } else {
            self.glsl_represents_until(term_chars)
        }
    }
    pub fn glsl_represents_until_decl_end(&mut self) -> ParseResult<&'s str> {
        self.strip_ignores();
        let (_, glsl_strip_bytes) = self
            .0
            .chars()
            .count_with_bytes_while(|c| c != ';' && c != '@');
        if glsl_strip_bytes == 0 {
            return Err(());
        }
        let glsl_strip = &self.0[..glsl_strip_bytes];
        self.0 = &self.0[glsl_strip_bytes..];

        Ok(glsl_strip)
    }
    pub fn glsl_represents_until(&mut self, term_chars: &[char]) -> ParseResult<&'s str> {
        self.strip_ignores();
        let (_, glsl_strip_bytes) = self
            .0
            .chars()
            .count_with_bytes_while(|c| term_chars.iter().all(|&cc| cc != c));
        if glsl_strip_bytes == 0 {
            return Err(());
        }
        let glsl_strip = &self.0[..glsl_strip_bytes];
        self.0 = &self.0[glsl_strip_bytes..];

        Ok(glsl_strip)
    }

    pub fn index_number(&mut self) -> ParseResult<usize> {
        self.strip_ignores();
        let (_, num_bytes) = self.0.chars().count_with_bytes_while(|c| c.is_digit(10));
        let n = usize::from_str(&self.0[..num_bytes]).map_err(drop)?;
        self.0 = &self.0[num_bytes..];

        Ok(n)
    }

    pub fn shader_stage(&mut self) -> ParseResult<br::ShaderStage> {
        self.strip_ignores();
        if self.strip_prefix("FragmentShader") {
            return Ok(br::ShaderStage::FRAGMENT);
        }
        if self.strip_prefix("VertexShader") {
            return Ok(br::ShaderStage::VERTEX);
        }
        return Err(());
    }
    pub fn bracketed_stage(&mut self) -> ParseResult<br::ShaderStage> {
        self.strip_ignores();
        if !self.bracket_start() {
            return Err(());
        }
        let st = self.shader_stage()?;
        if !self.bracket_end() {
            return Err(());
        }
        return Ok(st);
    }
    pub fn codeblock(&mut self) -> ParseResult<&'s str> {
        self.strip_ignores();
        if !self.block_start() {
            return Err(());
        }

        // 上位に押し上げてエラー処理した方がスタックトレースが見やすいのでResultで返す
        fn strip_bytes_counter<I: Iterator<Item = char>>(
            mut c: I,
            current: usize,
            nestlevel: usize,
        ) -> ParseResult<usize> {
            match c.next() {
                Some(cc @ '{') => strip_bytes_counter(c, current + cc.len_utf8(), nestlevel + 1),
                Some(cc @ '}') => {
                    if nestlevel == 0 {
                        Ok(current)
                    } else {
                        strip_bytes_counter(c, current + cc.len_utf8(), nestlevel - 1)
                    }
                }
                Some(cc) => strip_bytes_counter(c, current + cc.len_utf8(), nestlevel),
                None => Err(()),
            }
        }
        let cb_slice_bytes =
            strip_bytes_counter(self.0.chars(), 0, 0).expect("Missing closing brace");
        let cb_slice = &self.0[..cb_slice_bytes];
        self.0 = &self.0[cb_slice_bytes + 1..];

        Ok(cb_slice)
    }
    fn bindrate(&mut self) -> br::vk::VkVertexInputRate {
        if self.bracket_start() {
            let irate = match self
                .strip_ignores()
                .strip_ident()
                .expect("Expect either PerInstance or PerVertex")
            {
                "PerInstance" => br::vk::VK_VERTEX_INPUT_RATE_INSTANCE,
                "PerVertex" => br::vk::VK_VERTEX_INPUT_RATE_VERTEX,
                x => panic!("Expect either PerInstance or PerVertex. Found {:?}", x),
            };
            if !self.bracket_end() {
                panic!("Missing ]");
            }

            irate
        } else {
            br::vk::VK_VERTEX_INPUT_RATE_VERTEX
        }
    }
    /// `Binding` <IndexNumber> <BindRate>
    pub fn binding(&mut self) -> ParseResult<(usize, br::vk::VkVertexInputRate)> {
        self.strip_ignores().expect_strip_prefix("Binding")?;
        let index = self.index_number()?;
        let irate = self.bindrate();

        Ok((index, irate))
    }
    /// `SpecConstant` v <BracketedStage> `(` <IndexNumber> `)`
    pub fn spec_constant_header_rest(&mut self) -> ParseResult<(br::ShaderStage, usize)> {
        self.strip_ignores();
        let stg = self.bracketed_stage()?;
        if !self.strip_ignores().strip_prefix("(") {
            return Err(());
        }
        let idx = self.index_number()?;
        if !self.strip_ignores().strip_prefix(")") {
            return Err(());
        }

        Ok((stg, idx))
    }
    /// (`Uniform`/`SamplerBuffer`) v <BracketedStage> `(` <IndexNumber> `,` <IndexNumber> `)`
    pub fn resource_header_rest(&mut self) -> ParseResult<(br::ShaderStage, usize, usize)> {
        self.strip_ignores();
        let stg = self.bracketed_stage()?;
        self.strip_ignores().expect_strip_prefix("(")?;
        let set = self.index_number()?;
        self.strip_ignores().expect_strip_prefix(",")?;
        let binding = self.index_number()?;
        self.strip_ignores().expect_strip_prefix(")")?;

        Ok((stg, set, binding))
    }
    /// `PushConstant` v <BracketStage>
    pub fn push_constant_header_rest(&mut self) -> ParseResult<br::ShaderStage> {
        self.strip_ignores().bracketed_stage()
    }
    /// `Header` v <BracketStage>
    pub fn rawcode_header_rest(&mut self) -> ParseResult<br::ShaderStage> {
        self.strip_ignores().bracketed_stage()
    }

    fn expect_block_start(&mut self) -> ParseResult<()> {
        self.strip_ignores().expect_strip_prefix("{")
    }
    pub fn block_start(&mut self) -> bool {
        self.strip_ignores();
        self.strip_prefix("{")
    }
    pub fn block_end(&mut self) -> bool {
        self.strip_ignores();
        self.strip_prefix("}")
    }
    pub fn bracket_start(&mut self) -> bool {
        self.strip_ignores();
        self.strip_prefix("[")
    }
    pub fn bracket_end(&mut self) -> bool {
        self.strip_ignores();
        self.strip_prefix("]")
    }
    pub fn declaration_end(&mut self) -> bool {
        self.strip_ignores().strip_prefix(";")
    }
    pub fn arrow(&mut self) -> bool {
        self.strip_ignores().strip_prefix("->")
    }

    pub fn no_chars(&self) -> bool {
        self.0.is_empty()
    }
    fn expect_strip_prefix(&mut self, prefix: &str) -> ParseResult<()> {
        if !self.strip_prefix(prefix) {
            Err(())
        } else {
            Ok(())
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Variable<'s> {
    name: &'s str,
    type_str: &'s str,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BindingBlock<'s> {
    rate: br::vk::VkVertexInputRate,
    vars: Vec<Variable<'s>>,
}
impl<'s> BindingBlock<'s> {
    fn packed_size(&self) -> usize {
        let mut total = 0;
        for &Variable { type_str, .. } in &self.vars {
            match type_str {
                "vec4" => {
                    let offs = align2(total, align_of::<[f32; 4]>());
                    total = offs + size_of::<[f32; 4]>();
                }
                "vec3" => {
                    let offs = align2(total, align_of::<f32>());
                    total = offs + size_of::<[f32; 3]>();
                }
                "vec2" => {
                    let offs = align2(total, align_of::<[f32; 2]>());
                    total = offs + size_of::<[f32; 2]>();
                }
                "float" => {
                    let offs = align2(total, align_of::<f32>());
                    total = offs + size_of::<f32>();
                }
                "uint" => {
                    let offs = align2(total, align_of::<u32>());
                    total = offs + size_of::<u32>();
                }
                "int" => {
                    let offs = align2(total, align_of::<u32>());
                    total = offs + size_of::<u32>();
                }
                "ivec4" => {
                    let offset = align2(total, align_of::<[i32; 4]>());
                    total = offset + size_of::<[i32; 4]>();
                }
                "i8vec4" => {
                    let offset = align2(total, align_of::<[i8; 4]>());
                    total = offset + size_of::<[i8; 4]>();
                }
                "u8vec4" => {
                    let offset = align2(total, align_of::<[u8; 4]>());
                    total = offset + size_of::<[u8; 4]>();
                }
                _ => warn!("Unimplement: Unable to determine exact packed size"),
            }
        }
        return total;
    }
}

fn align2(x: usize, a: usize) -> usize {
    (x + (a - 1)) & !(a - 1)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ToplevelBlock<'s> {
    VertexInput(Vec<(usize, BindingBlock<'s>)>),
    ShaderCode(br::ShaderStage, &'s str),
    Varying(br::ShaderStage, br::ShaderStage, Vec<Variable<'s>>),
    SpecConstant(br::ShaderStage, usize, &'s str, &'s str, &'s str),
    Uniform(br::ShaderStage, usize, usize, &'s str, &'s str),
    SamplerBuffer(br::ShaderStage, usize, usize, &'s str),
    Sampler(br::ShaderStage, usize, usize, usize, &'s str),
    SamplerCube(br::ShaderStage, usize, usize, &'s str),
    PushConstant(br::ShaderStage, &'s str, &'s str),
    Header(br::ShaderStage, &'s str),
}
impl<'s> Tokenizer<'s> {
    pub fn binding_block(&mut self) -> ParseResult<(usize, BindingBlock<'s>)> {
        let (index, rate) = self.binding()?;
        self.expect_block_start()?;
        let mut vars = Vec::new();
        while !self.block_end() {
            let idents = self.ident_list();
            if idents.is_empty() {
                panic!("No declaration found");
            }
            let glsl_type_str = self.glsl_type_ascription().expect("Type required(binding)");
            vars.extend(idents.into_iter().map(|name| Variable {
                name,
                type_str: glsl_type_str,
            }));
            if !self.declaration_end() {
                panic!("; required at end of each declaration");
            }
        }

        Ok((index, BindingBlock { rate, vars }))
    }
    pub fn vertex_input_block(&mut self) -> Vec<(usize, BindingBlock<'s>)> {
        if self.expect_block_start().is_err() {
            return Vec::new();
        }
        let mut bindings = Vec::new();
        while !self.block_end() {
            bindings.push(self.binding_block().expect("Binding block required"));
        }

        bindings
    }

    pub fn varying(&mut self) -> (br::ShaderStage, br::ShaderStage, Vec<Variable<'s>>) {
        let src = self.shader_stage().expect("Source ShaderStage required");
        if !self.arrow() {
            panic!("Arrow required");
        }
        let dst = self
            .shader_stage()
            .expect("Destination ShaderStage required");
        if !self.block_start() {
            panic!("Variable Block required");
        }
        let mut vars = Vec::new();
        while !self.block_end() {
            let idents = self.ident_list();
            if idents.is_empty() {
                panic!("No declaration found");
            }
            let glsl_type_str = self.glsl_type_ascription().expect("Type required(varying)");
            vars.extend(idents.into_iter().map(|name| Variable {
                name,
                type_str: glsl_type_str,
            }));
            if !self.declaration_end() {
                panic!("; required at end of each declaration");
            }
        }

        (src, dst, vars)
    }
    pub fn spec_constant(&mut self) -> ToplevelBlock<'s> {
        let (stg, idx) = self
            .spec_constant_header_rest()
            .expect("ShaderStage then ConstantID required");
        let id = self
            .strip_ignores()
            .strip_ident()
            .expect("Identifier required");
        let ty = self
            .strip_ignores()
            .glsl_type_ascription_until(&['='])
            .expect("Type required for SpecConstant");
        if !self.strip_ignores().strip_prefix("=") {
            panic!("Equal required");
        }
        let init = self
            .glsl_represents_until_decl_end()
            .expect("Initial value required");
        if !self.strip_ignores().declaration_end() {
            panic!("; required at the end of each declaration");
        }

        ToplevelBlock::SpecConstant(stg, idx, id, ty, init)
    }
    pub fn uniform(&mut self) -> ToplevelBlock<'s> {
        let (stg, set, binding) = self
            .resource_header_rest()
            .expect("ShaderStage then pair of descriptor set and binding indices are required");
        let id = self
            .strip_ignores()
            .strip_ident()
            .expect("Identifier for GLSL TypeName required");
        let code = self.codeblock().expect("GLSL CodeBlock required");

        ToplevelBlock::Uniform(stg, set, binding, id, code)
    }
    pub fn sampler_buffer(&mut self) -> ToplevelBlock<'s> {
        let (stg, set, binding) = self
            .resource_header_rest()
            .expect("ShaderStage then pair of descriptor set and binding indices required");
        let id = self
            .strip_ignores()
            .strip_ident()
            .expect("Identifier for GLSL VariableName required");

        ToplevelBlock::SamplerBuffer(stg, set, binding, id)
    }
    pub fn sampler(&mut self, dim: usize) -> ToplevelBlock<'s> {
        let (stg, set, binding) = self
            .resource_header_rest()
            .expect("ShaderStage then pair of descriptor set and binding indices required");
        let id = self
            .strip_ignores()
            .strip_ident()
            .expect("Identifier for GLSL VariableName required");

        ToplevelBlock::Sampler(stg, set, binding, dim, id)
    }

    pub fn sampler_cube(&mut self) -> ToplevelBlock<'s> {
        let (stg, set, binding) = self
            .resource_header_rest()
            .expect("ShaderStage the pair of descriptor set and binding indices required");
        let id = self
            .strip_ignores()
            .strip_ident()
            .expect("Identifier for GLSL VariableName required");

        ToplevelBlock::SamplerCube(stg, set, binding, id)
    }

    pub fn push_constant(&mut self) -> ToplevelBlock<'s> {
        let stg = self
            .push_constant_header_rest()
            .expect("ShaderStage is required");
        let id = self
            .strip_ignores()
            .strip_ident()
            .expect("Identifier for PushConstant TypeName required");
        let code = self.codeblock().expect("GLSL CodeBlock required");

        ToplevelBlock::PushConstant(stg, id, code)
    }
    pub fn rawcode_header(&mut self) -> ToplevelBlock<'s> {
        // Header <BracketedStage> v <Codeblock>
        let stg = self.rawcode_header_rest().expect("ShaderStage required");
        let code = self.codeblock().expect("GLSL CodeBlock required");

        ToplevelBlock::Header(stg, code)
    }

    pub fn toplevel_block(&mut self) -> ToplevelBlock<'s> {
        match DeclarationOps::parse(self).expect("Requires Toplevel Declaration") {
            DeclarationOps::VertexInput => {
                let vi = self.vertex_input_block();
                if vi.is_empty() {
                    panic!("Empty VertexInput is not allowed");
                } else {
                    ToplevelBlock::VertexInput(vi)
                }
            }
            DeclarationOps::VertexShader => ToplevelBlock::ShaderCode(
                br::ShaderStage::VERTEX,
                self.codeblock().expect("GLSL CodeBlock required"),
            ),
            DeclarationOps::FragmentShader => ToplevelBlock::ShaderCode(
                br::ShaderStage::FRAGMENT,
                self.codeblock().expect("GLSL CodeBlock required"),
            ),
            DeclarationOps::Varyings => {
                let (src, dst, vars) = self.varying();
                ToplevelBlock::Varying(src, dst, vars)
            }
            DeclarationOps::SpecConstant => self.spec_constant(),
            DeclarationOps::Uniform => self.uniform(),
            DeclarationOps::PushConstant => self.push_constant(),
            DeclarationOps::Header => self.rawcode_header(),
            DeclarationOps::SamplerBuffer => self.sampler_buffer(),
            DeclarationOps::Sampler(d) => self.sampler(d),
            DeclarationOps::SamplerCube => self.sampler_cube(),
        }
    }

    pub fn toplevel_blocks(&mut self) -> Vec<ToplevelBlock<'s>> {
        let mut blocks = Vec::new();
        while !self.strip_ignores().no_chars() {
            blocks.push(self.toplevel_block());
        }

        blocks
    }
}

pub type GlslType<'s> = &'s str;

#[derive(Debug, Clone)]
pub enum DescriptorBoundResource<'s> {
    Uniform {
        struct_name: &'s str,
        member_code: &'s str,
    },
    SamplerBuffer(&'s str),
    Sampler(usize, &'s str),
    SamplerCube(&'s str),
}
#[derive(Debug, Clone)]
pub struct CombinedShader<'s> {
    vertex_input: Vec<(usize, BindingBlock<'s>)>,
    header_codes: BTreeMap<br::ShaderStage, Vec<&'s str>>,
    vertex_shader_code: &'s str,
    fragment_shader_code: Option<&'s str>,
    varyings_between_shaders: Vec<(br::ShaderStage, br::ShaderStage, Vec<Variable<'s>>)>,
    spec_constants_per_stage:
        BTreeMap<br::ShaderStage, BTreeMap<usize, (&'s str, GlslType<'s>, &'s str)>>,
    resources_per_stage:
        BTreeMap<br::ShaderStage, BTreeMap<(usize, usize), DescriptorBoundResource<'s>>>,
    push_constant_per_stage: BTreeMap<br::ShaderStage, (&'s str, &'s str)>,
}
impl<'s> CombinedShader<'s> {
    pub fn from_parsed_blocks(blocks: Vec<ToplevelBlock<'s>>) -> Self {
        let mut cs = CombinedShader {
            vertex_input: Vec::new(),
            header_codes: BTreeMap::new(),
            vertex_shader_code: "",
            fragment_shader_code: None,
            varyings_between_shaders: Vec::new(),
            spec_constants_per_stage: BTreeMap::new(),
            resources_per_stage: BTreeMap::new(),
            push_constant_per_stage: BTreeMap::new(),
        };

        for tb in blocks {
            match tb {
                ToplevelBlock::VertexInput(mut bindings) => cs.vertex_input.append(&mut bindings),
                ToplevelBlock::ShaderCode(br::ShaderStage::VERTEX, c) => {
                    if cs.vertex_shader_code.is_empty() {
                        cs.vertex_shader_code = c;
                    } else {
                        panic!("Multiple Vertex Shader code is not allowed");
                    }
                }
                ToplevelBlock::ShaderCode(br::ShaderStage::FRAGMENT, c) => {
                    if cs.fragment_shader_code.is_none() {
                        cs.fragment_shader_code = Some(c);
                    } else {
                        panic!("Multiple Fragment Shader code is not allowed");
                    }
                }
                ToplevelBlock::ShaderCode(ss, _) => {
                    panic!("Unsupported Shader Stage: {:08b}", ss.0)
                }
                ToplevelBlock::Varying(src, dst, vars) => {
                    cs.varyings_between_shaders.push((src, dst, vars))
                }
                ToplevelBlock::SpecConstant(stg, idx, name, ty, init) => {
                    let storage = cs
                        .spec_constants_per_stage
                        .entry(stg)
                        .or_insert_with(BTreeMap::new);
                    if storage.contains_key(&idx) {
                        panic!("Multiple Definitions of SpecConstant for same id in same stage");
                    }
                    storage.insert(idx, (name, ty, init));
                }
                ToplevelBlock::Uniform(stg, set, binding, name, init) => {
                    let storage = cs
                        .resources_per_stage
                        .entry(stg)
                        .or_insert_with(BTreeMap::new);
                    if storage.contains_key(&(set, binding)) {
                        panic!("Multiple Definitions of the Resource for same (set, binding) in same stage");
                    }
                    storage.insert(
                        (set, binding),
                        DescriptorBoundResource::Uniform {
                            struct_name: name,
                            member_code: init,
                        },
                    );
                }
                ToplevelBlock::Sampler(stg, set, binding, dim, name) => {
                    let storage = cs
                        .resources_per_stage
                        .entry(stg)
                        .or_insert_with(BTreeMap::new);
                    if storage.contains_key(&(set, binding)) {
                        panic!("Multiple Definitions of the Resource for same (set, binding) in same stage");
                    }
                    storage.insert((set, binding), DescriptorBoundResource::Sampler(dim, name));
                }
                ToplevelBlock::SamplerCube(stg, set, binding, name) => {
                    let storage = cs
                        .resources_per_stage
                        .entry(stg)
                        .or_insert_with(BTreeMap::new);
                    if storage.contains_key(&(set, binding)) {
                        panic!("Multiple Definitions of the Resource for same (set, binding) in same stage");
                    }

                    storage.insert((set, binding), DescriptorBoundResource::SamplerCube(name));
                }
                ToplevelBlock::SamplerBuffer(stg, set, binding, name) => {
                    let storage = cs
                        .resources_per_stage
                        .entry(stg)
                        .or_insert_with(BTreeMap::new);
                    if storage.contains_key(&(set, binding)) {
                        panic!("Multiple Definitions of the Resource for same (set, binding) in same stage");
                    }
                    storage.insert((set, binding), DescriptorBoundResource::SamplerBuffer(name));
                }
                ToplevelBlock::PushConstant(stg, name, members) => {
                    if cs.push_constant_per_stage.contains_key(&stg) {
                        panic!("Multiple Definitions of PushConstants for same shader stage");
                    }
                    cs.push_constant_per_stage.insert(stg, (name, members));
                }
                ToplevelBlock::Header(stg, code) => {
                    cs.header_codes
                        .entry(stg)
                        .or_insert_with(Vec::new)
                        .push(code);
                }
            }
        }
        if cs.vertex_shader_code.is_empty() {
            panic!("VertexShader is not defined");
        }
        return cs;
    }

    pub fn is_provided_fsh(&self) -> bool {
        self.fragment_shader_code.is_some()
    }

    pub fn translate_vtype(s: &str) -> &str {
        match s {
            "i8vec4" => "ivec4",
            "u8vec4" => "uvec4",
            v => v,
        }
    }
    pub fn emit_vertex_shader(&self) -> String {
        let mut code = String::from("#version 450\n\n");

        // 入力変数(vertex_inputから)
        for (n, vi_vars) in self
            .vertex_input
            .iter()
            .flat_map(|&(_, ref bb)| &bb.vars)
            .enumerate()
        {
            code += &format!(
                "layout(location = {}) in {} {};\n",
                n,
                Self::translate_vtype(vi_vars.type_str),
                vi_vars.name
            );
        }
        // 出力変数
        for (n, ovar) in self
            .varyings_between_shaders
            .iter()
            .filter(|&&(src, _, _)| src == br::ShaderStage::VERTEX)
            .flat_map(|&(_, _, ref v)| v)
            .enumerate()
        {
            let require_flat = ovar.type_str == "int";
            code += &format!(
                "layout(location = {}) {}out {} {};\n",
                n,
                if require_flat { "flat " } else { "" },
                Self::translate_vtype(ovar.type_str),
                ovar.name
            );
        }
        // gl_Positionの宣言を追加
        if self.vertex_shader_code.contains("RasterPosition") {
            code += "out gl_PerVertex { out vec4 gl_Position; };\n";
        }
        // 定数(uniformとspecconstantとpushconstant)
        if let Some(cons) = self.spec_constants_per_stage.get(&br::ShaderStage::VERTEX) {
            for (id, &(name, ty, init)) in cons.iter() {
                code += &format!(
                    "layout(constant_id = {}) const {} {} = {};\n",
                    id, ty, name, init
                );
            }
        }
        self.emit_resource_bindings(&mut code, br::ShaderStage::VERTEX);
        if let Some(&(name, cb)) = self.push_constant_per_stage.get(&br::ShaderStage::VERTEX) {
            code += &format!("layout(push_constant) uniform {} {{{}}};\n", name, cb);
        }
        code += "\n";
        // header
        if let Some(cs) = self.header_codes.get(&br::ShaderStage::VERTEX) {
            for c in cs {
                code += c;
            }
            code += "\n";
        }
        // main
        code += &format!(
            "void main() {{{}}}",
            self.vertex_shader_code
                .replace("RasterPosition", "gl_Position")
        );
        return code;
    }
    pub fn emit_fragment_shader(&self) -> String {
        let mut code = String::from("#version 450\n\n");

        // 入力変数(varyingsから)
        for (n, ivar) in self
            .varyings_between_shaders
            .iter()
            .filter(|&&(_, dst, _)| dst == br::ShaderStage::FRAGMENT)
            .flat_map(|&(_, _, ref v)| v)
            .enumerate()
        {
            let require_flat = ivar.type_str == "int";
            code += &format!(
                "layout(location = {}) {}in {} {};\n",
                n,
                if require_flat { "flat " } else { "" },
                Self::translate_vtype(ivar.type_str),
                ivar.name
            );
        }
        // 出力変数(ソースコード中/Target\[\d+\]/から)
        let mut fragment_code = String::from(
            *self
                .fragment_shader_code
                .as_ref()
                .expect("No fragment shader"),
        );
        let rx = Regex::new(r"Target\[(\d+)\]").expect("compiling regex");
        loop {
            let replaced = if let Some(caps) = rx.captures(&fragment_code) {
                let index = caps.get(1).expect("unreachable").as_str();
                code += &format!(
                    "layout(location = {index}) out vec4 sv_target_{index};\n",
                    index = index
                );
                fragment_code.replace(
                    &format!("Target[{}]", index),
                    &format!("sv_target_{}", index),
                )
            } else {
                break;
            };
            fragment_code = replaced;
        }
        // 定数(uniformとspecconstant)
        if let Some(cons) = self
            .spec_constants_per_stage
            .get(&br::ShaderStage::FRAGMENT)
        {
            for (id, &(name, ty, init)) in cons.iter() {
                code += &format!(
                    "layout(constant_id = {}) const {} {} = {};\n",
                    id, ty, name, init
                );
            }
        }
        self.emit_resource_bindings(&mut code, br::ShaderStage::FRAGMENT);
        if let Some(&(name, cb)) = self.push_constant_per_stage.get(&br::ShaderStage::FRAGMENT) {
            code += &format!("layout(push_constant) uniform {} {{{}}};\n", name, cb);
        }
        code += "\n";
        // header
        if let Some(cs) = self.header_codes.get(&br::ShaderStage::FRAGMENT) {
            for c in cs {
                code += c;
            }
            code += "\n";
        }
        // main
        code += &format!("void main() {{{}}}", fragment_code);
        return code;
    }
    fn emit_resource_bindings(&self, code: &mut String, stage: br::ShaderStage) {
        if let Some(ufs) = self.resources_per_stage.get(&stage) {
            for (&(set, bindings), d) in ufs.iter() {
                let resource_def_code = match d {
                    DescriptorBoundResource::Uniform {
                        struct_name,
                        member_code,
                    } => format!("uniform {struct_name} {{{member_code}}};\n"),
                    DescriptorBoundResource::SamplerBuffer(name) => {
                        format!("uniform samplerBuffer {name};\n")
                    }
                    DescriptorBoundResource::Sampler(dim, name) => {
                        format!("uniform sampler{dim}D {name};\n")
                    }
                    DescriptorBoundResource::SamplerCube(name) => {
                        format!("uniform samplerCube {name};\n")
                    }
                };

                *code += &format!("layout(set = {set}, binding = {bindings}) {resource_def_code}");
            }
        }
    }
    pub fn emit_vertex_bindings(&self) -> Vec<br::vk::VkVertexInputBindingDescription> {
        self.vertex_input
            .iter()
            .map(
                |&(binding, ref blk)| br::vk::VkVertexInputBindingDescription {
                    binding: binding as _,
                    inputRate: blk.rate,
                    stride: blk.packed_size() as _,
                },
            )
            .collect()
    }
    pub fn emit_vertex_attributes(&self) -> Vec<br::vk::VkVertexInputAttributeDescription> {
        let mut attrs = Vec::new();
        let mut location_offs = 0;
        for &(binding, ref blk) in self.vertex_input.iter() {
            let mut offs_in_binding = 0;
            for (loc_offs, &Variable { type_str, .. }) in blk.vars.iter().enumerate() {
                match type_str {
                    "vec4" => {
                        attrs.push(br::vk::VkVertexInputAttributeDescription {
                            location: (location_offs + loc_offs) as _,
                            binding: binding as _,
                            format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                            offset: offs_in_binding as _,
                        });
                        offs_in_binding = align2(
                            offs_in_binding + size_of::<[f32; 4]>(),
                            align_of::<[f32; 4]>(),
                        );
                    }
                    "vec3" => {
                        attrs.push(br::vk::VkVertexInputAttributeDescription {
                            location: (location_offs + loc_offs) as _,
                            binding: binding as _,
                            format: br::vk::VK_FORMAT_R32G32B32_SFLOAT,
                            offset: offs_in_binding as _,
                        });
                        offs_in_binding = align2(
                            offs_in_binding + size_of::<[f32; 3]>(),
                            align_of::<[f32; 3]>(),
                        );
                    }
                    "vec2" => {
                        attrs.push(br::vk::VkVertexInputAttributeDescription {
                            location: (location_offs + loc_offs) as _,
                            binding: binding as _,
                            format: br::vk::VK_FORMAT_R32G32_SFLOAT,
                            offset: offs_in_binding as _,
                        });
                        offs_in_binding = align2(
                            offs_in_binding + size_of::<[f32; 2]>(),
                            align_of::<[f32; 2]>(),
                        );
                    }
                    "float" => {
                        attrs.push(br::vk::VkVertexInputAttributeDescription {
                            location: (location_offs + loc_offs) as _,
                            binding: binding as _,
                            format: br::vk::VK_FORMAT_R32_SFLOAT,
                            offset: offs_in_binding as _,
                        });
                        offs_in_binding =
                            align2(offs_in_binding + size_of::<f32>(), align_of::<f32>());
                    }
                    "uint" => {
                        attrs.push(br::vk::VkVertexInputAttributeDescription {
                            location: (location_offs + loc_offs) as _,
                            binding: binding as _,
                            format: br::vk::VK_FORMAT_R32_UINT,
                            offset: offs_in_binding as _,
                        });
                        offs_in_binding =
                            align2(offs_in_binding + size_of::<u32>(), align_of::<u32>());
                    }
                    "int" => {
                        attrs.push(br::vk::VkVertexInputAttributeDescription {
                            location: (location_offs + loc_offs) as _,
                            binding: binding as _,
                            format: br::vk::VK_FORMAT_R32_SINT,
                            offset: offs_in_binding as _,
                        });
                        offs_in_binding =
                            align2(offs_in_binding + size_of::<i32>(), align_of::<i32>());
                    }
                    "ivec4" => {
                        attrs.push(br::vk::VkVertexInputAttributeDescription {
                            location: (location_offs + loc_offs) as _,
                            binding: binding as _,
                            format: br::vk::VK_FORMAT_R32G32B32A32_SINT,
                            offset: offs_in_binding as _,
                        });
                        offs_in_binding = align2(
                            offs_in_binding + size_of::<[i32; 4]>(),
                            align_of::<[i32; 4]>(),
                        );
                    }
                    "i8vec4" => {
                        attrs.push(br::vk::VkVertexInputAttributeDescription {
                            location: (location_offs + loc_offs) as _,
                            binding: binding as _,
                            format: br::vk::VK_FORMAT_R8G8B8A8_SINT,
                            offset: offs_in_binding as _,
                        });
                        offs_in_binding = align2(
                            offs_in_binding + size_of::<[i8; 4]>(),
                            align_of::<[i8; 4]>(),
                        );
                    }
                    "u8vec4" => {
                        attrs.push(br::vk::VkVertexInputAttributeDescription {
                            location: (location_offs + loc_offs) as _,
                            binding: binding as _,
                            format: br::vk::VK_FORMAT_R8G8B8A8_UINT,
                            offset: offs_in_binding as _,
                        });
                        offs_in_binding = align2(
                            offs_in_binding + size_of::<[u8; 4]>(),
                            align_of::<[u8; 4]>(),
                        );
                    }
                    _ => warn!(
                        "Unimplemented: Cannot estimate appropriate attribute info for `{}`",
                        type_str
                    ),
                }
            }
            location_offs += blk.vars.len();
        }

        debug!("Generated Attributes: {:?}", attrs);
        return attrs;
    }
}
