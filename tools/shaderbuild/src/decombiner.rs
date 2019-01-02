//! Combined Shader Decombiner

use bedrock as br;
use std::str::FromStr;
use std::mem::{align_of, size_of};
use regex::Regex;
use std::collections::BTreeMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeclarationOps { VertexInput, VertexShader, FragmentShader, Varyings, SpecConstant, Uniform, PushConstant }

pub type ParseResult<T> = Result<T, ()>;

trait CharIterationExt : Iterator<Item = char> + Sized {
    fn count_with_bytes(self) -> (usize, usize) { self.fold((0, 0), |(c, b), cc| (c + 1, b + cc.len_utf8())) }
    fn count_with_bytes_while<P: Fn(char) -> bool>(self, pred: P) -> (usize, usize) {
        self.take_while(|&c| pred(c)).fold((0, 0), |(c, b), cc| (c + 1, b + cc.len_utf8()))
    }
}
impl<I: Iterator<Item = char>> CharIterationExt for I {}

pub struct Tokenizer<'s>(&'s str);
impl<'s> Tokenizer<'s> {
    pub fn new(s: &'s str) -> Self { Tokenizer(s) }

    fn strip_comment(&mut self) -> bool {
        if self.strip_prefix("//") {
            let (_, strip_bytes) = self.0.chars().count_with_bytes_while(|c| c != '\n');
            self.0 = &self.0[strip_bytes..];
            return true;
        }
        else { false }
    }
    fn strip_ignores(&mut self) -> &mut Self {
        while self.strip_prefix(" ") || self.strip_prefix("\n") || self.strip_prefix("\r")
            || self.strip_prefix("\t") || self.strip_comment() { }
        return self;
    }

    fn strip_prefix(&mut self, p: &'s str) -> bool {
        if self.0.starts_with(p) { self.0 = &self.0[p.len()..]; return true; }
        else { false }
    }
    fn strip_ident(&mut self) -> ParseResult<&'s str> {
        if self.0.starts_with(|c: char| c.is_digit(10)) { return Err(()); }
        let (_, bytes) = self.0.chars().count_with_bytes_while(|c| c.is_alphanumeric() || c == '_');
        if bytes == 0 { return Err(()); }
        let slice = &self.0[..bytes]; self.0 = &self.0[bytes..];
        return Ok(slice);
    }
    pub fn ident_list(&mut self) -> Vec<&'s str> {
        self.strip_ignores();
        let mut v = if let Ok(id) = self.strip_ident() { vec![id] } else { return Vec::new(); };
        while self.strip_ignores().strip_prefix(",") {
            if let Ok(id) = self.strip_ignores().strip_ident() { v.push(id); } else { break; }
        }
        return v;
    }

    /// : ...
    pub fn glsl_type_ascription(&mut self) -> ParseResult<&'s str> {
        self.strip_ignores();
        if !self.strip_prefix(":") { return Err(()); }
        return self.glsl_represents_until_decl_end();
    }
    /// : ... <term_char>
    pub fn glsl_type_ascription_until(&mut self, term_chars: &[char]) -> ParseResult<&'s str> {
        self.strip_ignores();
        if !self.strip_prefix(":") { return Err(()); }
        return self.glsl_represents_until(term_chars);
    }
    pub fn glsl_represents_until_decl_end(&mut self) -> ParseResult<&'s str> {
        self.strip_ignores();
        let (_, glsl_strip_bytes) = self.0.chars().count_with_bytes_while(|c| c != ';' && c != '@');
        if glsl_strip_bytes == 0 { return Err(()); }
        let glsl_strip = &self.0[..glsl_strip_bytes];
        self.0 = &self.0[glsl_strip_bytes..];
        return Ok(glsl_strip);
    }
    pub fn glsl_represents_until(&mut self, term_chars: &[char]) -> ParseResult<&'s str> {
        self.strip_ignores();
        let (_, glsl_strip_bytes) = self.0.chars().count_with_bytes_while(|c| term_chars.iter().all(|&cc| cc != c));
        if glsl_strip_bytes == 0 { return Err(()); }
        let glsl_strip = &self.0[..glsl_strip_bytes];
        self.0 = &self.0[glsl_strip_bytes..];
        return Ok(glsl_strip);
    }
    
    pub fn index_number(&mut self) -> ParseResult<usize> {
        self.strip_ignores();
        let (_, num_bytes) = self.0.chars().count_with_bytes_while(|c| c.is_digit(10));
        if let Ok(n) = usize::from_str(&self.0[..num_bytes]) {
            self.0 = &self.0[num_bytes..];
            return Ok(n);
        }
        else { Err(()) }
    }

    pub fn declaration_op(&mut self) -> ParseResult<DeclarationOps> {
        self.strip_ignores();
        if self.strip_prefix("FragmentShader") { return Ok(DeclarationOps::FragmentShader); }
        if self.strip_prefix("SpecConstant") { return Ok(DeclarationOps::SpecConstant); }
        if self.strip_prefix("VertexShader") { return Ok(DeclarationOps::VertexShader); }
        if self.strip_prefix("PushConstant") { return Ok(DeclarationOps::PushConstant); }
        if self.strip_prefix("VertexInput") { return Ok(DeclarationOps::VertexInput); }
        if self.strip_prefix("Varyings") { return Ok(DeclarationOps::Varyings); }
        if self.strip_prefix("Uniform") { return Ok(DeclarationOps::Uniform); }
        return Err(());
    }
    pub fn shader_stage(&mut self) -> ParseResult<br::ShaderStage> {
        self.strip_ignores();
        if self.strip_prefix("FragmentShader") { return Ok(br::ShaderStage::FRAGMENT); }
        if self.strip_prefix("VertexShader") { return Ok(br::ShaderStage::VERTEX); }
        return Err(());
    }
    pub fn bracketed_stage(&mut self) -> ParseResult<br::ShaderStage> {
        self.strip_ignores();
        if !self.bracket_start() { return Err(()); }
        let st = self.shader_stage()?;
        if !self.bracket_end() { return Err(()); }
        return Ok(st);
    }
    pub fn codeblock(&mut self) -> ParseResult<&'s str> {
        self.strip_ignores();
        if !self.block_start() { return Err(()); }
        fn strip_bytes_counter<I: Iterator<Item = char>>(mut c: I, current: usize, nestlevel: usize)
                -> ParseResult<usize> {
            match c.next() {
                Some(cc @ '{') => strip_bytes_counter(c, current + cc.len_utf8(), nestlevel + 1),
                Some(cc @ '}') => if nestlevel == 0 { Ok(current) }
                    else { strip_bytes_counter(c, current + cc.len_utf8(), nestlevel - 1) },
                Some(cc) => strip_bytes_counter(c, current + cc.len_utf8(), nestlevel),
                None => Err(())
            }
        }
        let cb_slice_bytes = strip_bytes_counter(self.0.chars(), 0, 0).expect("Missing closing brace");
        let cb_slice = &self.0[..cb_slice_bytes];
        self.0 = &self.0[cb_slice_bytes + 1..];
        return Ok(cb_slice);
    }
    pub fn binding(&mut self) -> ParseResult<(usize, br::vk::VkVertexInputRate)> {
        self.strip_ignores();
        if !self.strip_prefix("Binding") { return Err(()); }
        let index = self.index_number()?;
        let irate;
        if self.bracket_start() {
            self.strip_ignores();
            if self.strip_prefix("PerInstance") { irate = br::vk::VK_VERTEX_INPUT_RATE_INSTANCE; }
            else if self.strip_prefix("PerVertex") { irate = br::vk::VK_VERTEX_INPUT_RATE_VERTEX; }
            else { return Err(()); }
            if !self.bracket_end() { return Err(()); }
        }
        else { irate = br::vk::VK_VERTEX_INPUT_RATE_VERTEX; }
        return Ok((index, irate));
    }
    /// `SpecConstant` v <BracketedStage> `(` <IndexNumber> `)`
    pub fn spec_constant_header_rest(&mut self) -> ParseResult<(br::ShaderStage, usize)> {
        self.strip_ignores();
        let stg = self.bracketed_stage()?;
        if !self.strip_ignores().strip_prefix("(") { return Err(()); }
        let idx = self.index_number()?;
        if !self.strip_ignores().strip_prefix(")") { return Err(()); }
        return Ok((stg, idx));
    }
    /// `Uniform` v <BracketedStage> `(` <IndexNumber> `,` <IndexNumber> `)`
    pub fn uniform_header_rest(&mut self) -> ParseResult<(br::ShaderStage, usize, usize)> {
        self.strip_ignores();
        let stg = self.bracketed_stage()?;
        if !self.strip_ignores().strip_prefix("(") { return Err(()); }
        let set = self.index_number()?;
        if !self.strip_ignores().strip_prefix(",") { return Err(()); }
        let binding = self.index_number()?;
        if !self.strip_ignores().strip_prefix(")") { return Err(()); }
        return Ok((stg, set, binding));
    }
    /// `PushConstant` v <BracketStage> `(` <IndexNumber> `)`
    pub fn push_constant_header_rest(&mut self) -> ParseResult<br::ShaderStage> {
        self.strip_ignores();
        return self.bracketed_stage();
    }

    pub fn block_start(&mut self) -> bool { self.strip_ignores(); self.strip_prefix("{") }
    pub fn block_end(&mut self) -> bool { self.strip_ignores(); self.strip_prefix("}") }
    pub fn bracket_start(&mut self) -> bool { self.strip_ignores(); self.strip_prefix("[") }
    pub fn bracket_end(&mut self) -> bool { self.strip_ignores(); self.strip_prefix("]") }
    pub fn declaration_end(&mut self) -> bool { self.strip_ignores().strip_prefix(";") }
    pub fn arrow(&mut self) -> bool { self.strip_ignores().strip_prefix("->") }

    pub fn no_chars(&self) -> bool { self.0.is_empty() }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Variable<'s> { name: &'s str, type_str: &'s str }
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BindingBlock<'s> {
    rate: br::vk::VkVertexInputRate, vars: Vec<Variable<'s>>
}
impl<'s> BindingBlock<'s> {
    fn packed_size(&self) -> usize {
        let mut total = 0;
        for &Variable { type_str, .. } in &self.vars {
            if type_str == "vec4" {
                let offs = align2(total, align_of::<[f32; 4]>());
                total = offs + size_of::<[f32; 4]>();
            }
            else if type_str == "vec3" {
                let offs = align2(total, align_of::<f32>());
                total = offs + size_of::<[f32; 3]>();
            }
            else if type_str == "vec2" {
                let offs = align2(total, align_of::<[f32; 2]>());
                total = offs + size_of::<[f32; 2]>();
            }
            else if type_str == "float" {
                let offs = align2(total, align_of::<f32>());
                total = offs + size_of::<f32>();
            }
            else { println!("Warning: Unable to determine exact packed size"); }
        }
        return total;
    }
}

fn align2(x: usize, a: usize) -> usize { (x + (a - 1)) & !(a - 1) }

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ToplevelBlock<'s> {
    VertexInput(Vec<(usize, BindingBlock<'s>)>),
    ShaderCode(br::ShaderStage, &'s str),
    Varying(br::ShaderStage, br::ShaderStage, Vec<Variable<'s>>),
    SpecConstant(br::ShaderStage, usize, &'s str, &'s str, &'s str),
    Uniform(br::ShaderStage, usize, usize, &'s str, &'s str),
    PushConstant(br::ShaderStage, &'s str, &'s str)
}
impl<'s> Tokenizer<'s> {
    pub fn binding_block(&mut self) -> ParseResult<(usize, BindingBlock<'s>)> {
        let (index, rate) = self.binding()?;
        if !self.block_start() { return Err(()); }
        let mut vars = Vec::new();
        while !self.block_end() {
            let idents = self.ident_list();
            if idents.is_empty() { panic!("No declaration found"); }
            let glsl_type_str = self.glsl_type_ascription().expect("Type required(binding)");
            vars.extend(idents.into_iter().map(|name| Variable { name, type_str: glsl_type_str }));
            if !self.declaration_end() { panic!("; required at end of each declaration"); }
        }
        return Ok((index, BindingBlock { rate, vars }));
    }
    pub fn vertex_input_block(&mut self) -> Vec<(usize, BindingBlock<'s>)> {
        if !self.block_start() { return Vec::new(); }
        let mut bindings = Vec::new();
        while !self.block_end() {
            bindings.push(self.binding_block().expect("Binding block required"));
        }
        return bindings;
    }

    pub fn varying(&mut self) -> (br::ShaderStage, br::ShaderStage, Vec<Variable<'s>>) {
        let src = self.shader_stage().expect("Source ShaderStage required");
        if !self.arrow() { panic!("Arrow required"); }
        let dst = self.shader_stage().expect("Destination ShaderStage required");
        if !self.block_start() { panic!("Variable Block required"); }
        let mut vars = Vec::new();
        while !self.block_end() {
            let idents = self.ident_list();
            if idents.is_empty() { panic!("No declaration found"); }
            let glsl_type_str = self.glsl_type_ascription().expect("Type required(varying)");
            vars.extend(idents.into_iter().map(|name| Variable { name, type_str: glsl_type_str }));
            if !self.declaration_end() { panic!("; required at end of each declaration"); }
        }
        return (src, dst, vars);
    }
    pub fn spec_constant(&mut self) -> ToplevelBlock<'s> {
        let (stg, idx) = self.spec_constant_header_rest().expect("ShaderStage then ConstantID required");
        let id = self.strip_ignores().strip_ident().expect("Identifier required");
        let ty = self.strip_ignores().glsl_type_ascription_until(&['=']).expect("Type required for SpecConstant");
        if !self.strip_ignores().strip_prefix("=") { panic!("Equal required"); }
        let init = self.glsl_represents_until_decl_end().expect("Initial value required");
        if !self.strip_ignores().declaration_end() { panic!("; required at the end of each declaration"); }
        return ToplevelBlock::SpecConstant(stg, idx, id, ty, init);
    }
    pub fn uniform(&mut self) -> ToplevelBlock<'s> {
        let (stg, set, binding) = self.uniform_header_rest()
            .expect("ShaderStage then pair of descriptor set and binding indices are required");
        let id = self.strip_ignores().strip_ident().expect("Identifier for GLSL TypeName required");
        let code = self.codeblock().expect("GLSL CodeBlock required");
        return ToplevelBlock::Uniform(stg, set, binding, id, code);
    }
    pub fn push_constant(&mut self) -> ToplevelBlock<'s> {
        let stg = self.push_constant_header_rest().expect("ShaderStage is required");
        let id = self.strip_ignores().strip_ident().expect("Identifier for PushConstant TypeName required");
        let code = self.codeblock().expect("GLSL CodeBlock required");
        return ToplevelBlock::PushConstant(stg, id, code);
    }

    pub fn toplevel_block(&mut self) -> ParseResult<ToplevelBlock<'s>> {
        match self.declaration_op() {
            Ok(DeclarationOps::VertexInput) => {
                let vi = self.vertex_input_block();
                if vi.is_empty() { Err(()) } else { Ok(ToplevelBlock::VertexInput(vi)) }
            },
            Ok(DeclarationOps::VertexShader) =>
                self.codeblock().map(|c| ToplevelBlock::ShaderCode(br::ShaderStage::VERTEX, c)),
            Ok(DeclarationOps::FragmentShader) =>
                self.codeblock().map(|c| ToplevelBlock::ShaderCode(br::ShaderStage::FRAGMENT, c)),
            Ok(DeclarationOps::Varyings) => {
                let (src, dst, vars) = self.varying();
                return Ok(ToplevelBlock::Varying(src, dst, vars));
            },
            Ok(DeclarationOps::SpecConstant) => Ok(self.spec_constant()),
            Ok(DeclarationOps::Uniform) => Ok(self.uniform()),
            Ok(DeclarationOps::PushConstant) => Ok(self.push_constant()),
            _ => Err(())
        }
    }

    pub fn toplevel_blocks(&mut self) -> Vec<ToplevelBlock<'s>> {
        let mut blocks = Vec::new();
        while !self.strip_ignores().no_chars() {
            blocks.push(self.toplevel_block().expect("Toplevel-Block required"));
        }
        return blocks;
    }
}

pub type GlslType<'s> = &'s str;

#[derive(Debug, Clone)]
pub struct CombinedShader<'s> {
    vertex_input: Vec<(usize, BindingBlock<'s>)>,
    vertex_shader_code: &'s str, fragment_shader_code: Option<&'s str>,
    varyings_between_shaders: Vec<(br::ShaderStage, br::ShaderStage, Vec<Variable<'s>>)>,
    spec_constants_per_stage: BTreeMap<br::ShaderStage, BTreeMap<usize, (&'s str, GlslType<'s>, &'s str)>>,
    uniforms_per_stage: BTreeMap<br::ShaderStage, BTreeMap<(usize, usize), (&'s str, &'s str)>>,
    push_constant_per_stage: BTreeMap<br::ShaderStage, (&'s str, &'s str)>
}
impl<'s> CombinedShader<'s> {
    pub fn from_parsed_blocks(blocks: Vec<ToplevelBlock<'s>>) -> Self {
        let mut cs = CombinedShader {
            vertex_input: Vec::new(),
            vertex_shader_code: "", fragment_shader_code: None,
            varyings_between_shaders: Vec::new(),
            spec_constants_per_stage: BTreeMap::new(),
            uniforms_per_stage: BTreeMap::new(),
            push_constant_per_stage: BTreeMap::new()
        };

        for tb in blocks {
            match tb {
                ToplevelBlock::VertexInput(mut bindings) => cs.vertex_input.append(&mut bindings),
                ToplevelBlock::ShaderCode(br::ShaderStage::VERTEX, c) => {
                    if cs.vertex_shader_code.is_empty() { cs.vertex_shader_code = c; }
                    else { panic!("Multiple Vertex Shader code"); }
                },
                ToplevelBlock::ShaderCode(br::ShaderStage::FRAGMENT, c) => {
                    if cs.fragment_shader_code.is_none() { cs.fragment_shader_code = Some(c); }
                    else { panic!("Multiple Fragment Shader code"); }
                },
                ToplevelBlock::ShaderCode(ss, _) => panic!("Unsupported Shader Stage: {:08b}", ss.0),
                ToplevelBlock::Varying(src, dst, vars) => cs.varyings_between_shaders.push((src, dst, vars)),
                ToplevelBlock::SpecConstant(stg, idx, name, ty, init) => {
                    let storage = cs.spec_constants_per_stage.entry(stg).or_insert_with(BTreeMap::new);
                    if storage.contains_key(&idx) {
                        panic!("Multiple Definitions of SpecConstant for same id in same stage");
                    }
                    storage.insert(idx, (name, ty, init));
                },
                ToplevelBlock::Uniform(stg, set, binding, name, init) => {
                    let storage = cs.uniforms_per_stage.entry(stg).or_insert_with(BTreeMap::new);
                    if storage.contains_key(&(set, binding)) {
                        panic!("Multiple Definitions of Uniform for same (set, binding) in same stage");
                    }
                    storage.insert((set, binding), (name, init));
                },
                ToplevelBlock::PushConstant(stg, name, members) => {
                    if cs.push_constant_per_stage.contains_key(&stg) {
                        panic!("Multiple Definitions of PushConstants for same shader stage");
                    }
                    cs.push_constant_per_stage.insert(stg, (name, members));
                }
            }
        }
        if cs.vertex_shader_code.is_empty() { panic!("VertexShader is not specified"); }
        return cs;
    }

    pub fn is_provided_fsh(&self) -> bool { self.fragment_shader_code.is_some() }

    pub fn emit_vertex_shader(&self) -> String {
        let mut code = String::from("#version 450\n\n");

        // 入力変数(vertex_inputから)
        for (n, vi_vars) in self.vertex_input.iter().flat_map(|&(_, ref bb)| &bb.vars).enumerate() {
            code += &format!("layout(location = {}) in {} {};\n", n, vi_vars.type_str, vi_vars.name);
        }
        // 出力変数
        for (n, ovar) in self.varyings_between_shaders.iter()
            .filter(|&&(src, _, _)| src == br::ShaderStage::VERTEX)
            .flat_map(|&(_, _, ref v)| v).enumerate() {
            code += &format!("layout(location = {}) out {} {};\n", n, ovar.type_str, ovar.name);
        }
        // gl_Positionの宣言を追加
        if self.vertex_shader_code.contains("RasterPosition") {
            code += "out gl_PerVertex { out vec4 gl_Position; };\n";
        }
        // 定数(uniformとspecconstantとpushconstant)
        if let Some(cons) = self.spec_constants_per_stage.get(&br::ShaderStage::VERTEX) {
            for (id, &(name, ty, init)) in cons.iter() {
                code += &format!("layout(constant_id = {}) const {} {} = {};\n", id, ty, name, init);
            }
        }
        if let Some(ufs) = self.uniforms_per_stage.get(&br::ShaderStage::VERTEX) {
            for (&(set, bindings), &(name, cont)) in ufs.iter() {
                code += &format!("layout(set = {}, binding = {}) uniform {} {{{}}};\n", set, bindings, name, cont);
            }
        }
        if let Some(&(name, cb)) = self.push_constant_per_stage.get(&br::ShaderStage::VERTEX) {
            code += &format!("layout(push_constant) uniform {} {{{}}};\n", name, cb);
        }
        code += "\n";
        // main
        code += &format!("void main() {{{}}}", self.vertex_shader_code.replace("RasterPosition", "gl_Position"));
        return code;
    }
    pub fn emit_fragment_shader(&self) -> String {
        let mut code = String::from("#version 450\n\n");

        // 入力変数(varyingsから)
        for (n, ivar) in self.varyings_between_shaders.iter()
            .filter(|&&(_, dst, _)| dst == br::ShaderStage::FRAGMENT)
            .flat_map(|&(_, _, ref v)| v).enumerate() {
            code += &format!("layout(location = {}) in {} {};\n", n, ivar.type_str, ivar.name);
        }
        // 出力変数(ソースコード中/Target\[\d+\]/から)
        let mut fragment_code = String::from(*self.fragment_shader_code.as_ref().expect("No fragment shader"));
        let rx = Regex::new(r"Target\[(\d+)\]").expect("compiling regex");
        loop {
            let replaced = if let Some(caps) = rx.captures(&fragment_code) {
                let index = caps.get(1).expect("unreachable").as_str();
                code += &format!("layout(location = {index}) out vec4 sv_target_{index};\n", index=index);
                fragment_code.replace(&format!("Target[{}]", index), &format!("sv_target_{}", index))
            }
            else { break; };
            fragment_code = replaced;
        }
        // 定数(uniformとspecconstant)
        if let Some(cons) = self.spec_constants_per_stage.get(&br::ShaderStage::FRAGMENT) {
            for (id, &(name, ty, init)) in cons.iter() {
                code += &format!("layout(constant_id = {}) const {} {} = {};\n", id, ty, name, init);
            }
        }
        if let Some(ufs) = self.uniforms_per_stage.get(&br::ShaderStage::FRAGMENT) {
            for (&(set, bindings), &(name, cont)) in ufs.iter() {
                code += &format!("layout(set = {}, binding = {}) uniform {} {{{}}};\n", set, bindings, name, cont);
            }
        }
        if let Some(&(name, cb)) = self.push_constant_per_stage.get(&br::ShaderStage::FRAGMENT) {
            code += &format!("layout(push_constant) uniform {} {{{}}};\n", name, cb);
        }
        code += "\n";
        // main
        code += &format!("void main() {{{}}}", fragment_code);
        return code;
    }
    pub fn emit_vertex_bindings(&self) -> Vec<br::vk::VkVertexInputBindingDescription> {
        self.vertex_input.iter().map(|&(binding, ref blk)| br::vk::VkVertexInputBindingDescription {
            binding: binding as _, inputRate: blk.rate, stride: blk.packed_size() as _
        }).collect()
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
                            binding: binding as _, format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                            offset: offs_in_binding as _
                        });
                        offs_in_binding = align2(offs_in_binding + size_of::<[f32; 4]>(), align_of::<[f32; 4]>());
                    },
                    "vec3" => {
                        attrs.push(br::vk::VkVertexInputAttributeDescription {
                            location: (location_offs + loc_offs) as _,
                            binding: binding as _, format: br::vk::VK_FORMAT_R32G32B32_SFLOAT,
                            offset: offs_in_binding as _
                        });
                        offs_in_binding = align2(offs_in_binding + size_of::<[f32; 3]>(), align_of::<[f32; 3]>());
                    },
                    "vec2" => {
                        attrs.push(br::vk::VkVertexInputAttributeDescription {
                            location: (location_offs + loc_offs) as _,
                            binding: binding as _, format: br::vk::VK_FORMAT_R32G32_SFLOAT,
                            offset: offs_in_binding as _
                        });
                        offs_in_binding = align2(offs_in_binding + size_of::<[f32; 2]>(), align_of::<[f32; 2]>());
                    },
                    "float" => {
                        attrs.push(br::vk::VkVertexInputAttributeDescription {
                            location: (location_offs + loc_offs) as _,
                            binding: binding as _, format: br::vk::VK_FORMAT_R32_SFLOAT,
                            offset: offs_in_binding as _
                        });
                        offs_in_binding = align2(offs_in_binding + size_of::<f32>(), align_of::<f32>());
                    },
                    _ => println!("Warning: Cannot estimate appropriate attribute info")
                }
            }
            location_offs += blk.vars.len();
        }
        return attrs;
    }
}
