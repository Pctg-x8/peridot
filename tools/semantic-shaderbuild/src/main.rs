use std::{collections::HashMap, path::Path};

use bedrock as br;

fn main() {
    for a in std::env::args().skip(1) {
        // <input_path>[=<output_path>]の形式で指定

        let mut splitted = a.splitn(2, '=');
        let input_path = std::path::PathBuf::from(splitted.next().expect("empty input"));
        let output_path = match splitted.next() {
            Some(x) => std::path::PathBuf::from(x),
            // 指定がない場合は入力パスの拡張子を変えたものにする
            None => input_path.with_extension("pss"),
        };

        run(input_path, output_path);
    }
}

fn run(input_path: impl AsRef<Path>, output_path: impl AsRef<Path>) {
    let content = std::fs::read_to_string(input_path.as_ref()).expect("Failed to read input");
    let mut tokenizer = Tokenizer::new(&content);
    let mut gc = CombinedShaderGenContext::new();
    while !tokenizer.source.is_empty() {
        let toplevel = ToplevelBlock::parse(&mut tokenizer).expect("parse error");
        // println!("toplevel: {toplevel:#?}");
        gc.process(toplevel).expect("genctx error");

        tokenizer.strip_ignores();
    }

    println!(
        "{}: Compiling Generated GLSL......",
        input_path.as_ref().display()
    );

    let vsh_compiler = {
        let mut compiler =
            GLSLCompiler::new("vertex").expect("Failed to spawn vertex shader compiler");
        gc.emit_vertex_shader_code(&mut compiler)
            .expect("Failed to emit vertex shader code");
        compiler.terminate_input();
        compiler
    };
    let fsh_compiler = if gc.has_fragment_shader() {
        let mut compiler =
            GLSLCompiler::new("frag").expect("Failed to spawn fragment shader compiler");
        gc.emit_fragment_shader_code(&mut compiler)
            .expect("Failed to emit fragment shader code");
        compiler.terminate_input();
        Some(compiler)
    } else {
        None
    };

    let vsh_spv = match vsh_compiler
        .wait_for_completion()
        .expect("io error in compile vsh")
    {
        CompilationResult::Succeeded(x) => x,
        CompilationResult::Failed(s) => panic!("vsh compilation failed! {s:?}"),
    };
    let fsh_spv = if let Some(c) = fsh_compiler {
        match c.wait_for_completion().expect("io error in compile fsh") {
            CompilationResult::Succeeded(x) => Some(x),
            CompilationResult::Failed(s) => panic!("fsh compilation failed! {s:?}"),
        }
    } else {
        None
    };

    let asset = peridot_semantic_shader::ShaderPackAsset {
        vertex_shader_code: vsh_spv,
        fragment_shader_code: fsh_spv,
        input_semantic_location_map: gc
            .vertex_input_semantic_to_location_number
            .into_iter()
            .map(|(k, (v, _))| (k, v))
            .collect(),
    };
    asset
        .write(
            &mut std::fs::File::options()
                .create(true)
                .write(true)
                .truncate(true)
                .open(output_path)
                .expect("Failed to open dest file"),
        )
        .expect("Failed to write asset");
}

pub enum CompilationResult {
    Succeeded(Vec<u32>),
    Failed(std::process::ExitStatus),
}

pub struct GLSLCompiler(std::process::Child);
impl GLSLCompiler {
    pub fn new(stage: &str) -> std::io::Result<Self> {
        let p = std::process::Command::new("glslc")
            .arg(&format!("-fshader-stage={stage}"))
            .args(["-o", "-", "-mfmt=num", "-"])
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::inherit())
            .spawn()?;

        Ok(Self(p))
    }

    pub fn terminate_input(&mut self) {
        drop(self.0.stdin.take());
    }

    pub fn wait_for_completion(self) -> std::io::Result<CompilationResult> {
        let r = self.0.wait_with_output()?;

        if !r.status.success() {
            return Ok(CompilationResult::Failed(r.status));
        }

        let spv_binary = unsafe { core::str::from_utf8_unchecked(&r.stdout) }
            .split(',')
            .map(|x| match x.trim().as_bytes() {
                &[b'0', b'x', a, b, c, d, e, f, g, h] => {
                    const fn cn(c: u8) -> u32 {
                        match c {
                            b'0'..=b'9' => (c - b'0') as _,
                            b'a'..=b'f' => ((c - b'a') + 0x0a) as _,
                            b'A'..=b'F' => ((c - b'A') + 0x0a) as _,
                            _ => unreachable!(),
                        }
                    }

                    cn(a) << 28
                        | cn(b) << 24
                        | cn(c) << 20
                        | cn(d) << 16
                        | cn(e) << 12
                        | cn(f) << 8
                        | cn(g) << 4
                        | cn(h)
                }
                _ => unreachable!(),
            })
            .collect::<Vec<_>>();

        Ok(CompilationResult::Succeeded(spv_binary))
    }
}
impl std::io::Write for GLSLCompiler {
    #[inline(always)]
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.stdin.as_mut().expect("no stdin?").write(buf)
    }

    #[inline(always)]
    fn write_all(&mut self, buf: &[u8]) -> std::io::Result<()> {
        self.0.stdin.as_mut().expect("no stdin?").write_all(buf)
    }

    #[inline(always)]
    fn write_fmt(&mut self, fmt: std::fmt::Arguments<'_>) -> std::io::Result<()> {
        self.0.stdin.as_mut().expect("no stdin?").write_fmt(fmt)
    }

    #[inline(always)]
    fn write_vectored(&mut self, bufs: &[std::io::IoSlice<'_>]) -> std::io::Result<usize> {
        self.0
            .stdin
            .as_mut()
            .expect("no stdin?")
            .write_vectored(bufs)
    }

    #[inline(always)]
    fn flush(&mut self) -> std::io::Result<()> {
        self.0.stdin.as_mut().expect("no stdin?").flush()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Location {
    pub line: usize,
    pub col: usize,
}
impl core::fmt::Display for Location {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceRef<'s> {
    pub slice: &'s str,
    pub loc: Location,
}

pub struct Tokenizer<'s> {
    source: &'s str,
    loc: Location,
}
impl<'s> Tokenizer<'s> {
    pub fn new(source: &'s str) -> Self {
        Self {
            source,
            loc: Location { line: 1, col: 1 },
        }
    }

    pub fn strip_ignores(&mut self) {
        // continuous spaces
        let (c, b) = self
            .source
            .chars()
            .take_while(|&c| matches!(c, ' ' | '\t' | '\r' | '\x0c'))
            .fold((0, 0), |(c, b), ch| match ch {
                // この2文字は字数を進めない
                '\r' | '\x0c' => (c, b + ch.len_utf8()),
                _ => (c + 1, b + ch.len_utf8()),
            });
        if c > 0 {
            self.source = &self.source[b..];
            self.loc.col += c;
            return self.strip_ignores();
        }

        // continuous linefeeds
        let (c, b) = self
            .source
            .chars()
            .take_while(|&c| c == '\n')
            .fold((0, 0), |(c, b), ch| (c + 1, b + ch.len_utf8()));
        if c > 0 {
            self.source = &self.source[b..];
            self.loc.line += c;
            self.loc.col = 1;
            return self.strip_ignores();
        }
    }

    #[inline]
    fn strip_head(&mut self, chars: usize, bytes: usize) -> SourceRef<'s> {
        let r = SourceRef {
            slice: &self.source[..bytes],
            loc: self.loc,
        };
        self.source = &self.source[bytes..];
        self.loc.col += chars;

        r
    }

    pub fn try_strip_ident(&mut self) -> Result<SourceRef<'s>, Location> {
        self.strip_ignores();

        if !self
            .source
            .starts_with(|c: char| c.is_alphabetic() || matches!(c, '_' | '$'))
        {
            // not a head of ident
            return Err(self.loc);
        }

        let (c, b) = self
            .source
            .chars()
            .take_while(|&c| c.is_alphanumeric() || matches!(c, '_' | '$'))
            .fold((0, 0), |(c, b), ch| (c + 1, b + ch.len_utf8()));
        if c <= 0 {
            // no ident characters in streak
            return Err(self.loc);
        }

        Ok(self.strip_head(c, b))
    }

    pub fn try_strip_int(&mut self) -> Result<SourceRef<'s>, Location> {
        self.strip_ignores();

        if self.source.starts_with("0x") || self.source.starts_with("0X") {
            // hexadecimal

            let (c, b) = self
                .source
                .chars()
                .skip(2)
                .take_while(|&c| matches!(c, '0'..='9' | 'a'..='f' | 'A'..='F' | '_'))
                .fold((0, 0), |(c, b), ch| (c + 1, b + ch.len_utf8()));
            if c <= 0 {
                // no ipart characters in streak
                return Err(self.loc);
            }

            return Ok(self.strip_head(c + 2, b + 2));
        }

        let (c, b) = self
            .source
            .chars()
            .take_while(|&c| matches!(c, '0'..='9' | '_'))
            .fold((0, 0), |(c, b), ch| (c + 1, b + ch.len_utf8()));
        if c <= 0 {
            // no ipart characters in streak
            return Err(self.loc);
        }

        Ok(self.strip_head(c, b))
    }

    pub fn try_strip_block_start(&mut self) -> Result<Location, Location> {
        self.strip_ignores();

        if self.source.starts_with('{') {
            Ok(self.strip_head(1, 1).loc)
        } else {
            Err(self.loc)
        }
    }

    pub fn try_strip_block_end(&mut self) -> Result<Location, Location> {
        self.strip_ignores();

        if self.source.starts_with('}') {
            Ok(self.strip_head(1, 1).loc)
        } else {
            Err(self.loc)
        }
    }

    pub fn try_strip_bracket_start(&mut self) -> Result<Location, Location> {
        self.strip_ignores();

        if self.source.starts_with('[') {
            Ok(self.strip_head(1, 1).loc)
        } else {
            Err(self.loc)
        }
    }

    pub fn try_strip_bracket_end(&mut self) -> Result<Location, Location> {
        self.strip_ignores();

        if self.source.starts_with(']') {
            Ok(self.strip_head(1, 1).loc)
        } else {
            Err(self.loc)
        }
    }

    pub fn try_strip_paren_start(&mut self) -> Result<Location, Location> {
        self.strip_ignores();

        if self.source.starts_with('(') {
            Ok(self.strip_head(1, 1).loc)
        } else {
            Err(self.loc)
        }
    }

    pub fn try_strip_paren_end(&mut self) -> Result<Location, Location> {
        self.strip_ignores();

        if self.source.starts_with(')') {
            Ok(self.strip_head(1, 1).loc)
        } else {
            Err(self.loc)
        }
    }

    pub fn try_strip_colon(&mut self) -> Result<Location, Location> {
        self.strip_ignores();

        if self.source.starts_with(':') {
            Ok(self.strip_head(1, 1).loc)
        } else {
            Err(self.loc)
        }
    }

    pub fn try_strip_semicolon(&mut self) -> Result<Location, Location> {
        self.strip_ignores();

        if self.source.starts_with(';') {
            Ok(self.strip_head(1, 1).loc)
        } else {
            Err(self.loc)
        }
    }

    pub fn try_strip_rightarrow(&mut self) -> Result<Location, Location> {
        self.strip_ignores();

        if self.source.starts_with("->") {
            Ok(self.strip_head(2, 2).loc)
        } else {
            Err(self.loc)
        }
    }

    pub fn try_strip_comma(&mut self) -> Result<Location, Location> {
        self.strip_ignores();

        if self.source.starts_with(',') {
            Ok(self.strip_head(1, 1).loc)
        } else {
            Err(self.loc)
        }
    }

    pub fn try_strip_vertical_bar(&mut self) -> Result<Location, Location> {
        self.strip_ignores();

        if self.source.starts_with('|') {
            Ok(self.strip_head(1, 1).loc)
        } else {
            Err(self.loc)
        }
    }

    pub fn try_strip_raw_code_block(&mut self) -> Result<SourceRef<'s>, Location> {
        self.strip_ignores();

        if !self.source.starts_with('{') {
            // not a block start
            return Err(self.loc);
        }

        let mut loc_track = self.loc;
        let mut b = 1;
        let mut block_level = 1;
        while b < self.source.len() {
            if self.source[b..].starts_with('}') {
                block_level -= 1;
                if block_level <= 0 {
                    break;
                }

                loc_track.col += 1;
                b += 1;
                continue;
            }

            if self.source[b..].starts_with('{') {
                block_level += 1;
                loc_track.col += 1;
                b += 1;

                continue;
            }

            let (cn, bn) = self.source[b..]
                .chars()
                .take_while(|&c| c == '\n')
                .fold((0, 0), |(c, b), ch| (c + 1, b + ch.len_utf8()));
            if cn > 0 {
                // process linefeed(location counter)
                loc_track.col = 1;
                loc_track.line += cn;
                b += bn;
            }

            for ch in self.source[b..]
                .chars()
                .take_while(|&c| !matches!(c, '{' | '}' | '\n'))
            {
                loc_track.col += 1;
                b += ch.len_utf8();
            }
        }

        if !self.source[b..].starts_with('}') {
            // not closing block
            return Err(self.loc);
        }

        b += 1;
        loc_track.col += 1;

        let r = SourceRef {
            slice: &self.source[..b],
            loc: self.loc,
        };
        self.source = &self.source[b..];
        self.loc = loc_track;

        Ok(r)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("Misc semantic (at {0}) requires explicit index")]
    MiscRequiresExplicitIndex(Location),
    #[error("Unknown semantic at {0}")]
    UnknownSemantic(Location),
    #[error("Identifier expected, parsing near at {0}")]
    IdentExpected(Location),
    #[error("Integer number expected, parsing near at {0}")]
    IntExpected(Location),
    #[error("{0:?} expected, parsing near at {1}")]
    SpecificTokenExpected(&'static str, Location),
    #[error("Unknown ShaderStage at {0}")]
    UnknownShaderStage(Location),
    #[error("Raw shader code block expected, parsing near at {0}")]
    RawShaderCodeBlockExpected(Location),
    #[error("Unknown toplevel block at {0}")]
    UnknownToplevelBlock(Location),
}
impl ParseError {
    pub const fn closing_bracket_expected(loc: Location) -> Self {
        Self::SpecificTokenExpected("]", loc)
    }

    pub const fn colon_expected(loc: Location) -> Self {
        Self::SpecificTokenExpected(":", loc)
    }

    pub const fn semicolon_expected(loc: Location) -> Self {
        Self::SpecificTokenExpected(";", loc)
    }

    pub const fn comma_expected(loc: Location) -> Self {
        Self::SpecificTokenExpected(",", loc)
    }

    pub const fn bracket_start_expected(loc: Location) -> Self {
        Self::SpecificTokenExpected("[", loc)
    }

    pub const fn block_start_expected(loc: Location) -> Self {
        Self::SpecificTokenExpected("{", loc)
    }

    pub const fn block_end_expected(loc: Location) -> Self {
        Self::SpecificTokenExpected("}", loc)
    }

    pub const fn paren_start_expected(loc: Location) -> Self {
        Self::SpecificTokenExpected("(", loc)
    }

    pub const fn paren_end_expected(loc: Location) -> Self {
        Self::SpecificTokenExpected(")", loc)
    }

    pub const fn rightarrow_expected(loc: Location) -> Self {
        Self::SpecificTokenExpected("->", loc)
    }
}

#[derive(Debug, Clone)]
pub struct TypeSyntax<'s>(pub SourceRef<'s>);
impl<'s> TypeSyntax<'s> {
    pub fn parse(tokenizer: &mut Tokenizer<'s>) -> Result<Self, ParseError> {
        let ident = tokenizer
            .try_strip_ident()
            .map_err(ParseError::IdentExpected)?;

        Ok(Self(ident))
    }
}

#[repr(transparent)]
#[derive(Debug, Clone)]
pub struct ShaderStage(pub br::ShaderStage);
impl ShaderStage {
    pub fn parse(tokenizer: &mut Tokenizer) -> Result<(Self, Location), ParseError> {
        let text = tokenizer
            .try_strip_ident()
            .map_err(ParseError::IdentExpected)?;

        if text.slice.eq_ignore_ascii_case("vertex") {
            return Ok((Self(br::ShaderStage::Vertex), text.loc));
        }
        if text.slice.eq_ignore_ascii_case("tesscontrol") {
            return Ok((Self(br::ShaderStage::TessellationControl), text.loc));
        }
        if text.slice.eq_ignore_ascii_case("tesseval") {
            return Ok((Self(br::ShaderStage::TessellationEvaluation), text.loc));
        }
        if text.slice.eq_ignore_ascii_case("geometry") {
            return Ok((Self(br::ShaderStage::Geometry), text.loc));
        }
        if text.slice.eq_ignore_ascii_case("fragment") {
            return Ok((Self(br::ShaderStage::Fragment), text.loc));
        }

        Err(ParseError::UnknownShaderStage(text.loc))
    }
}

#[repr(transparent)]
#[derive(Debug, Clone)]
pub struct ShaderStageBits(pub Vec<(ShaderStage, Location)>);
impl ShaderStageBits {
    pub fn parse(tokenizer: &mut Tokenizer) -> Result<Self, ParseError> {
        let mut bits = vec![ShaderStage::parse(tokenizer)?];
        while tokenizer.try_strip_vertical_bar().is_ok() {
            bits.push(ShaderStage::parse(tokenizer)?);
        }

        Ok(Self(bits))
    }

    pub fn bits(&self) -> br::vk::VkShaderStageFlags {
        self.0
            .iter()
            .fold(0, |a, (x, _)| a | x.0 as br::vk::VkShaderStageFlags)
    }
}

#[repr(transparent)]
#[derive(Debug)]
pub struct VertexInputSemantic(pub peridot_semantic_shader::VertexInputSemantic);
impl VertexInputSemantic {
    pub fn parse(tokenizer: &mut Tokenizer) -> Result<(Self, Location), ParseError> {
        let text = tokenizer
            .try_strip_ident()
            .map_err(ParseError::IdentExpected)?;
        let text_lc = text.slice.to_ascii_lowercase();

        if let Some(left) = text_lc.strip_prefix("position") {
            if left.is_empty() {
                return Ok((
                    Self(peridot_semantic_shader::VertexInputSemantic::Position(0)),
                    text.loc,
                ));
            }
            if let Ok(index) = left.parse() {
                return Ok((
                    Self(peridot_semantic_shader::VertexInputSemantic::Position(
                        index,
                    )),
                    text.loc,
                ));
            }
        }
        if let Some(left) = text_lc.strip_prefix("normal") {
            if left.is_empty() {
                return Ok((
                    Self(peridot_semantic_shader::VertexInputSemantic::Normal(0)),
                    text.loc,
                ));
            }
            if let Ok(index) = left.parse() {
                return Ok((
                    Self(peridot_semantic_shader::VertexInputSemantic::Normal(index)),
                    text.loc,
                ));
            }
        }
        if let Some(left) = text_lc.strip_prefix("tangent") {
            if left.is_empty() {
                return Ok((
                    Self(peridot_semantic_shader::VertexInputSemantic::Tangent(0)),
                    text.loc,
                ));
            }
            if let Ok(index) = left.parse() {
                return Ok((
                    Self(peridot_semantic_shader::VertexInputSemantic::Tangent(index)),
                    text.loc,
                ));
            }
        }
        if let Some(left) = text_lc.strip_prefix("binormal") {
            if left.is_empty() {
                return Ok((
                    Self(peridot_semantic_shader::VertexInputSemantic::Binormal(0)),
                    text.loc,
                ));
            }
            if let Ok(index) = left.parse() {
                return Ok((
                    Self(peridot_semantic_shader::VertexInputSemantic::Binormal(
                        index,
                    )),
                    text.loc,
                ));
            }
        }
        if let Some(left) = text_lc.strip_prefix("texcoord") {
            if left.is_empty() {
                return Ok((
                    Self(peridot_semantic_shader::VertexInputSemantic::Texcoord(0)),
                    text.loc,
                ));
            }
            if let Ok(index) = left.parse() {
                return Ok((
                    Self(peridot_semantic_shader::VertexInputSemantic::Texcoord(
                        index,
                    )),
                    text.loc,
                ));
            }
        }
        if let Some(left) = text_lc.strip_prefix("misc") {
            if left.is_empty() {
                return Err(ParseError::MiscRequiresExplicitIndex(text.loc));
            }
            if let Ok(index) = left.parse() {
                return Ok((
                    Self(peridot_semantic_shader::VertexInputSemantic::Misc(index)),
                    text.loc,
                ));
            }
        }

        Err(ParseError::UnknownSemantic(text.loc))
    }
}

#[derive(Debug)]
pub struct VertexInputEntry<'s> {
    pub semantic: (VertexInputSemantic, Location),
    pub name: SourceRef<'s>,
    pub r#type: TypeSyntax<'s>,
}
impl<'s> VertexInputEntry<'s> {
    pub fn parse(tokenizer: &mut Tokenizer<'s>) -> Result<Self, ParseError> {
        tokenizer
            .try_strip_bracket_start()
            .map_err(ParseError::bracket_start_expected)?;
        let semantic = VertexInputSemantic::parse(tokenizer)?;
        tokenizer
            .try_strip_bracket_end()
            .map_err(ParseError::closing_bracket_expected)?;

        let name = tokenizer
            .try_strip_ident()
            .map_err(ParseError::IdentExpected)?;
        tokenizer
            .try_strip_colon()
            .map_err(ParseError::colon_expected)?;
        let r#type = TypeSyntax::parse(tokenizer)?;

        Ok(Self {
            semantic,
            name,
            r#type,
        })
    }
}

#[derive(Debug)]
pub struct VertexInputBlock<'s> {
    pub entries: Vec<VertexInputEntry<'s>>,
}
impl<'s> VertexInputBlock<'s> {
    pub fn parse_after_head_ident(tokenizer: &mut Tokenizer<'s>) -> Result<Self, ParseError> {
        tokenizer
            .try_strip_block_start()
            .map_err(ParseError::block_start_expected)?;

        let mut entries = Vec::new();
        while !tokenizer.try_strip_block_end().is_ok() {
            entries.push(VertexInputEntry::parse(tokenizer)?);
            tokenizer
                .try_strip_semicolon()
                .map_err(ParseError::semicolon_expected)?;
        }

        Ok(Self { entries })
    }
}

#[derive(Debug)]
pub struct FragmentOutputEntry<'s> {
    pub name: SourceRef<'s>,
    pub r#type: TypeSyntax<'s>,
}
impl<'s> FragmentOutputEntry<'s> {
    pub fn parse(tokenizer: &mut Tokenizer<'s>) -> Result<Self, ParseError> {
        let name = tokenizer
            .try_strip_ident()
            .map_err(ParseError::IdentExpected)?;
        tokenizer
            .try_strip_colon()
            .map_err(ParseError::colon_expected)?;
        let r#type = TypeSyntax::parse(tokenizer)?;

        Ok(Self { name, r#type })
    }
}

#[derive(Debug)]
pub struct FragmentOutputBlock<'s> {
    pub entries: Vec<FragmentOutputEntry<'s>>,
}
impl<'s> FragmentOutputBlock<'s> {
    pub fn parse_after_head_ident(tokenizer: &mut Tokenizer<'s>) -> Result<Self, ParseError> {
        tokenizer
            .try_strip_block_start()
            .map_err(ParseError::block_start_expected)?;
        let mut entries = Vec::new();
        while !tokenizer.try_strip_block_end().is_ok() {
            entries.push(FragmentOutputEntry::parse(tokenizer)?);
            tokenizer
                .try_strip_semicolon()
                .map_err(ParseError::semicolon_expected)?;
        }

        Ok(Self { entries })
    }
}

#[derive(Debug, Clone)]
pub struct DataBlockEntry<'s> {
    pub name: SourceRef<'s>,
    pub r#type: TypeSyntax<'s>,
}
impl<'s> DataBlockEntry<'s> {
    pub fn parse(tokenizer: &mut Tokenizer<'s>) -> Result<Self, ParseError> {
        let name = tokenizer
            .try_strip_ident()
            .map_err(ParseError::IdentExpected)?;
        tokenizer
            .try_strip_colon()
            .map_err(ParseError::colon_expected)?;
        let r#type = TypeSyntax::parse(tokenizer)?;

        Ok(Self { name, r#type })
    }
}

#[derive(Debug, Clone)]
pub struct UniformDataBlock<'s> {
    pub shader_stage: Option<ShaderStageBits>,
    pub set_index: SourceRef<'s>,
    pub binding_index: SourceRef<'s>,
    pub block_name: SourceRef<'s>,
    pub entries: Vec<DataBlockEntry<'s>>,
}
impl<'s> UniformDataBlock<'s> {
    pub fn parse_after_head_ident(tokenizer: &mut Tokenizer<'s>) -> Result<Self, ParseError> {
        let shader_stage = if tokenizer.try_strip_bracket_start().is_ok() {
            let x = ShaderStageBits::parse(tokenizer)?;
            tokenizer
                .try_strip_bracket_end()
                .map_err(ParseError::closing_bracket_expected)?;

            Some(x)
        } else {
            None
        };

        tokenizer
            .try_strip_paren_start()
            .map_err(ParseError::paren_start_expected)?;
        let set_index = tokenizer.try_strip_int().map_err(ParseError::IntExpected)?;
        tokenizer
            .try_strip_comma()
            .map_err(ParseError::comma_expected)?;
        let binding_index = tokenizer.try_strip_int().map_err(ParseError::IntExpected)?;
        tokenizer
            .try_strip_paren_end()
            .map_err(ParseError::paren_end_expected)?;

        let block_name = tokenizer
            .try_strip_ident()
            .map_err(ParseError::IdentExpected)?;
        tokenizer
            .try_strip_block_start()
            .map_err(ParseError::block_start_expected)?;
        let mut entries = Vec::new();
        while !tokenizer.try_strip_block_end().is_ok() {
            entries.push(DataBlockEntry::parse(tokenizer)?);
            tokenizer
                .try_strip_semicolon()
                .map_err(ParseError::semicolon_expected)?;
        }

        Ok(Self {
            shader_stage,
            set_index,
            binding_index,
            block_name,
            entries,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Sampler2DDefinition<'s> {
    pub shader_stage: Option<ShaderStageBits>,
    pub set_index: SourceRef<'s>,
    pub binding_index: SourceRef<'s>,
    pub name: SourceRef<'s>,
}
impl<'s> Sampler2DDefinition<'s> {
    pub fn parse_after_head_ident(tokenizer: &mut Tokenizer<'s>) -> Result<Self, ParseError> {
        let shader_stage = if tokenizer.try_strip_bracket_start().is_ok() {
            let x = ShaderStageBits::parse(tokenizer)?;
            tokenizer
                .try_strip_bracket_end()
                .map_err(ParseError::closing_bracket_expected)?;

            Some(x)
        } else {
            None
        };

        tokenizer
            .try_strip_paren_start()
            .map_err(ParseError::paren_start_expected)?;
        let set_index = tokenizer.try_strip_int().map_err(ParseError::IntExpected)?;
        tokenizer
            .try_strip_comma()
            .map_err(ParseError::comma_expected)?;
        let binding_index = tokenizer.try_strip_int().map_err(ParseError::IntExpected)?;
        tokenizer
            .try_strip_paren_end()
            .map_err(ParseError::paren_end_expected)?;

        let name = tokenizer
            .try_strip_ident()
            .map_err(ParseError::IdentExpected)?;
        tokenizer
            .try_strip_semicolon()
            .map_err(ParseError::semicolon_expected)?;

        Ok(Self {
            shader_stage,
            set_index,
            binding_index,
            name,
        })
    }
}

#[derive(Debug, Clone)]
pub struct VaryingEntry<'s> {
    pub name: SourceRef<'s>,
    pub r#type: TypeSyntax<'s>,
}
impl<'s> VaryingEntry<'s> {
    pub fn parse(tokenizer: &mut Tokenizer<'s>) -> Result<Self, ParseError> {
        let name = tokenizer
            .try_strip_ident()
            .map_err(ParseError::IdentExpected)?;
        tokenizer
            .try_strip_colon()
            .map_err(ParseError::colon_expected)?;
        let r#type = TypeSyntax::parse(tokenizer)?;

        Ok(Self { name, r#type })
    }
}

#[derive(Debug)]
pub struct VaryingBlock<'s> {
    pub from_stage: (ShaderStage, Location),
    pub to_stage: (ShaderStage, Location),
    pub entries: Vec<VaryingEntry<'s>>,
}
impl<'s> VaryingBlock<'s> {
    pub fn parse_after_head_ident(tokenizer: &mut Tokenizer<'s>) -> Result<Self, ParseError> {
        let from_stage = ShaderStage::parse(tokenizer)?;
        tokenizer
            .try_strip_rightarrow()
            .map_err(ParseError::rightarrow_expected)?;
        let to_stage = ShaderStage::parse(tokenizer)?;

        tokenizer
            .try_strip_block_start()
            .map_err(ParseError::block_start_expected)?;
        let mut entries = Vec::new();
        while !tokenizer.try_strip_block_end().is_ok() {
            entries.push(VaryingEntry::parse(tokenizer)?);
            tokenizer
                .try_strip_semicolon()
                .map_err(ParseError::semicolon_expected)?;
        }

        Ok(Self {
            from_stage,
            to_stage,
            entries,
        })
    }
}

#[derive(Debug)]
pub enum ToplevelBlock<'s> {
    VertexInput(VertexInputBlock<'s>),
    UniformData(UniformDataBlock<'s>),
    Sampler2D(Sampler2DDefinition<'s>),
    Varying(VaryingBlock<'s>),
    Header((ShaderStage, Location), SourceRef<'s>),
    VertexShader(SourceRef<'s>),
    FragmentShader(SourceRef<'s>),
    FragmentOutput(FragmentOutputBlock<'s>),
}
impl<'s> ToplevelBlock<'s> {
    pub fn parse(tokenizer: &mut Tokenizer<'s>) -> Result<Self, ParseError> {
        let head = tokenizer
            .try_strip_ident()
            .map_err(ParseError::IdentExpected)?;

        match head.slice {
            "VertexInput" => Ok(Self::VertexInput(VertexInputBlock::parse_after_head_ident(
                tokenizer,
            )?)),
            "Uniform" => Ok(Self::UniformData(UniformDataBlock::parse_after_head_ident(
                tokenizer,
            )?)),
            "Sampler2D" => Ok(Self::Sampler2D(
                Sampler2DDefinition::parse_after_head_ident(tokenizer)?,
            )),
            "Varyings" => Ok(Self::Varying(VaryingBlock::parse_after_head_ident(
                tokenizer,
            )?)),
            "Header" => {
                tokenizer
                    .try_strip_bracket_start()
                    .map_err(ParseError::bracket_start_expected)?;
                let shader_stage = ShaderStage::parse(tokenizer)?;
                tokenizer
                    .try_strip_bracket_end()
                    .map_err(ParseError::closing_bracket_expected)?;

                let code = tokenizer
                    .try_strip_raw_code_block()
                    .map_err(ParseError::RawShaderCodeBlockExpected)?;

                Ok(Self::Header(shader_stage, code))
            }
            "VertexShader" => Ok(Self::VertexShader(
                tokenizer
                    .try_strip_raw_code_block()
                    .map_err(ParseError::RawShaderCodeBlockExpected)?,
            )),
            "FragmentShader" => Ok(Self::FragmentShader(
                tokenizer
                    .try_strip_raw_code_block()
                    .map_err(ParseError::RawShaderCodeBlockExpected)?,
            )),
            "FragmentOutput" => Ok(Self::FragmentOutput(
                FragmentOutputBlock::parse_after_head_ident(tokenizer)?,
            )),
            _ => Err(ParseError::UnknownToplevelBlock(head.loc)),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum GenContextError {
    #[error("conflicting vertex input semantic(before at {0}), processing entry at {1}")]
    ConflictingVertexInputSemantic(Location, Location),
    #[error("too many vertex inputs, processing entry at {0}")]
    TooManyVertexInputs(Location),
}

#[derive(Debug)]
pub struct CombinedShaderGenContext<'s> {
    vertex_inputs_ordered: Vec<(SourceRef<'s>, TypeSyntax<'s>)>,
    vertex_input_semantic_to_location_number:
        HashMap<peridot_semantic_shader::VertexInputSemantic, (u32, Location)>,
    vertex_outputs_ordered: Vec<(SourceRef<'s>, TypeSyntax<'s>)>,
    vertex_uniform_blocks: Vec<UniformDataBlock<'s>>,
    vertex_sampler2d_defs: Vec<Sampler2DDefinition<'s>>,
    vertex_shader_header_ordered: Vec<SourceRef<'s>>,
    vertex_shader_main_ordered: Vec<SourceRef<'s>>,
    fragment_inputs_ordered: Vec<(SourceRef<'s>, TypeSyntax<'s>)>,
    fragment_outputs_ordered: Vec<(SourceRef<'s>, TypeSyntax<'s>)>,
    fragment_uniform_blocks: Vec<UniformDataBlock<'s>>,
    fragment_sampler2d_defs: Vec<Sampler2DDefinition<'s>>,
    fragment_shader_header_ordered: Vec<SourceRef<'s>>,
    fragment_shader_main_ordered: Vec<SourceRef<'s>>,
}
impl<'s> CombinedShaderGenContext<'s> {
    pub fn new() -> Self {
        Self {
            vertex_inputs_ordered: Vec::new(),
            vertex_input_semantic_to_location_number: HashMap::new(),
            vertex_outputs_ordered: Vec::new(),
            vertex_uniform_blocks: Vec::new(),
            vertex_sampler2d_defs: Vec::new(),
            vertex_shader_header_ordered: Vec::new(),
            vertex_shader_main_ordered: Vec::new(),
            fragment_inputs_ordered: Vec::new(),
            fragment_outputs_ordered: Vec::new(),
            fragment_uniform_blocks: Vec::new(),
            fragment_sampler2d_defs: Vec::new(),
            fragment_shader_header_ordered: Vec::new(),
            fragment_shader_main_ordered: Vec::new(),
        }
    }

    pub fn process(&mut self, t: ToplevelBlock<'s>) -> Result<(), GenContextError> {
        match t {
            ToplevelBlock::VertexInput(vi) => {
                self.vertex_inputs_ordered.reserve(vi.entries.len());
                for e in vi.entries {
                    let location_number: u32 = self
                        .vertex_inputs_ordered
                        .len()
                        .try_into()
                        .map_err(|_| GenContextError::TooManyVertexInputs(e.name.loc))?;
                    self.vertex_inputs_ordered.push((e.name, e.r#type));
                    let before_semantic = self
                        .vertex_input_semantic_to_location_number
                        .insert(e.semantic.0 .0, (location_number, e.semantic.1));
                    if let Some((_, before_loc)) = before_semantic {
                        return Err(GenContextError::ConflictingVertexInputSemantic(
                            before_loc,
                            e.semantic.1,
                        ));
                    }
                }
            }
            ToplevelBlock::UniformData(u) => {
                let bits = u
                    .shader_stage
                    .as_ref()
                    .map_or(br::vk::VK_SHADER_STAGE_ALL, |x| x.bits());

                if (bits & br::vk::VK_SHADER_STAGE_VERTEX_BIT) != 0 {
                    self.vertex_uniform_blocks.push(u.clone());
                }

                if (bits & br::vk::VK_SHADER_STAGE_FRAGMENT_BIT) != 0 {
                    self.fragment_uniform_blocks.push(u.clone());
                }
            }
            ToplevelBlock::Sampler2D(s) => {
                let bits = match s.shader_stage {
                    Some(ref x) => x.bits(),
                    None => br::vk::VK_SHADER_STAGE_ALL,
                };

                if (bits & br::vk::VK_SHADER_STAGE_VERTEX_BIT) != 0 {
                    self.vertex_sampler2d_defs.push(s.clone());
                }

                if (bits & br::vk::VK_SHADER_STAGE_FRAGMENT_BIT) != 0 {
                    self.fragment_sampler2d_defs.push(s.clone());
                }
            }
            ToplevelBlock::Varying(v) => {
                if v.from_stage.0 .0 == br::ShaderStage::Vertex {
                    self.vertex_outputs_ordered
                        .extend(v.entries.iter().map(|x| (x.name.clone(), x.r#type.clone())));
                }

                if v.to_stage.0 .0 == br::ShaderStage::Fragment {
                    self.fragment_inputs_ordered
                        .extend(v.entries.iter().map(|x| (x.name.clone(), x.r#type.clone())));
                }
            }
            ToplevelBlock::Header((ShaderStage(br::ShaderStage::Vertex), _), cb) => {
                self.vertex_shader_header_ordered.push(cb);
            }
            ToplevelBlock::Header((ShaderStage(br::ShaderStage::TessellationControl), _), _) => {
                unimplemented!("tess control support");
            }
            ToplevelBlock::Header((ShaderStage(br::ShaderStage::TessellationEvaluation), _), _) => {
                unimplemented!("tess eval support");
            }
            ToplevelBlock::Header((ShaderStage(br::ShaderStage::Geometry), _), _) => {
                unimplemented!("geometry support");
            }
            ToplevelBlock::Header((ShaderStage(br::ShaderStage::Fragment), _), cb) => {
                self.fragment_shader_header_ordered.push(cb);
            }
            ToplevelBlock::Header((ShaderStage(br::ShaderStage::Compute), _), _) => {
                unimplemented!("compute support");
            }
            ToplevelBlock::VertexShader(cb) => {
                self.vertex_shader_main_ordered.push(cb);
            }
            ToplevelBlock::FragmentShader(cb) => {
                self.fragment_shader_main_ordered.push(cb);
            }
            ToplevelBlock::FragmentOutput(fo) => {
                self.fragment_outputs_ordered
                    .extend(fo.entries.into_iter().map(|x| (x.name, x.r#type)));
            }
        }

        Ok(())
    }

    #[inline(always)]
    pub fn has_fragment_shader(&self) -> bool {
        !self.fragment_shader_main_ordered.is_empty()
    }

    pub fn emit_vertex_shader_code(
        &self,
        sink: &mut (impl std::io::Write + ?Sized),
    ) -> std::io::Result<()> {
        sink.write_all(b"#version 450\n\n")?;

        // define inputs
        for (ln, (n, t)) in self.vertex_inputs_ordered.iter().enumerate() {
            writeln!(
                sink,
                "layout(location = {ln}) in {} {};",
                t.0.slice, n.slice
            )?;
        }

        // define outputs
        for (ln, (n, t)) in self.vertex_outputs_ordered.iter().enumerate() {
            writeln!(
                sink,
                "layout(location = {ln}) out {} {};",
                t.0.slice, n.slice
            )?;
        }
        sink.write_all(b"out gl_PerVertex { vec4 gl_Position; };\n")?;

        // define data blocks
        for b in self.vertex_uniform_blocks.iter() {
            writeln!(
                sink,
                "layout(set = {}, binding = {}) uniform {} {{",
                b.set_index.slice, b.binding_index.slice, b.block_name.slice
            )?;
            for e in b.entries.iter() {
                writeln!(sink, "    {} {};", e.r#type.0.slice, e.name.slice)?;
            }
            sink.write_all(b"};\n")?;
        }
        for b in self.vertex_sampler2d_defs.iter() {
            writeln!(
                sink,
                "layout(set = {}, binding = {}) uniform sampler2D {};",
                b.set_index.slice, b.binding_index.slice, b.name.slice
            )?;
        }

        // expand header code blocks
        sink.write_vectored(
            &self
                .vertex_shader_header_ordered
                .iter()
                .map(|x| {
                    std::io::IoSlice::new(
                        x.slice
                            .strip_prefix('{')
                            .unwrap_or(x.slice)
                            .strip_suffix('}')
                            .unwrap_or(x.slice)
                            .as_bytes(),
                    )
                })
                .collect::<Vec<_>>(),
        )?;

        // expand main code blocks
        sink.write_all(b"void main() {\n")?;
        sink.write_vectored(
            &self
                .vertex_shader_main_ordered
                .iter()
                .map(|x| {
                    std::io::IoSlice::new(
                        x.slice
                            .strip_prefix('{')
                            .unwrap_or(x.slice)
                            .strip_suffix('}')
                            .unwrap_or(x.slice)
                            .as_bytes(),
                    )
                })
                .collect::<Vec<_>>(),
        )?;
        sink.write_all(b"}\n")?;

        Ok(())
    }

    pub fn emit_fragment_shader_code(
        &self,
        sink: &mut (impl std::io::Write + ?Sized),
    ) -> std::io::Result<()> {
        sink.write_all(b"#version 450\n\n")?;

        // define inputs
        for (ln, (n, t)) in self.fragment_inputs_ordered.iter().enumerate() {
            writeln!(
                sink,
                "layout(location = {ln}) in {} {};",
                t.0.slice, n.slice
            )?;
        }

        // define outputs
        for (ln, (n, t)) in self.fragment_outputs_ordered.iter().enumerate() {
            writeln!(
                sink,
                "layout(location = {ln}) out {} {};",
                t.0.slice, n.slice
            )?;
        }

        // define data blocks
        for b in self.fragment_uniform_blocks.iter() {
            writeln!(
                sink,
                "layout(set = {}, binding = {}) uniform {} {{",
                b.set_index.slice, b.binding_index.slice, b.block_name.slice
            )?;
            for e in b.entries.iter() {
                writeln!(sink, "    {} {};", e.r#type.0.slice, e.name.slice)?;
            }
            sink.write_all(b"};\n")?;
        }
        for b in self.fragment_sampler2d_defs.iter() {
            writeln!(
                sink,
                "layout(set = {}, binding = {}) uniform sampler2D {};",
                b.set_index.slice, b.binding_index.slice, b.name.slice
            )?;
        }

        // expand header code blocks
        sink.write_vectored(
            &self
                .fragment_shader_header_ordered
                .iter()
                .map(|x| {
                    std::io::IoSlice::new(
                        x.slice
                            .strip_prefix('{')
                            .unwrap_or(x.slice)
                            .strip_suffix('}')
                            .unwrap_or(x.slice)
                            .as_bytes(),
                    )
                })
                .collect::<Vec<_>>(),
        )?;

        // expand main code blocks
        sink.write_all(b"void main() {\n")?;
        sink.write_vectored(
            &self
                .fragment_shader_main_ordered
                .iter()
                .map(|x| {
                    std::io::IoSlice::new(
                        x.slice
                            .strip_prefix('{')
                            .unwrap_or(x.slice)
                            .strip_suffix('}')
                            .unwrap_or(x.slice)
                            .as_bytes(),
                    )
                })
                .collect::<Vec<_>>(),
        )?;
        sink.write_all(b"}\n")?;

        Ok(())
    }
}
