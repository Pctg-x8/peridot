use peridot::VertexInputSemantic;

use crate::{dump_ty_layout, dump_user_attrs, dump_vl_space_offset_forall_category};

#[derive(Debug, thiserror::Error)]
pub enum VertexSemanticExtractionError<'s> {
    #[error("invalid semantic name bytes({0:?}): {1}")]
    InvalidNameBytes(&'s core::ffi::CStr, core::str::Utf8Error),
    #[error("unsupported semantic name: {0}")]
    Unsupported(&'s str),
}

pub fn extract_vertex_semantic<'s>(
    semantic_name: &'s core::ffi::CStr,
    semantic_index: usize,
) -> Result<VertexInputSemantic, VertexSemanticExtractionError<'s>> {
    let semantic_name = match semantic_name.to_str() {
        Ok(x) => x,
        Err(e) => {
            return Err(VertexSemanticExtractionError::InvalidNameBytes(
                semantic_name,
                e,
            ));
        }
    };

    if semantic_name.eq_ignore_ascii_case("position") {
        return Ok(VertexInputSemantic::Position(semantic_index as _));
    }
    if semantic_name.eq_ignore_ascii_case("normal") {
        return Ok(VertexInputSemantic::Normal(semantic_index as _));
    }
    if semantic_name.eq_ignore_ascii_case("tangent") {
        return Ok(VertexInputSemantic::Tangent(semantic_index as _));
    }
    if semantic_name.eq_ignore_ascii_case("binormal") {
        return Ok(VertexInputSemantic::Binormal(semantic_index as _));
    }
    if semantic_name.eq_ignore_ascii_case("texcoord") {
        return Ok(VertexInputSemantic::Texcoord(semantic_index as _));
    }
    if semantic_name.eq_ignore_ascii_case("color") {
        return Ok(VertexInputSemantic::Color(semantic_index as _));
    }
    if semantic_name.eq_ignore_ascii_case("blendindex") {
        return Ok(VertexInputSemantic::BlendIndex(semantic_index as _));
    }
    if semantic_name.eq_ignore_ascii_case("blendweight") {
        return Ok(VertexInputSemantic::BlendWeight(semantic_index as _));
    }
    if semantic_name.eq_ignore_ascii_case("misc") {
        return Ok(VertexInputSemantic::Misc(semantic_index as _));
    }

    Err(VertexSemanticExtractionError::Unsupported(semantic_name))
}

#[derive(Debug)]
pub(crate) struct VertexInput {
    pub semantic_name: std::ffi::CString,
    pub semantic_index: usize,
    pub location: usize,
}

pub(crate) fn visit_vsh_input(
    v: &slang::reflection::VariableLayout,
    global_session: &impl slang::IGlobalSession,
    indent: usize,
    collected: &mut Vec<VertexInput>,
) {
    let ty = v.type_layout();
    println!("{}vsh param: {:?}", "  ".repeat(indent), v.name());
    dump_vl_space_offset_forall_category(v, &"  ".repeat(indent));
    dump_user_attrs(v, &"  ".repeat(indent + 1));
    dump_ty_layout(ty, indent + 1);

    if let Some(t) = ty.r#type()
        && t.kind() == slang::reflection::TypeKind::Struct
    {
        // recurse members
        for f in ty.iter_field() {
            visit_vsh_input(f, global_session, indent + 1, collected);
        }
        return;
    }

    if v.variable()
        .and_then(|v| v.find_user_attribute_by_name(global_session, c"peridot_vertex"))
        .is_some()
    {
        assert!(
            v.iter_category()
                .any(|x| x == slang::reflection::ParameterCategory::VaryingInput)
        );

        collected.push(VertexInput {
            semantic_name: v.semantic_name().expect("no semantic decorated").to_owned(),
            semantic_index: v.semantic_index(),
            location: v.offset(slang::reflection::ParameterCategory::VaryingInput),
        });
    }
}
