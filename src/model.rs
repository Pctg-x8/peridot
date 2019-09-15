//! ModelData Traits and Impls

use super::*;
use bedrock as br;

// 仮定義
pub trait ModelData
{
    type PreallocOffsetType;
    type RendererParams;

    fn prealloc(&self, alloc: &mut BufferPrealloc) -> Self::PreallocOffsetType;
    fn stage_data_into(&self, mem: &br::MappedMemoryRange, offsets: Self::PreallocOffsetType) -> Self::RendererParams;
}
pub trait DefaultRenderCommands
{
    type Extras;

    fn default_render_commands<EH: EngineEvents<NL>, NL: NativeLinker>(&self, e: &Engine<EH, NL>,
        cmd: &mut br::CmdRecord, buffer: &Buffer, extras: &Self::Extras);
}
