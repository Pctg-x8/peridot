
use bedrock as br;
use std::rc::Rc;

pub trait SpecConstantStorage
{
    fn as_pair(&self) -> (Vec<br::vk::VkSpecializationMapEntry>, br::DynamicDataCell);
}

pub struct LayoutedPipeline(br::Pipeline, Rc<br::PipelineLayout>);
impl LayoutedPipeline
{
    pub fn combine(p: br::Pipeline, layout: &Rc<br::PipelineLayout>) -> Self
    {
        LayoutedPipeline(p, layout.clone())
    }
    pub fn pipeline(&self) -> &br::Pipeline { &self.0 }
    pub fn layout(&self) -> &br::PipelineLayout { &self.1 }
    pub fn bind(&self, rec: &mut br::CmdRecord) { rec.bind_graphics_pipeline_pair(&self.0, &self.1); }
}
