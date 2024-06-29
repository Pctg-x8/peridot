use std::borrow::Cow;

use windows::{
    Foundation::Numerics::{Vector2, Vector3},
    Win32::Graphics::DirectWrite::DWRITE_FONT_WEIGHT_NORMAL,
    UI::Composition::{SpriteVisual, VisualCollection},
};

use crate::{
    app_subsystem_instances::AppSubsystemInstances,
    uikit::{HitTestTree, MountableView, ViewContext},
    winapi_extras::VisualExtensions,
};

pub struct LabelView {
    pub root: SpriteVisual,
}
impl LabelView {
    pub fn new(
        text: impl Into<Cow<'static, str>>,
        ctx: &(impl ViewContext + ?Sized),
    ) -> windows::core::Result<Self> {
        let root = AppSubsystemInstances::get()
            .compositor
            .CreateSpriteVisual()?;
        let text_format = AppSubsystemInstances::get()
            .text_format_stock
            .borrow_mut()
            .get("system-ui", 12.0, DWRITE_FONT_WEIGHT_NORMAL)?;
        let text_surface = AppSubsystemInstances::get()
            .text_surface_stock
            .borrow_mut()
            .get(&text_format, ctx.current_dpi(), text)?;
        let brush = AppSubsystemInstances::get()
            .compositor
            .CreateSurfaceBrushWithSurface(&text_surface.surface)?;
        root.set_properties().brush(&brush)?.size(Vector2 {
            X: text_surface.width,
            Y: text_surface.height,
        })?;

        Ok(Self { root })
    }

    pub fn set_position(&self, pos: Vector3) -> windows::core::Result<()> {
        self.root.SetOffset(pos)
    }
}
impl MountableView for LabelView {
    fn mount(
        &self,
        onto: &VisualCollection,
        _onto_ht: &HitTestTree,
        _view_context: &dyn ViewContext,
    ) -> windows::core::Result<()> {
        onto.InsertAtTop(&self.root)?;

        Ok(())
    }

    fn unmount(&self, _view_context: &dyn ViewContext) -> windows::core::Result<()> {
        self.root.Parent()?.Children()?.Remove(&self.root)?;

        Ok(())
    }
}
