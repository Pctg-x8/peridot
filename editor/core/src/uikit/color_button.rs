use core::cell::Cell;
use std::{rc::Rc, u32};

use shared::{LogicalUnit, Point, Rect, Size};

use crate::{
    input::{
        EventContinueControl, InputEventContext,
        hittest::{
            HitTestTreeActionHandler, HitTestTreeData, HitTestTreeRef, PointerButtonActionArgs,
        },
    },
    rendering::composite::{
        AnimatableColor, Border, CompositeMode, CompositeRect, CompositeTexture, CompositeTreeRef,
        CornerRadius, TextureMappingMode, TextureType,
    },
    uicore::{
        CustomFlyoutViewOpenRequest, FlyoutSurfacePresenter, FlyoutSurfacePresenterConstructor,
        MeasureContext, RenderContext, TeardownContext, TypedViewIdentifier, View, ViewConstructor,
        ViewIdentifier, ViewInitContext, ViewLayoutStateStore, ViewRegisterable,
        ViewRenderElements, ViewRenderQueue,
    },
    uikit::{
        COLOR_PICKER_SHARED_RES, ColorPickerBackingStoreEvent, ColorPickerSharedResources,
        ColorPickerView, ColorPickerViewInit,
    },
};

pub struct EditableColorButtonViewInit {
    pub color: u32,
}
impl ViewConstructor for EditableColorButtonViewInit {
    type ConcreteView = EditableColorButtonView;

    #[inline(always)]
    fn construct(self, id: TypedViewIdentifier<Self::ConcreteView>) -> Self::ConcreteView {
        EditableColorButtonView {
            id,
            eh: None,
            color: self.color,
        }
    }
}

pub struct EditableColorButtonView {
    id: TypedViewIdentifier<EditableColorButtonView>,
    eh: Option<Rc<EditableColorButtonEventHandler>>,
    color: u32,
}
impl EditableColorButtonView {
    const COLOR_PREVIEW_MARGIN: f32 = 6.0;
}
impl View for EditableColorButtonView {
    fn render(
        &mut self,
        layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        let e = match self.eh {
            Some(ref e) => {
                ctx.composite_tree
                    .begin_mod_chain(e.ct_color)
                    .composite_mode(CompositeMode::FillColor(AnimatableColor::Value([
                        e.color.get() as u8 as f32 / 255.0,
                        (e.color.get() >> 8) as u8 as f32 / 255.0,
                        (e.color.get() >> 16) as u8 as f32 / 255.0,
                        (e.color.get() >> 24) as u8 as f32 / 255.0,
                    ])))
                    .apply();
                ctx.composite_tree
                    .begin_mod_chain(e.ct_root)
                    .rect_imm(layout_rect.clone())
                    .apply();
                ctx.ht_manager.mod_chain(e.ht_root).rect(layout_rect);

                e
            }
            None => {
                // first render
                let shared = COLOR_PICKER_SHARED_RES.0.get_or_init(|| {
                    ColorPickerSharedResources::new(
                        ctx.main_thread_texture_id_issuer,
                        ctx.system_link.rt_sender(),
                    )
                });

                let ct_root = CompositeRect::build()
                    .rect_imm(layout_rect.clone())
                    .composite_fill_color_imm([1.0, 1.0, 1.0, 0.0])
                    .border(Border {
                        thickness: 1.0,
                        color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                        ..Default::default()
                    })
                    .corner_radius(CornerRadius::all(8.0))
                    .create(ctx.composite_tree);
                let ct_color_base = CompositeRect::build()
                    .offset_imm(Self::COLOR_PREVIEW_MARGIN, Self::COLOR_PREVIEW_MARGIN)
                    .size_imm(
                        -Self::COLOR_PREVIEW_MARGIN * 2.0,
                        -Self::COLOR_PREVIEW_MARGIN * 2.0,
                    )
                    .expand_full()
                    .composite(CompositeMode::DirectSourceOver(CompositeTexture {
                        id: shared.alpha_slider_bg_tex_id,
                        r#type: TextureType::Color,
                        mapping: TextureMappingMode::Repeat,
                        slice_borders: [0.0; 4],
                    }))
                    .create(ctx.composite_tree);
                let ct_color = CompositeRect::build()
                    .expand_full()
                    .composite_fill_color_imm([
                        self.color as u8 as f32 / 255.0,
                        (self.color >> 8) as u8 as f32 / 255.0,
                        (self.color >> 16) as u8 as f32 / 255.0,
                        (self.color >> 24) as u8 as f32 / 255.0,
                    ])
                    .create(ctx.composite_tree);
                let ht_root = HitTestTreeData::build()
                    .rect(layout_rect)
                    .interactive_defaults()
                    .create(ctx.ht_manager);

                ctx.composite_tree.add_child(ct_color_base, ct_color);
                ctx.composite_tree.add_child(ct_root, ct_color_base);

                let eh = Rc::new_cyclic(|thisref| EditableColorButtonEventHandler {
                    thisref: thisref.clone(),
                    view_id: self.id,
                    ct_root,
                    ht_root,
                    ct_color,
                    color: Cell::new(self.color),
                });
                ctx.ht_manager.set_action_handler(ht_root, &eh);

                &*self.eh.insert(eh)
            }
        };

        ViewRenderElements {
            composite_tree: Some(e.ct_root),
            hit_tree: Some(e.ht_root),
            ..ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        let Some(entity) = self.eh.take() else {
            // not rendered
            return;
        };

        ctx.composite_tree.free_all(entity.ct_root);
        ctx.ht_manager.free_all(entity.ht_root);
    }

    fn measure_preferred_content_size(&self, _ctx: &mut MeasureContext) -> Size<LogicalUnit> {
        Size::new_logical(48.0, 20.0)
    }
}

struct EditableColorButtonEventHandler {
    thisref: std::rc::Weak<EditableColorButtonEventHandler>,
    view_id: TypedViewIdentifier<EditableColorButtonView>,
    ct_root: CompositeTreeRef,
    ct_color: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    color: Cell<u32>,
}
impl HitTestTreeActionHandler for EditableColorButtonEventHandler {
    fn on_click(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        let vc = Box::new(EditableColorButtonPickerFlyoutViewConstructor {
            backing_store: self.thisref.clone(),
        });
        let (gl, gt, gw, gh, _) = context.ht_manager.compute_global_rect_autoroot(sender);
        context.request_open_custom_flyout_view(CustomFlyoutViewOpenRequest {
            parent: context
                .ht_manager
                .query_root_window(sender)
                .expect("not mounted"),
            pos: Point::new_logical(gl + gw * 0.5 - vc.size().width * 0.5, gt + gh),
            content_ctor: vc,
        });

        EventContinueControl::STOP_PROPAGATION
    }
}
impl ColorPickerBackingStoreEvent for EditableColorButtonEventHandler {
    fn value(&self) -> u32 {
        self.color.get()
    }

    fn new_value(&self, value: u32, view_render_queue: &mut ViewRenderQueue) {
        self.color.set(value);
        view_render_queue.schedule(self.view_id.into_untyped());
    }
}

struct EditableColorButtonPickerFlyoutView(TypedViewIdentifier<ColorPickerView>);
impl EditableColorButtonPickerFlyoutView {
    fn new(
        ctx: &mut ViewInitContext,
        backing_store: &std::rc::Weak<EditableColorButtonEventHandler>,
    ) -> Self {
        Self(ctx.construct_view(
            ColorPickerViewInit {
                backing_store: backing_store.clone(),
            },
            |_| [],
        ))
    }
}
impl FlyoutSurfacePresenter for EditableColorButtonPickerFlyoutView {
    fn root_view_id(&self) -> ViewIdentifier {
        self.0.into_untyped()
    }
}

pub struct EditableColorButtonPickerFlyoutViewConstructor {
    backing_store: std::rc::Weak<EditableColorButtonEventHandler>,
}
impl FlyoutSurfacePresenterConstructor for EditableColorButtonPickerFlyoutViewConstructor {
    fn size(&self) -> Size<LogicalUnit> {
        Size::new_logical(128.0 + 16.0, 128.0 + 32.0 + 16.0 + 20.0 + 16.0)
    }

    fn create(&self, ctx: &mut ViewInitContext) -> Box<dyn FlyoutSurfacePresenter> {
        Box::new(EditableColorButtonPickerFlyoutView::new(
            ctx,
            &self.backing_store,
        ))
    }
}
