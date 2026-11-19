use core::cell::Cell;
use std::rc::Rc;

use shared::{LogicalUnit, Point, Rect, Size};

use crate::{
    input::{
        EventContinueControl, FocusTargetToken, InputEventContext, KeyInputCode,
        KeyInputEventHandler, ModifierKey,
        hittest::{
            CursorShape, HitTestArgs, HitTestTreeActionHandler, HitTestTreeData, HitTestTreeRef,
            PointerActionArgs, PointerButtonActionArgs,
        },
    },
    rendering::{
        MainThreadTextureIDIssuer, RenderMessage, RenderMessageSender, ShaderTexture, TextureID,
        composite::{
            AnimatableColor, AnimatableFloat, Border, CompositeMode, CompositeRect,
            CompositeRectText, CompositeRectTextRun, CompositeTexture, CompositeTree,
            CompositeTreeRef, CornerRadius, Gradient, GradientRef, TextureMappingMode, TextureType,
        },
    },
    uicore::{
        MeasureContext, RenderContext, TeardownContext, TypedViewIdentifier, View, ViewConstructor,
        ViewLayoutStateStore, ViewRenderElements, ViewRenderQueue,
    },
    uikit::TextInputViewCore,
    utils::UnsafeMainThreadOnlyOnceCell,
};

pub trait ColorPickerBackingStoreEvent {
    fn value(&self) -> u32;
    fn new_value(&self, value: u32, view_render_queue: &mut ViewRenderQueue);
}

pub struct ColorPickerSharedResources {
    pub ring_tex_id: TextureID,
    pub alpha_slider_bg_tex_id: TextureID,
}
impl ColorPickerSharedResources {
    pub fn new(
        texid_issuer: &mut MainThreadTextureIDIssuer,
        rt_sender: &RenderMessageSender,
    ) -> Self {
        let ring_tex_id = texid_issuer.issue();
        rt_sender
            .send(RenderMessage::RegisterShaderTexture {
                id: ring_tex_id,
                data: ShaderTexture {
                    width: 128.0,
                    height: 128.0,
                    shader_path: "color_picker_ring.spv".into(),
                },
            })
            .expect("rt_sender.send");
        let alpha_slider_bg_tex_id = texid_issuer.issue();
        rt_sender
            .send(RenderMessage::RegisterShaderTexture {
                id: alpha_slider_bg_tex_id,
                data: ShaderTexture {
                    width: 16.0,
                    height: 16.0,
                    shader_path: "checkerboard.spv".into(),
                },
            })
            .expect("rt_sender.send");

        Self {
            ring_tex_id,
            alpha_slider_bg_tex_id,
        }
    }
}

pub static COLOR_PICKER_SHARED_RES: UnsafeMainThreadOnlyOnceCell<ColorPickerSharedResources> =
    UnsafeMainThreadOnlyOnceCell(core::cell::OnceCell::new());

pub struct ColorPickerViewInit<BackingStore: ColorPickerBackingStoreEvent + 'static> {
    pub backing_store: std::rc::Weak<BackingStore>,
}
impl<BackingStore: ColorPickerBackingStoreEvent + 'static> ViewConstructor
    for ColorPickerViewInit<BackingStore>
{
    type ConcreteView = ColorPickerView;

    #[inline(always)]
    fn construct(self, id: TypedViewIdentifier<Self::ConcreteView>) -> Self::ConcreteView {
        ColorPickerView {
            view_id: id,
            backing_store: self.backing_store,
            eh: None,
        }
    }
}

pub struct ColorPickerView {
    view_id: TypedViewIdentifier<Self>,
    backing_store: std::rc::Weak<dyn ColorPickerBackingStoreEvent>,
    eh: Option<Rc<ColorPickerEventHandler>>,
}
impl Drop for ColorPickerView {
    fn drop(&mut self) {
        if self.eh.is_some() {
            tracing::warn!("ColorPickedView dropped but still rendered");
        }
    }
}
impl ColorPickerView {
    const RING_THICKNESS: f32 = 12.0;
    const GRADIENT_BOX_MARGIN: f32 = 4.0;
    const POINTER_SIZE: f32 = 12.0;
    const ALPHA_SLIDER_THUMB_THICKNESS: f32 = 3.0;
}
impl View for ColorPickerView {
    fn render(
        &mut self,
        layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        let e = match self.eh {
            Some(ref e) => {
                ctx.composite_tree
                    .begin_mod_chain(e.ct_root)
                    .offset_imm(layout_rect.left, layout_rect.top)
                    .apply();
                ctx.ht_manager.get_data_mut(e.ht_root).left = layout_rect.left;
                ctx.ht_manager.get_data_mut(e.ht_root).top = layout_rect.top;

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

                let gradient_box_size =
                    2.0 * (64.0 - Self::RING_THICKNESS - Self::GRADIENT_BOX_MARGIN) / 2.0f32.sqrt();

                let ct_root = CompositeRect::build()
                    .offset_imm(layout_rect.left, layout_rect.top)
                    .size_imm(128.0, 128.0)
                    .composite(CompositeMode::DirectSourceOver(CompositeTexture {
                        id: shared.ring_tex_id,
                        r#type: TextureType::Color,
                        mapping: TextureMappingMode::Stretch,
                        slice_borders: [0.0; 4],
                    }))
                    .create(ctx.composite_tree);
                let ct_sat_light_box = CompositeRect::build()
                    .size_imm(gradient_box_size, gradient_box_size)
                    .centering()
                    .composite(CompositeMode::ColorPickerGradientBox(
                        AnimatableColor::Value([1.0, 0.0, 0.0, 1.0]),
                    ))
                    .create(ctx.composite_tree);
                let ct_pointer = CompositeRect::build()
                    .size_imm(Self::POINTER_SIZE, Self::POINTER_SIZE)
                    .corner_radius(CornerRadius::all(Self::POINTER_SIZE * 0.5))
                    .border(Border {
                        thickness: 2.0,
                        color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                        ..Default::default()
                    })
                    .create(ctx.composite_tree);
                let ct_pointer_dark = CompositeRect::build()
                    .offset_imm(2.0, 2.0)
                    .size_imm(Self::POINTER_SIZE - 4.0, Self::POINTER_SIZE - 4.0)
                    .corner_radius(CornerRadius::all((Self::POINTER_SIZE - 4.0) * 0.5))
                    .border(Border {
                        thickness: 1.0,
                        color: AnimatableColor::Value([0.0, 0.0, 0.0, 0.5]),
                        ..Default::default()
                    })
                    .create(ctx.composite_tree);
                let alpha_slider_content_gradient =
                    ctx.composite_tree.create_gradient(Gradient::Linear {
                        start_color: [1.0, 0.0, 0.0, 0.0],
                        end_color: [1.0, 0.0, 0.0, 1.0],
                        start_pos_relative: [0.0, 0.0],
                        end_pos_relative: [1.0, 0.0],
                    });
                let ct_alpha_slider_base = CompositeRect::build()
                    .offset_imm(0.0, 128.0 + 8.0)
                    .size_imm(128.0, 16.0)
                    .composite(CompositeMode::DirectSourceOver(CompositeTexture {
                        id: shared.alpha_slider_bg_tex_id,
                        r#type: TextureType::Color,
                        mapping: TextureMappingMode::Repeat,
                        slice_borders: [0.0; 4],
                    }))
                    .create(ctx.composite_tree);
                let ct_alpha_slider_content = CompositeRect::build()
                    .expand_full()
                    .composite(CompositeMode::FillLinearGradient(
                        alpha_slider_content_gradient,
                    ))
                    .create(ctx.composite_tree);
                let ct_alpha_slider_thumb = CompositeRect::build()
                    .size_imm(Self::ALPHA_SLIDER_THUMB_THICKNESS, 0.0)
                    .expand_height()
                    .composite_fill_color_imm([0.1, 0.1, 0.1, 1.0])
                    .border(Border {
                        thickness: 0.5,
                        color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                        ..Default::default()
                    })
                    .create(ctx.composite_tree);
                let ct_hex_label = CompositeRect::build()
                    .offset_imm(0.0, 128.0 + 32.0 + 16.0)
                    .size_imm(0.0, 20.0)
                    .text(
                        CompositeRectText::build()
                            .run(
                                CompositeRectTextRun::build("HEX".into())
                                    .color_imm([1.0, 1.0, 1.0, 1.0]),
                            )
                            .vertical_middle(),
                    )
                    .create(ctx.composite_tree);
                let ht_root = HitTestTreeData::build()
                    .left(layout_rect.left)
                    .top(layout_rect.top)
                    .width(128.0)
                    .height(128.0)
                    .create(ctx.ht_manager);
                let ht_sat_light_box = HitTestTreeData::build()
                    .width(gradient_box_size)
                    .height(gradient_box_size)
                    .centering()
                    .create(ctx.ht_manager);
                let ht_alpha_slider = HitTestTreeData::build()
                    .top(128.0 + 8.0)
                    .width(128.0)
                    .height(16.0)
                    .create(ctx.ht_manager);

                let hex_text_input_kf = ctx.keyboard_focus_registry.acquire_token();
                let hex_text_input_ht = HitTestTreeData::build()
                    .rect(Rect::from_lt_size(
                        Point::new_logical(32.0, 128.0 + 32.0 + 16.0),
                        Size::new_logical(128.0 - 32.0, 20.0),
                    ))
                    .cursor_shape(CursorShape::IBeam)
                    .keyboard_focus(hex_text_input_kf)
                    .create(ctx.ht_manager);
                let hex_text_input_view = crate::uikit::TextInputViewCore::new(
                    ctx,
                    Rect::from_lt_size(
                        Point::new_logical(32.0, 128.0 + 32.0 + 16.0),
                        Size::new_logical(128.0 - 32.0, 20.0),
                    ),
                    [0.0, 0.0],
                    [0.0, 0.0],
                    self.view_id.into_untyped(),
                    hex_text_input_ht,
                );

                ctx.composite_tree.add_child(ct_root, ct_sat_light_box);
                ctx.composite_tree.add_child(ct_pointer, ct_pointer_dark);
                ctx.composite_tree.add_child(ct_sat_light_box, ct_pointer);
                ctx.composite_tree
                    .add_child(ct_alpha_slider_base, ct_alpha_slider_content);
                ctx.composite_tree
                    .add_child(ct_alpha_slider_base, ct_alpha_slider_thumb);
                ctx.composite_tree.add_child(ct_root, ct_alpha_slider_base);
                ctx.composite_tree.add_child(ct_root, ct_hex_label);
                ctx.composite_tree
                    .add_child(ct_root, hex_text_input_view.entity().ct_root());
                ctx.ht_manager.add_child(ht_root, ht_sat_light_box);
                ctx.ht_manager.add_child(ht_root, ht_alpha_slider);
                ctx.ht_manager.add_child(ht_root, hex_text_input_ht);

                let eh = Rc::new(ColorPickerEventHandler {
                    backing_store: self.backing_store.clone(),
                    ct_root,
                    ct_sat_light_box,
                    ct_pointer,
                    ct_alpha_slider_thumb,
                    alpha_slider_content_gradient,
                    ht_root,
                    ht_sat_light_box,
                    ht_alpha_slider,
                    sat_light_box_size: Size::new_logical(gradient_box_size, gradient_box_size),
                    ring_selecting: Cell::new(false),
                    box_selecting: Cell::new(false),
                    alpha_sliding: Cell::new(false),
                    current_hue: Cell::new(0.0),
                    current_light: Cell::new(1.0),
                    current_saturation: Cell::new(0.0),
                    current_alpha: Cell::new(1.0),
                    hex_text_input_view,
                    hex_text_input_ht,
                    hex_text_input_kf,
                });
                ctx.ht_manager.set_action_handler(ht_root, &eh);
                ctx.ht_manager.set_action_handler(ht_sat_light_box, &eh);
                ctx.ht_manager.set_action_handler(ht_alpha_slider, &eh);
                ctx.ht_manager.set_action_handler(hex_text_input_ht, &eh);
                ctx.keyboard_focus_registry
                    .set_event_handler(hex_text_input_kf, &eh);

                if let Some(e) = self.backing_store.upgrade() {
                    let v = e.value();

                    eh.set_by_color(v, ctx.composite_tree);
                    eh.hex_text_input_view
                        .entity()
                        .lazy_update(|e| e.set_content(ColorPickerEventHandler::fmt(v)));
                }

                &*self.eh.insert(eh)
            }
        };

        e.hex_text_input_view
            .entity()
            .process_pending_updates_with_ht_mutation(
                ctx.composite_tree,
                ctx.system_link,
                ctx.ht_manager,
                ctx.current_sec,
            );

        ViewRenderElements {
            composite_tree: Some(e.ct_root),
            hit_tree: Some(e.ht_root),
            keyboard_focus: Some(e.hex_text_input_kf),
            ..ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        let Some(e) = self.eh.take() else {
            // not rendered
            return;
        };

        ctx.composite_tree.free_all(e.ct_root);
        ctx.ht_manager.free_all(e.ht_root);
        ctx.composite_tree
            .free_gradient(e.alpha_slider_content_gradient);
    }

    fn measure_preferred_content_size(&self, _ctx: &mut MeasureContext) -> Size<LogicalUnit> {
        Size::new_logical(128.0, 128.0 + 32.0 + 16.0 + 20.0)
    }
}

struct ColorPickerEventHandler {
    backing_store: std::rc::Weak<dyn ColorPickerBackingStoreEvent>,
    ct_root: CompositeTreeRef,
    ct_sat_light_box: CompositeTreeRef,
    ct_pointer: CompositeTreeRef,
    ct_alpha_slider_thumb: CompositeTreeRef,
    alpha_slider_content_gradient: GradientRef,
    ht_root: HitTestTreeRef,
    ht_sat_light_box: HitTestTreeRef,
    ht_alpha_slider: HitTestTreeRef,
    sat_light_box_size: Size<LogicalUnit>,
    ring_selecting: Cell<bool>,
    box_selecting: Cell<bool>,
    alpha_sliding: Cell<bool>,
    current_hue: Cell<f32>,
    current_light: Cell<f32>,
    current_saturation: Cell<f32>,
    current_alpha: Cell<f32>,
    hex_text_input_view: TextInputViewCore,
    hex_text_input_ht: HitTestTreeRef,
    hex_text_input_kf: FocusTargetToken,
}
impl KeyInputEventHandler for ColorPickerEventHandler {
    fn focus_taken(&self, context: &mut InputEventContext) {
        self.hex_text_input_view.entity().focus_taken(context);
    }

    fn focus_released(&self, context: &mut InputEventContext) {
        self.hex_text_input_view.entity().focus_released(context);
        self.confirm_direct_input(context.composite_tree, context.view_render_queue);
    }

    fn keydown(&self, context: &mut InputEventContext, code: KeyInputCode, modifier: ModifierKey) {
        if code == KeyInputCode::Enter {
            // 確定or入力開始
            self.confirm_direct_input(context.composite_tree, context.view_render_queue);
            return;
        }

        if code == KeyInputCode::Esc {
            // 入力キャンセル
            self.cancel_direct_input(context.view_render_queue);
            return;
        }

        self.hex_text_input_view
            .entity()
            .keydown(context, code, modifier);
    }

    fn r#char(&self, context: &mut InputEventContext, ch: char, modifier: ModifierKey) {
        self.hex_text_input_view
            .entity()
            .char(context, ch, modifier);
    }

    fn keyup(&self, context: &mut InputEventContext, code: KeyInputCode, modifier: ModifierKey) {
        self.hex_text_input_view
            .entity()
            .keyup(context, code, modifier);
    }

    #[cfg(feature = "wayland")]
    fn ime_state_changes(
        &self,
        context: &mut InputEventContext,
        new_committed_string: Option<&str>,
        new_preedit_string: Option<&str>,
    ) {
        self.hex_text_input_view.entity().ime_state_changes(
            context,
            new_committed_string,
            new_preedit_string,
        );
    }
}
impl HitTestTreeActionHandler for ColorPickerEventHandler {
    fn hittest(&self, target: HitTestTreeRef, args: &HitTestArgs) -> bool {
        if target == self.ht_root {
            let dcenter_x = args.tree_local_x - 64.0;
            let dcenter_y = args.tree_local_y - 64.0;
            let dcenter = (dcenter_x * dcenter_x + dcenter_y * dcenter_y).sqrt();

            return (64.0 - ColorPickerView::RING_THICKNESS) <= dcenter && dcenter <= 64.0;
        }

        true
    }

    fn on_pointer_down(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        if sender == self.ht_root {
            // ring
            let (local_x, local_y, _, _) = context.ht_manager.translate_client_to_tree_local(
                self.ht_root,
                args.client_pos.x,
                args.client_pos.y,
                args.client_size.width,
                args.client_size.height,
            );
            let dcenter_x = local_x - 64.0;
            let dcenter_y = local_y - 64.0;
            let hue =
                360.0 * (dcenter_y.atan2(dcenter_x) / core::f32::consts::TAU + 0.5) + 360.0 - 90.0;
            self.select_hue(hue, context.composite_tree, context.view_render_queue);
            self.ring_selecting.set(true);

            return EventContinueControl::STOP_PROPAGATION | EventContinueControl::CAPTURE_ELEMENT;
        }

        if sender == self.ht_sat_light_box {
            let (local_x, local_y, w, h) = context.ht_manager.translate_client_to_tree_local(
                self.ht_sat_light_box,
                args.client_pos.x,
                args.client_pos.y,
                args.client_size.width,
                args.client_size.height,
            );
            let local_x = local_x.clamp(0.0, w);
            let local_y = local_y.clamp(0.0, h);
            self.move_cursor(
                local_x,
                local_y,
                context.composite_tree,
                context.view_render_queue,
            );

            self.box_selecting.set(true);
            return EventContinueControl::STOP_PROPAGATION | EventContinueControl::CAPTURE_ELEMENT;
        }

        if sender == self.ht_alpha_slider {
            let (local_x, _, w, _) = context.ht_manager.translate_client_to_tree_local(
                self.ht_alpha_slider,
                args.client_pos.x,
                args.client_pos.y,
                args.client_size.width,
                args.client_size.height,
            );
            let new_alpha = local_x.clamp(0.0, w) / w;
            self.current_alpha.set(new_alpha);
            self.color_changed(context.composite_tree, context.view_render_queue);
            context
                .composite_tree
                .get_mut(self.ct_alpha_slider_thumb)
                .offset[0] = AnimatableFloat::Value(
                new_alpha * w - ColorPickerView::ALPHA_SLIDER_THUMB_THICKNESS * 0.5,
            );
            context
                .composite_tree
                .mark_dirty(self.ct_alpha_slider_thumb);

            self.alpha_sliding.set(true);
            return EventContinueControl::STOP_PROPAGATION | EventContinueControl::CAPTURE_ELEMENT;
        }

        if sender == self.hex_text_input_ht {
            return self
                .hex_text_input_view
                .entity()
                .on_pointer_down(sender, context, args);
        }

        EventContinueControl::empty()
    }

    fn on_pointer_move(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        if sender == self.ht_root && self.ring_selecting.get() {
            // ring
            let (local_x, local_y, _, _) = context.ht_manager.translate_client_to_tree_local(
                self.ht_root,
                args.client_pos.x,
                args.client_pos.y,
                args.client_size.width,
                args.client_size.height,
            );
            let dcenter_x = local_x - 64.0;
            let dcenter_y = local_y - 64.0;
            let hue =
                360.0 * (dcenter_y.atan2(dcenter_x) / core::f32::consts::TAU + 0.5) + 360.0 - 90.0;
            self.select_hue(hue, context.composite_tree, context.view_render_queue);

            return EventContinueControl::STOP_PROPAGATION;
        }

        if sender == self.ht_sat_light_box && self.box_selecting.get() {
            let (local_x, local_y, w, h) = context.ht_manager.translate_client_to_tree_local(
                self.ht_sat_light_box,
                args.client_pos.x,
                args.client_pos.y,
                args.client_size.width,
                args.client_size.height,
            );
            let local_x = local_x.clamp(0.0, w);
            let local_y = local_y.clamp(0.0, h);
            self.move_cursor(
                local_x,
                local_y,
                context.composite_tree,
                context.view_render_queue,
            );

            return EventContinueControl::STOP_PROPAGATION;
        }

        if sender == self.ht_alpha_slider && self.alpha_sliding.get() {
            let (local_x, _, w, _) = context.ht_manager.translate_client_to_tree_local(
                self.ht_alpha_slider,
                args.client_pos.x,
                args.client_pos.y,
                args.client_size.width,
                args.client_size.height,
            );
            let new_alpha = local_x.clamp(0.0, w) / w;
            self.current_alpha.set(new_alpha);
            self.color_changed(context.composite_tree, context.view_render_queue);
            context
                .composite_tree
                .get_mut(self.ct_alpha_slider_thumb)
                .offset[0] = AnimatableFloat::Value(
                new_alpha * w - ColorPickerView::ALPHA_SLIDER_THUMB_THICKNESS * 0.5,
            );
            context
                .composite_tree
                .mark_dirty(self.ct_alpha_slider_thumb);

            return EventContinueControl::STOP_PROPAGATION;
        }

        if sender == self.hex_text_input_ht {
            return self
                .hex_text_input_view
                .entity()
                .on_pointer_move(sender, context, args);
        }

        EventContinueControl::empty()
    }

    fn on_drag_move(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        if sender == self.ht_root {
            // ring
            let (local_x, local_y, _, _) = context.ht_manager.translate_client_to_tree_local(
                self.ht_root,
                args.client_pos.x,
                args.client_pos.y,
                args.client_size.width,
                args.client_size.height,
            );
            let dcenter_x = local_x - 64.0;
            let dcenter_y = local_y - 64.0;
            let hue =
                360.0 * (dcenter_y.atan2(dcenter_x) / core::f32::consts::TAU + 0.5) + 360.0 - 90.0;
            self.select_hue(hue, context.composite_tree, context.view_render_queue);

            return EventContinueControl::STOP_PROPAGATION;
        }

        if sender == self.ht_sat_light_box {
            let (local_x, local_y, w, h) = context.ht_manager.translate_client_to_tree_local(
                self.ht_sat_light_box,
                args.client_pos.x,
                args.client_pos.y,
                args.client_size.width,
                args.client_size.height,
            );
            let local_x = local_x.clamp(0.0, w);
            let local_y = local_y.clamp(0.0, h);
            self.move_cursor(
                local_x,
                local_y,
                context.composite_tree,
                context.view_render_queue,
            );

            return EventContinueControl::STOP_PROPAGATION;
        }

        if sender == self.ht_alpha_slider {
            let (local_x, _, w, _) = context.ht_manager.translate_client_to_tree_local(
                self.ht_alpha_slider,
                args.client_pos.x,
                args.client_pos.y,
                args.client_size.width,
                args.client_size.height,
            );
            let new_alpha = local_x.clamp(0.0, w) / w;
            self.current_alpha.set(new_alpha);
            self.color_changed(context.composite_tree, context.view_render_queue);
            context
                .composite_tree
                .get_mut(self.ct_alpha_slider_thumb)
                .offset[0] = AnimatableFloat::Value(
                new_alpha * w - ColorPickerView::ALPHA_SLIDER_THUMB_THICKNESS * 0.5,
            );
            context
                .composite_tree
                .mark_dirty(self.ct_alpha_slider_thumb);

            return EventContinueControl::STOP_PROPAGATION;
        }

        if sender == self.hex_text_input_ht {
            return self
                .hex_text_input_view
                .entity()
                .on_drag_move(sender, context, args);
        }

        EventContinueControl::empty()
    }

    fn on_pointer_up(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        if sender == self.ht_root {
            self.ring_selecting.set(false);
            return EventContinueControl::STOP_PROPAGATION
                | EventContinueControl::RELEASE_CAPTURE_ELEMENT;
        }

        if sender == self.ht_sat_light_box {
            self.box_selecting.set(false);
            return EventContinueControl::STOP_PROPAGATION
                | EventContinueControl::RELEASE_CAPTURE_ELEMENT;
        }

        if sender == self.ht_alpha_slider {
            self.alpha_sliding.set(false);
            return EventContinueControl::STOP_PROPAGATION
                | EventContinueControl::RELEASE_CAPTURE_ELEMENT;
        }

        if sender == self.hex_text_input_ht {
            return self
                .hex_text_input_view
                .entity()
                .on_pointer_up(sender, context, args);
        }

        EventContinueControl::empty()
    }
}
impl ColorPickerEventHandler {
    fn move_cursor<E>(
        &self,
        x: f32,
        y: f32,
        composite_tree: &mut CompositeTree<E>,
        view_render_queue: &mut ViewRenderQueue,
    ) {
        self.current_light
            .set(1.0 - y / self.sat_light_box_size.height);
        self.current_saturation
            .set(x / self.sat_light_box_size.width);
        self.color_changed(composite_tree, view_render_queue);

        let ct_pointer = composite_tree.get_mut(self.ct_pointer);
        ct_pointer.offset = [
            AnimatableFloat::Value(x - ColorPickerView::POINTER_SIZE * 0.5),
            AnimatableFloat::Value(y - ColorPickerView::POINTER_SIZE * 0.5),
        ];
        composite_tree.mark_dirty(self.ct_pointer);
    }

    fn select_hue<E>(
        &self,
        hue: f32,
        composite_tree: &mut CompositeTree<E>,
        view_render_queue: &mut ViewRenderQueue,
    ) {
        self.current_hue.set(hue);
        self.color_changed(composite_tree, view_render_queue);

        let r = hue_to_rgb_wave(hue + 120.0);
        let g = hue_to_rgb_wave(hue);
        let b = hue_to_rgb_wave(hue - 120.0);

        composite_tree.get_mut(self.ct_sat_light_box).composite_mode =
            CompositeMode::ColorPickerGradientBox(AnimatableColor::Value([r, g, b, 1.0]));
        composite_tree.mark_dirty(self.ct_sat_light_box);
    }

    fn color_changed<E>(
        &self,
        composite_tree: &mut CompositeTree<E>,
        view_render_queue: &mut ViewRenderQueue,
    ) {
        const fn lerp(a: f32, b: f32, t: f32) -> f32 {
            a + (b - a) * t
        }

        let r = lerp(
            1.0,
            hue_to_rgb_wave(self.current_hue.get() + 120.0),
            self.current_saturation.get(),
        ) * self.current_light.get();
        let g = lerp(
            1.0,
            hue_to_rgb_wave(self.current_hue.get() - 0.0),
            self.current_saturation.get(),
        ) * self.current_light.get();
        let b = lerp(
            1.0,
            hue_to_rgb_wave(self.current_hue.get() - 120.0),
            self.current_saturation.get(),
        ) * self.current_light.get();
        let rgba = gen_rgba(
            (r * 255.0) as _,
            (g * 255.0) as _,
            (b * 255.0) as _,
            (self.current_alpha.get() * 255.0) as _,
        );

        self.hex_text_input_view
            .entity()
            .lazy_update_and_schedule(view_render_queue, |e| e.set_content(Self::fmt(rgba)));

        composite_tree.set_gradient(
            self.alpha_slider_content_gradient,
            Gradient::Linear {
                start_color: [r, g, b, 0.0],
                end_color: [r, g, b, 1.0],
                start_pos_relative: [0.0, 0.0],
                end_pos_relative: [1.0, 0.0],
            },
        );

        if let Some(e) = self.backing_store.upgrade() {
            e.new_value(rgba, view_render_queue);
        }
    }

    fn set_by_color<E>(&self, color: u32, composite_tree: &mut CompositeTree<E>) {
        let r = color as u8 as f32 / 255.0;
        let g = (color >> 8) as u8 as f32 / 255.0;
        let b = (color >> 16) as u8 as f32 / 255.0;
        let a = (color >> 24) as u8 as f32 / 255.0;

        let max = r.max(g).max(b);
        let min = r.min(g).min(b);
        let d = max - min;
        let hue = if d == 0.0 {
            0.0
        } else if max == r {
            60.0 * (g - b) / d
        } else if max == g {
            120.0 + 60.0 * (b - r) / d
        } else {
            240.0 + 60.0 * (r - g) / d
        };
        let hue = if hue < 0.0 { 360.0 + hue } else { hue };
        let saturation = (max - min) / max;
        let light = max;

        self.current_hue.set(hue);
        self.current_light.set(light);
        self.current_saturation.set(saturation);
        self.current_alpha.set(a);

        composite_tree.get_mut(self.ct_sat_light_box).composite_mode =
            CompositeMode::ColorPickerGradientBox(AnimatableColor::Value([
                hue_to_rgb_wave(hue + 120.0),
                hue_to_rgb_wave(hue),
                hue_to_rgb_wave(hue - 120.0),
                1.0,
            ]));
        composite_tree.mark_dirty(self.ct_sat_light_box);

        let pointer_x = saturation * self.sat_light_box_size.width;
        let pointer_y = (1.0 - light) * self.sat_light_box_size.height;
        let ct_pointer = composite_tree.get_mut(self.ct_pointer);
        ct_pointer.offset = [
            AnimatableFloat::Value(pointer_x - ColorPickerView::POINTER_SIZE * 0.5),
            AnimatableFloat::Value(pointer_y - ColorPickerView::POINTER_SIZE * 0.5),
        ];
        composite_tree.mark_dirty(self.ct_pointer);
        composite_tree.get_mut(self.ct_alpha_slider_thumb).offset[0] =
            AnimatableFloat::Value(a * 128.0 - ColorPickerView::ALPHA_SLIDER_THUMB_THICKNESS * 0.5);
        composite_tree.mark_dirty(self.ct_alpha_slider_thumb);

        composite_tree.set_gradient(
            self.alpha_slider_content_gradient,
            Gradient::Linear {
                start_color: [r, g, b, 0.0],
                end_color: [r, g, b, 1.0],
                start_pos_relative: [0.0, 0.0],
                end_pos_relative: [1.0, 0.0],
            },
        );
    }

    fn parse(text: &str) -> Option<u32> {
        const fn parse_ascii_hexdigit(c: u8) -> Option<u8> {
            match c {
                b'0'..=b'9' => Some(c - b'0'),
                b'A'..=b'F' => Some(c - b'A' + 10),
                b'a'..=b'f' => Some(c - b'a' + 10),
                _ => None,
            }
        }

        match text.as_bytes() {
            // RGB
            &[r, g, b] => {
                let r = parse_ascii_hexdigit(r)?;
                let g = parse_ascii_hexdigit(g)?;
                let b = parse_ascii_hexdigit(b)?;

                Some(gen_rgba(r | r << 4, g | g << 4, b | b << 4, 255))
            }
            // RGBA
            &[r, g, b, a] => {
                let r = parse_ascii_hexdigit(r)?;
                let g = parse_ascii_hexdigit(g)?;
                let b = parse_ascii_hexdigit(b)?;
                let a = parse_ascii_hexdigit(a)?;

                Some(gen_rgba(r | r << 4, g | g << 4, b | b << 4, a | a << 4))
            }
            // RRGGBB
            &[r0, r1, g0, g1, b0, b1] => {
                let r = parse_ascii_hexdigit(r1)? | parse_ascii_hexdigit(r0)? << 4;
                let g = parse_ascii_hexdigit(g1)? | parse_ascii_hexdigit(g0)? << 4;
                let b = parse_ascii_hexdigit(b1)? | parse_ascii_hexdigit(b0)? << 4;

                Some(gen_rgba(r, g, b, 255))
            }
            // RRGGBBAA
            &[r0, r1, g0, g1, b0, b1, a0, a1] => {
                let r = parse_ascii_hexdigit(r1)? | parse_ascii_hexdigit(r0)? << 4;
                let g = parse_ascii_hexdigit(g1)? | parse_ascii_hexdigit(g0)? << 4;
                let b = parse_ascii_hexdigit(b1)? | parse_ascii_hexdigit(b0)? << 4;
                let a = parse_ascii_hexdigit(a1)? | parse_ascii_hexdigit(a0)? << 4;

                Some(gen_rgba(r, g, b, a))
            }
            // unknown
            _ => None,
        }
    }

    fn fmt(rgba: u32) -> String {
        let r = rgba as u8;
        let g = (rgba >> 8) as u8;
        let b = (rgba >> 16) as u8;
        let a = (rgba >> 24) as u8;

        format!("{r:02X}{g:02X}{b:02X}{a:02X}")
    }

    fn confirm_direct_input<E>(
        &self,
        composite_tree: &mut CompositeTree<E>,
        view_render_queue: &mut ViewRenderQueue,
    ) {
        let backing_store = self
            .backing_store
            .upgrade()
            .expect("ColorPickerView has defunct");

        let current_value = backing_store.value();
        let new_value =
            Self::parse(&*self.hex_text_input_view.entity().content()).unwrap_or(current_value);

        // HitTestTreeへの変更がはいるので遅延させる
        self.hex_text_input_view
            .entity()
            .lazy_update_and_schedule(view_render_queue, |e| {
                e.perform_external_state_update(|st| st.set_content(Self::fmt(new_value)))
            });

        if current_value != new_value {
            // notify changed
            self.set_by_color(new_value, composite_tree);
            backing_store.new_value(new_value, view_render_queue);
        }
    }

    fn cancel_direct_input(&self, view_render_queue: &mut ViewRenderQueue) {
        let backing_store = self
            .backing_store
            .upgrade()
            .expect("ColorPickerView has defunct");
        self.hex_text_input_view
            .entity()
            .lazy_update_and_schedule(view_render_queue, |e| {
                e.perform_external_state_update(|st| {
                    st.set_content(Self::fmt(backing_store.value()))
                })
            });
    }
}

const fn hue_to_rgb_wave(hue: f32) -> f32 {
    // generate ／￣￣＼＿＿ wave
    let phase = (hue / 60.0) % 6.0;
    match phase {
        0.0..1.0 => phase,
        1.0..3.0 => 1.0,
        3.0..4.0 => 4.0 - phase,
        _ => 0.0,
    }
}

const fn gen_rgba(r: u8, g: u8, b: u8, a: u8) -> u32 {
    r as u32 | ((g as u32) << 8) | ((b as u32) << 16) | ((a as u32) << 24)
}
