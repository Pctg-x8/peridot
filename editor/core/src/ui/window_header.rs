use core::cell::Cell;
use std::rc::Rc;

use crate::{
    SyncEvent,
    input::{
        EventContinueControl, InputEventContext,
        hittest::{
            HitTestTreeActionHandler, HitTestTreeData, HitTestTreeRef, PointerActionArgs,
            PointerButtonActionArgs,
        },
    },
    rendering::{
        MainThreadTextureIDIssuer, Normalized2DStaticMeshTexture,
        Normalized2DStaticMeshTextureLazyInit, RenderMessageSender, TextureID,
        composite::{
            AnimatableColor, AnimatableFloat, AnimationCurve, CompositeMode, CompositeRect,
            CompositeRectScaleFactor, CompositeRectText, CompositeRectTextHorizontalAlignment,
            CompositeRectTextRun, CompositeRectTextVerticalAlignment, CompositeTexture,
            CompositeTree, CompositeTreeRef, TextureMappingMode, TextureType,
        },
        text::FontID,
    },
    uikit::{
        RenderContext, ViewIdentifier, ViewInstanceQueryableMut, ViewRegisterable,
        ViewRelationControllable, ViewRenderer,
    },
    utils::{LogicalUnit, Rect, Size},
};

pub enum Caption {
    Main { project_name: String },
    Sub,
}

#[derive(Default)]
pub struct ComponentInit {
    pub with_system_command_buttons: bool,
}

pub struct Component {
    main: ViewIdentifier,
    command_buttons: Option<[ViewIdentifier; 3]>,
}
impl Component {
    pub fn new(
        caption: Caption,
        init: ComponentInit,
        ctx: &mut (impl ViewRegisterable + ViewRelationControllable + ?Sized),
    ) -> Self {
        let main = ctx.construct_view(|_| Box::new(View::new(caption)));
        let command_buttons = if init.with_system_command_buttons {
            let close_button = ctx.construct_view(|_| {
                Box::new(SystemCommandButtonView::new(0.0, SystemCommand::Close))
            });
            let maximize_restore_button = ctx.construct_view(|_| {
                Box::new(SystemCommandButtonView::new(
                    SystemCommandButtonView::WIDTH,
                    SystemCommand::Maximize,
                ))
            });
            let minimize_button = ctx.construct_view(|_| {
                Box::new(SystemCommandButtonView::new(
                    SystemCommandButtonView::WIDTH * 2.0,
                    SystemCommand::Minimize,
                ))
            });

            ctx.view_set_parent(close_button, main);
            ctx.view_set_parent(maximize_restore_button, main);
            ctx.view_set_parent(minimize_button, main);

            Some([close_button, maximize_restore_button, minimize_button])
        } else {
            None
        };

        Self {
            main,
            command_buttons,
        }
    }

    pub const fn root_view(&self) -> ViewIdentifier {
        self.main
    }

    pub fn set_maximize_state(
        &self,
        is_maximized: bool,
        ctx: &mut (impl ViewInstanceQueryableMut + ViewRenderer + ?Sized),
    ) {
        if let Some([_, v, _]) = self.command_buttons {
            ctx.view_instance_mut::<SystemCommandButtonView>(v)
                .expect("query failed")
                .replace_cmd(if is_maximized {
                    SystemCommand::Restore
                } else {
                    SystemCommand::Maximize
                });
            ctx.schedule_view_render(v);
        }
    }
}

pub struct View {
    entity: Option<ViewEntity>,
    caption: Caption,
}
impl View {
    #[cfg(target_os = "macos")]
    pub const THICKNESS: f32 = 32.0;
    #[cfg(not(target_os = "macos"))]
    pub const THICKNESS: f32 = 24.0;

    fn new<'a>(init_caption: Caption) -> Self {
        Self {
            entity: None,
            caption: init_caption,
        }
    }
}
impl crate::uikit::View for View {
    fn render(
        &mut self,
        _layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
    ) -> crate::uikit::ViewRenderElements {
        let e = match self.entity {
            Some(ref e) => e,
            None => {
                // first render
                let ct_root = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    relative_size_adjustment: [1.0, 0.0],
                    size: [
                        AnimatableFloat::Value(0.0),
                        AnimatableFloat::Value(Self::THICKNESS),
                    ],
                    text: match self.caption {
                        Caption::Main { ref project_name } => Some(CompositeRectText {
                            runs: vec![
                                CompositeRectTextRun {
                                    font_id: FontID::UIDefault,
                                    content: "Peridot Marble Editor".into(),
                                    color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                                    ..Default::default()
                                },
                                CompositeRectTextRun {
                                    font_id: FontID::UITitleProjectName,
                                    content: project_name.clone(),
                                    color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                                    spacing_inline_start: 4.0,
                                    ..Default::default()
                                },
                            ],
                            horizontal_alignment: CompositeRectTextHorizontalAlignment::Middle,
                            vertical_alignment: CompositeRectTextVerticalAlignment::Middle,
                            ..Default::default()
                        }),
                        Caption::Sub => Some(CompositeRectText {
                            runs: vec![CompositeRectTextRun {
                                font_id: FontID::UIDefault,
                                content: "EditorWindow".into(),
                                color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                                ..Default::default()
                            }],
                            horizontal_alignment: CompositeRectTextHorizontalAlignment::Middle,
                            vertical_alignment: CompositeRectTextVerticalAlignment::Middle,
                            ..Default::default()
                        }),
                    },
                    ..Default::default()
                });
                let ht_root = ctx.ht_manager.create(HitTestTreeData {
                    width_adjustment_factor: 1.0,
                    height: Self::THICKNESS,
                    role: Some(crate::input::hittest::Role::TitleBar),
                    ..Default::default()
                });

                &*self.entity.insert(ViewEntity { ct_root, ht_root })
            }
        };

        crate::uikit::ViewRenderElements {
            composite_tree: Some(e.ct_root),
            hit_tree: Some(e.ht_root),
            ..crate::uikit::ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut crate::uikit::TeardownContext) {
        let Some(entity) = self.entity.take() else {
            // not rendered
            return;
        };

        ctx.mount_context.composite_tree.free_all(entity.ct_root);
        ctx.mount_context.ht_manager.free_all(entity.ht_root);
    }

    fn measure_preferred_content_size(
        &self,
        ctx: &mut crate::uikit::MeasureContext,
    ) -> Size<LogicalUnit> {
        Size::new_logical(0.0, Self::THICKNESS)
    }

    fn create_new_layout_layer(&self) -> bool {
        true
    }
}

struct ViewEntity {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
}

struct SystemCommandButtonActionHandler {
    ht_root: HitTestTreeRef,
    ct_hover: CompositeTreeRef,
    cmd: core::cell::Cell<SystemCommand>,
    hovering: core::cell::Cell<bool>,
    pressing: core::cell::Cell<bool>,
    is_dirty: core::cell::Cell<bool>,
}

struct SystemCommandButtonView {
    right_offset: f32,
    entity: Option<Rc<SystemCommandButtonViewEntity>>,
    cmd: SystemCommand,
}
impl SystemCommandButtonView {
    const WIDTH: f32 = 48.0;

    fn new(right_offset: f32, init_cmd: SystemCommand) -> Self {
        Self {
            right_offset,
            entity: None,
            cmd: init_cmd,
        }
    }

    fn replace_cmd(&mut self, cmd: SystemCommand) {
        self.cmd = cmd;
    }
}
impl crate::uikit::View for SystemCommandButtonView {
    fn render(
        &mut self,
        layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
    ) -> crate::uikit::ViewRenderElements {
        let e = match self.entity {
            Some(ref e) => {
                if e.cmd.replace(self.cmd) != self.cmd {
                    // cmd changeA
                    ctx.composite_tree
                        .begin_mod_chain(e.ct_icon)
                        .composite_mode(CompositeMode::ColorTint(
                            AnimatableColor::Value([0.9, 0.9, 0.9, 1.0]),
                            CompositeTexture {
                                id: self.cmd.texture_id(
                                    ctx.main_thread_texture_id_issuer,
                                    ctx.system_link.rt_sender(),
                                ),
                                r#type: TextureType::Mask,
                                mapping: TextureMappingMode::Stretch,
                                slice_borders: [0.0; 4],
                            },
                        ))
                        .apply();
                    ctx.ht_manager.get_data_mut(e.ht_root).role = self.cmd.role();
                }

                e
            }
            None => {
                // first render
                let ct_root = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    relative_offset_adjustment: [1.0, 0.0],
                    offset: [
                        AnimatableFloat::Value(-self.right_offset - Self::WIDTH),
                        AnimatableFloat::Value(0.0),
                    ],
                    relative_size_adjustment: [0.0, 1.0],
                    size: [
                        AnimatableFloat::Value(Self::WIDTH),
                        AnimatableFloat::Value(0.0),
                    ],
                    ..Default::default()
                });
                let ct_hover = ctx.composite_tree.create(CompositeRect {
                    relative_size_adjustment: [1.0, 1.0],
                    has_bitmap: true,
                    composite_mode: CompositeMode::FillColor(AnimatableColor::Value(
                        match self.cmd {
                            SystemCommand::Close => [1.0, 0.0, 0.0, 1.0],
                            _ => [1.0, 1.0, 1.0, 0.5],
                        },
                    )),
                    opacity: AnimatableFloat::Value(0.0),
                    ..Default::default()
                });
                let ct_icon = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    offset: [
                        AnimatableFloat::Value(-ICON_SIZE * 0.5),
                        AnimatableFloat::Value(-ICON_SIZE * 0.5),
                    ],
                    relative_offset_adjustment: [0.5, 0.5],
                    size: [
                        AnimatableFloat::Value(ICON_SIZE),
                        AnimatableFloat::Value(ICON_SIZE),
                    ],
                    has_bitmap: true,
                    composite_mode: CompositeMode::ColorTint(
                        AnimatableColor::Value([0.9, 0.9, 0.9, 1.0]),
                        CompositeTexture {
                            id: self.cmd.texture_id(
                                ctx.main_thread_texture_id_issuer,
                                ctx.system_link.rt_sender(),
                            ),
                            r#type: TextureType::Mask,
                            mapping: TextureMappingMode::Stretch,
                            slice_borders: [0.0; 4],
                        },
                    ),
                    ..Default::default()
                });

                ctx.composite_tree.add_child(ct_root, ct_hover);
                ctx.composite_tree.add_child(ct_root, ct_icon);

                let ht_root = ctx.ht_manager.create(HitTestTreeData {
                    left: -self.right_offset - Self::WIDTH,
                    left_adjustment_factor: 1.0,
                    width: Self::WIDTH,
                    height_adjustment_factor: 1.0,
                    role: self.cmd.role(),
                    ..Default::default()
                });

                let entity = Rc::new(SystemCommandButtonViewEntity {
                    ct_root,
                    ct_icon,
                    ct_hover,
                    ht_root,
                    cmd: Cell::new(self.cmd),
                    hovering: Cell::new(false),
                    pressing: Cell::new(false),
                    is_dirty: Cell::new(false),
                });
                ctx.ht_manager.set_action_handler(ht_root, &entity);

                &*self.entity.insert(entity)
            }
        };

        crate::uikit::ViewRenderElements {
            composite_tree: Some(e.ct_root),
            hit_tree: Some(e.ht_root),
            ..crate::uikit::ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut crate::uikit::TeardownContext) {
        let Some(entity) = self.entity.take() else {
            // not rendered
            return;
        };

        ctx.mount_context.composite_tree.free_all(entity.ct_root);
        ctx.mount_context.ht_manager.free_all(entity.ht_root);
    }

    fn measure_preferred_content_size(
        &self,
        ctx: &mut crate::uikit::MeasureContext,
    ) -> Size<LogicalUnit> {
        Size::new_logical(Self::WIDTH, View::THICKNESS)
    }
}

struct SystemCommandButtonViewEntity {
    ct_root: CompositeTreeRef,
    ct_icon: CompositeTreeRef,
    ct_hover: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    cmd: Cell<SystemCommand>,
    hovering: Cell<bool>,
    pressing: Cell<bool>,
    is_dirty: Cell<bool>,
}
impl HitTestTreeActionHandler for SystemCommandButtonViewEntity {
    fn on_pointer_enter(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerActionArgs,
    ) -> EventContinueControl {
        self.hovering.set(true);
        self.is_dirty.set(true);
        self.update(context.composite_tree, context.current_sec);

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_leave(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerActionArgs,
    ) -> EventContinueControl {
        self.hovering.set(false);
        self.pressing.set(false);
        self.is_dirty.set(true);
        self.update(context.composite_tree, context.current_sec);

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_down(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::STOP_PROPAGATION | EventContinueControl::CAPTURE_ELEMENT
    }

    fn on_pointer_up(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::STOP_PROPAGATION | EventContinueControl::RELEASE_CAPTURE_ELEMENT
    }

    fn on_click(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        let mounted_window = context
            .ht_manager
            .query_root_window(self.ht_root)
            .expect("not mounted");

        match self.cmd.get() {
            SystemCommand::Close => mounted_window.on_click_sys_close_button(),
            SystemCommand::Minimize => mounted_window.on_click_sys_minimize_button(),
            SystemCommand::Maximize => mounted_window.on_click_sys_maximize_button(),
            SystemCommand::Restore => mounted_window.on_click_sys_restore_button(),
        }

        EventContinueControl::STOP_PROPAGATION
    }
}
impl SystemCommandButtonViewEntity {
    fn update(&self, ct: &mut CompositeTree<SyncEvent>, current_sec: f32) {
        if self.is_dirty.replace(false) {
            ct.get_mut(self.ct_hover).opacity = if self.hovering.get() {
                AnimatableFloat::Animated {
                    from_value: 0.0,
                    to_value: 1.0,
                    sec_duration: (current_sec..current_sec + 0.1).into(),
                    curve: AnimationCurve::Linear,
                    event_on_complete: None,
                }
            } else {
                AnimatableFloat::Animated {
                    from_value: 1.0,
                    to_value: 0.0,
                    sec_duration: (current_sec..current_sec + 0.1).into(),
                    curve: AnimationCurve::Linear,
                    event_on_complete: None,
                }
            };

            ct.mark_dirty(self.ct_hover);
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SystemCommand {
    Close,
    Minimize,
    Maximize,
    Restore,
}
impl SystemCommand {
    #[inline(always)]
    const fn role(&self) -> Option<crate::input::hittest::Role> {
        match self {
            Self::Close => Some(crate::input::hittest::Role::CloseButton),
            Self::Maximize => Some(crate::input::hittest::Role::MaximizeButton),
            Self::Minimize => Some(crate::input::hittest::Role::MinimizeButton),
            Self::Restore => Some(crate::input::hittest::Role::RestoreButton),
        }
    }

    #[inline(always)]
    fn texture_id(
        &self,
        mt_texid_issuer: &mut MainThreadTextureIDIssuer,
        rt_sender: &RenderMessageSender,
    ) -> TextureID {
        match self {
            Self::Close => SYSCMD_CLOSE_ICON.get(mt_texid_issuer, rt_sender),
            Self::Minimize => SYSCMD_MINIMIZE_ICON.get(mt_texid_issuer, rt_sender),
            Self::Maximize => SYSCMD_MAXIMIZE_ICON.get(mt_texid_issuer, rt_sender),
            Self::Restore => SYSCMD_RESTORE_ICON.get(mt_texid_issuer, rt_sender),
        }
    }
}

static SYSCMD_CLOSE_ICON: Normalized2DStaticMeshTextureLazyInit =
    Normalized2DStaticMeshTextureLazyInit::new(Normalized2DStaticMeshTexture {
        vertices: &[
            [0.0 + 0.5 / ICON_SIZE, 0.0 - 0.5 / ICON_SIZE],
            [0.0 - 0.5 / ICON_SIZE, 0.0 + 0.5 / ICON_SIZE],
            [1.0 - 0.5 / ICON_SIZE, 1.0 + 0.5 / ICON_SIZE],
            [1.0 + 0.5 / ICON_SIZE, 1.0 - 0.5 / ICON_SIZE],
            [1.0 + 0.5 / ICON_SIZE, 0.0 + 0.5 / ICON_SIZE],
            [1.0 - 0.5 / ICON_SIZE, 0.0 - 0.5 / ICON_SIZE],
            [0.0 - 0.5 / ICON_SIZE, 1.0 - 0.5 / ICON_SIZE],
            [0.0 + 0.5 / ICON_SIZE, 1.0 + 0.5 / ICON_SIZE],
        ],
        indices: &[0, 1, 2, 2, 3, 0, 4, 5, 6, 6, 7, 4],
        width: ICON_SIZE as _,
        height: ICON_SIZE as _,
    });
static SYSCMD_MINIMIZE_ICON: Normalized2DStaticMeshTextureLazyInit =
    Normalized2DStaticMeshTextureLazyInit::new(Normalized2DStaticMeshTexture {
        vertices: &[
            [0.0, 1.0 - 1.5 / ICON_SIZE],
            [0.0, 1.0],
            [1.0, 1.0],
            [1.0, 1.0 - 1.5 / ICON_SIZE],
        ],
        indices: &[0, 1, 2, 2, 3, 0],
        width: ICON_SIZE as _,
        height: ICON_SIZE as _,
    });
static SYSCMD_MAXIMIZE_ICON: Normalized2DStaticMeshTextureLazyInit =
    Normalized2DStaticMeshTextureLazyInit::new(Normalized2DStaticMeshTexture {
        vertices: &[
            [0.0, 0.0],
            [0.0 + 1.5 / ICON_SIZE, 0.0 + 1.5 / ICON_SIZE],
            [1.0, 0.0],
            [1.0 - 1.5 / ICON_SIZE, 0.0 + 1.5 / ICON_SIZE],
            [1.0, 1.0],
            [1.0 - 1.5 / ICON_SIZE, 1.0 - 1.5 / ICON_SIZE],
            [0.0, 1.0],
            [0.0 + 1.5 / ICON_SIZE, 1.0 - 1.5 / ICON_SIZE],
        ],
        indices: &[
            0, 2, 3, 3, 1, 0, 2, 4, 5, 5, 3, 2, 4, 6, 7, 7, 5, 4, 6, 0, 1, 1, 7, 6,
        ],
        width: ICON_SIZE as _,
        height: ICON_SIZE as _,
    });
static SYSCMD_RESTORE_ICON: Normalized2DStaticMeshTextureLazyInit =
    Normalized2DStaticMeshTextureLazyInit::new(Normalized2DStaticMeshTexture {
        vertices: &[
            [0.0, 2.0 / ICON_SIZE],
            [1.0 - 2.0 / ICON_SIZE, 2.0 / ICON_SIZE],
            [1.0 - 2.0 / ICON_SIZE, 1.0],
            [0.0, 1.0],
            [1.0 / ICON_SIZE, 3.0 / ICON_SIZE],
            [1.0 - 3.0 / ICON_SIZE, 3.0 / ICON_SIZE],
            [1.0 - 3.0 / ICON_SIZE, 1.0 - 1.0 / ICON_SIZE],
            [1.0 / ICON_SIZE, 1.0 - 1.0 / ICON_SIZE],
            [3.0 / ICON_SIZE, 0.0],
            [1.0, 0.0],
            [1.0, 1.0 - 3.0 / ICON_SIZE],
            [3.0 / ICON_SIZE, 1.0 / ICON_SIZE],
            [1.0 - 1.0 / ICON_SIZE, 1.0 / ICON_SIZE],
            [1.0 - 1.0 / ICON_SIZE, 1.0 - 3.0 / ICON_SIZE],
        ],
        indices: &[
            0, 1, 4, 4, 1, 5, 1, 2, 5, 5, 2, 6, 2, 3, 6, 6, 3, 7, 3, 0, 7, 7, 0, 4, 8, 9, 11, 11,
            9, 12, 9, 10, 12, 12, 10, 13,
        ],
        width: ICON_SIZE as _,
        height: ICON_SIZE as _,
    });

const ICON_SIZE: f32 = 10.0;
