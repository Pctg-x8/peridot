use std::rc::Rc;

use crate::{
    SyncEvent,
    input::{
        EventContinueControl, InputEventContext,
        hittest::{
            HitTestTreeActionHandler, HitTestTreeData, HitTestTreeManager, HitTestTreeRef,
            PointerActionArgs, PointerButtonActionArgs,
        },
    },
    rendering::{
        MainThreadTextureIDIssuer, Normalized2DStaticMeshTexture, RenderMessage,
        RenderMessageSender, TextureID,
        composite::{
            AnimatableColor, AnimatableFloat, AnimationCurve, CompositeMode, CompositeRect,
            CompositeRectText, CompositeRectTextHorizontalAlignment, CompositeRectTextRun,
            CompositeRectTextVerticalAlignment, CompositeTree, CompositeTreeRef,
        },
        text::FontID,
    },
    uikit::{MountContext, MountTarget, ViewInitContext},
};

pub enum Caption {
    Main { project_name: String },
    Sub,
}

pub struct View {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    command_buttons: Option<[SystemCommandButtonView; 3]>,
}
impl View {
    #[cfg(target_os = "macos")]
    pub const THICKNESS: f32 = 32.0;
    #[cfg(not(target_os = "macos"))]
    pub const THICKNESS: f32 = 24.0;

    pub fn new<'a>(
        init_ctx: &mut ViewInitContext,
        init_caption: Caption,
        texture_id_set: &SystemCommandTextureIDSet,
        needs_system_command_buttons: bool,
    ) -> Self {
        let ct_root = init_ctx.mount_context.composite_tree.create(CompositeRect {
            has_bitmap: true,
            base_scale_factor: init_ctx.ui_scale_factor,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                1.0, 1.0, 1.0, 0.125,
            ])),
            relative_size_adjustment: [1.0, 0.0],
            size: [
                AnimatableFloat::Value(0.0),
                AnimatableFloat::Value(Self::THICKNESS),
            ],
            text: match init_caption {
                Caption::Main { project_name } => Some(CompositeRectText {
                    runs: vec![
                        CompositeRectTextRun {
                            font_id: FontID::UIDefault,
                            content: "Peridot Marble Editor".into(),
                            color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                            ..Default::default()
                        },
                        CompositeRectTextRun {
                            font_id: FontID::UITitleProjectName,
                            content: project_name,
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
        let ht_root = init_ctx.ht_manager.create(HitTestTreeData {
            width_adjustment_factor: 1.0,
            height: Self::THICKNESS,
            role: Some(crate::input::hittest::Role::TitleBar),
            ..Default::default()
        });
        let command_buttons = if needs_system_command_buttons {
            let views = [
                SystemCommandButtonView::new(init_ctx, texture_id_set, 0.0, SystemCommand::Close),
                SystemCommandButtonView::new(
                    init_ctx,
                    texture_id_set,
                    SystemCommandButtonView::WIDTH,
                    SystemCommand::Maximize,
                ),
                SystemCommandButtonView::new(
                    init_ctx,
                    texture_id_set,
                    SystemCommandButtonView::WIDTH * 2.0,
                    SystemCommand::Minimize,
                ),
            ];

            views[0].mount(init_ctx, ct_root, ht_root);
            views[1].mount(init_ctx, ct_root, ht_root);
            views[2].mount(init_ctx, ct_root, ht_root);

            Some(views)
        } else {
            None
        };

        Self {
            ct_root,
            ht_root,
            command_buttons,
        }
    }

    pub fn mount(&self, ctx: &mut MountContext, parent: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree.add_child(parent.ct_root(), self.ct_root);
        ctx.ht_manager.add_child(parent.ht_root(), self.ht_root);
    }

    pub fn rescale(
        &self,
        scale_factor: f32,
        composite_tree: &mut CompositeTree<SyncEvent>,
        texture_id_set: &SystemCommandTextureIDSet,
    ) {
        composite_tree.get_mut(self.ct_root).base_scale_factor = scale_factor;
        composite_tree.mark_dirty_all(self.ct_root);
        if let Some(ref xs) = self.command_buttons {
            for c in xs {
                c.rescale(composite_tree, texture_id_set, scale_factor);
            }
        }
    }

    pub fn set_maximize_state(
        &self,
        is_maximized: bool,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager<'_>,
        texture_id_set: &SystemCommandTextureIDSet,
    ) {
        if let Some(ref xs) = self.command_buttons {
            xs[1].replace_cmd(
                composite_tree,
                ht_manager,
                texture_id_set,
                if is_maximized {
                    SystemCommand::Restore
                } else {
                    SystemCommand::Maximize
                },
            );
        }
    }
}

struct SystemCommandButtonActionHandler {
    ht_root: HitTestTreeRef,
    ct_hover: CompositeTreeRef,
    cmd: core::cell::Cell<SystemCommand>,
    hovering: core::cell::Cell<bool>,
    pressing: core::cell::Cell<bool>,
    is_dirty: core::cell::Cell<bool>,
}
impl HitTestTreeActionHandler for SystemCommandButtonActionHandler {
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
impl SystemCommandButtonActionHandler {
    fn update(&self, ct: &mut CompositeTree<SyncEvent>, current_sec: f32) {
        if self.is_dirty.replace(false) {
            ct.get_mut(self.ct_hover).opacity = if self.hovering.get() {
                AnimatableFloat::Animated {
                    from_value: 0.0,
                    to_value: 1.0,
                    start_sec: current_sec,
                    end_sec: current_sec + 0.1,
                    curve: AnimationCurve::Linear,
                    event_on_complete: None,
                }
            } else {
                AnimatableFloat::Animated {
                    from_value: 1.0,
                    to_value: 0.0,
                    start_sec: current_sec,
                    end_sec: current_sec + 0.1,
                    curve: AnimationCurve::Linear,
                    event_on_complete: None,
                }
            };

            ct.mark_dirty(self.ct_hover);
        }
    }
}

struct SystemCommandButtonView {
    ct_root: CompositeTreeRef,
    ct_icon: CompositeTreeRef,
    ct_hover: CompositeTreeRef,
    action_handler: Rc<SystemCommandButtonActionHandler>,
}
impl SystemCommandButtonView {
    const ICON_SIZE: f32 = 10.0;
    const WIDTH: f32 = 48.0;

    const CLOSE_ICON: Normalized2DStaticMeshTexture = Normalized2DStaticMeshTexture {
        vertices: &[
            [0.0 + 0.5 / Self::ICON_SIZE, 0.0 - 0.5 / Self::ICON_SIZE],
            [0.0 - 0.5 / Self::ICON_SIZE, 0.0 + 0.5 / Self::ICON_SIZE],
            [1.0 - 0.5 / Self::ICON_SIZE, 1.0 + 0.5 / Self::ICON_SIZE],
            [1.0 + 0.5 / Self::ICON_SIZE, 1.0 - 0.5 / Self::ICON_SIZE],
            [1.0 + 0.5 / Self::ICON_SIZE, 0.0 + 0.5 / Self::ICON_SIZE],
            [1.0 - 0.5 / Self::ICON_SIZE, 0.0 - 0.5 / Self::ICON_SIZE],
            [0.0 - 0.5 / Self::ICON_SIZE, 1.0 - 0.5 / Self::ICON_SIZE],
            [0.0 + 0.5 / Self::ICON_SIZE, 1.0 + 0.5 / Self::ICON_SIZE],
        ],
        indices: &[0, 1, 2, 2, 3, 0, 4, 5, 6, 6, 7, 4],
        width: Self::ICON_SIZE as _,
        height: Self::ICON_SIZE as _,
    };
    const MINIMIZE_ICON: Normalized2DStaticMeshTexture = Normalized2DStaticMeshTexture {
        vertices: &[
            [0.0, 1.0 - 1.5 / Self::ICON_SIZE],
            [0.0, 1.0],
            [1.0, 1.0],
            [1.0, 1.0 - 1.5 / Self::ICON_SIZE],
        ],
        indices: &[0, 1, 2, 2, 3, 0],
        width: Self::ICON_SIZE as _,
        height: Self::ICON_SIZE as _,
    };
    const MAXIMIZE_ICON: Normalized2DStaticMeshTexture = Normalized2DStaticMeshTexture {
        vertices: &[
            [0.0, 0.0],
            [0.0 + 1.5 / Self::ICON_SIZE, 0.0 + 1.5 / Self::ICON_SIZE],
            [1.0, 0.0],
            [1.0 - 1.5 / Self::ICON_SIZE, 0.0 + 1.5 / Self::ICON_SIZE],
            [1.0, 1.0],
            [1.0 - 1.5 / Self::ICON_SIZE, 1.0 - 1.5 / Self::ICON_SIZE],
            [0.0, 1.0],
            [0.0 + 1.5 / Self::ICON_SIZE, 1.0 - 1.5 / Self::ICON_SIZE],
        ],
        indices: &[
            0, 2, 3, 3, 1, 0, 2, 4, 5, 5, 3, 2, 4, 6, 7, 7, 5, 4, 6, 0, 1, 1, 7, 6,
        ],
        width: Self::ICON_SIZE as _,
        height: Self::ICON_SIZE as _,
    };
    const RESTORE_ICON: Normalized2DStaticMeshTexture = Normalized2DStaticMeshTexture {
        vertices: &[
            [0.0, 2.0 / Self::ICON_SIZE],
            [1.0 - 2.0 / Self::ICON_SIZE, 2.0 / Self::ICON_SIZE],
            [1.0 - 2.0 / Self::ICON_SIZE, 1.0],
            [0.0, 1.0],
            [1.0 / Self::ICON_SIZE, 3.0 / Self::ICON_SIZE],
            [1.0 - 3.0 / Self::ICON_SIZE, 3.0 / Self::ICON_SIZE],
            [1.0 - 3.0 / Self::ICON_SIZE, 1.0 - 1.0 / Self::ICON_SIZE],
            [1.0 / Self::ICON_SIZE, 1.0 - 1.0 / Self::ICON_SIZE],
            [3.0 / Self::ICON_SIZE, 0.0],
            [1.0, 0.0],
            [1.0, 1.0 - 3.0 / Self::ICON_SIZE],
            [3.0 / Self::ICON_SIZE, 1.0 / Self::ICON_SIZE],
            [1.0 - 1.0 / Self::ICON_SIZE, 1.0 / Self::ICON_SIZE],
            [1.0 - 1.0 / Self::ICON_SIZE, 1.0 - 3.0 / Self::ICON_SIZE],
        ],
        indices: &[
            0, 1, 4, 4, 1, 5, 1, 2, 5, 5, 2, 6, 2, 3, 6, 6, 3, 7, 3, 0, 7, 7, 0, 4, 8, 9, 11, 11,
            9, 12, 9, 10, 12, 12, 10, 13,
        ],
        width: Self::ICON_SIZE as _,
        height: Self::ICON_SIZE as _,
    };

    fn new(
        init_ctx: &mut ViewInitContext,
        texture_id_set: &SystemCommandTextureIDSet,
        right_offset: f32,
        init_cmd: SystemCommand,
    ) -> Self {
        let ct_root = init_ctx.mount_context.composite_tree.create(CompositeRect {
            base_scale_factor: init_ctx.ui_scale_factor,
            relative_offset_adjustment: [1.0, 0.0],
            offset: [
                AnimatableFloat::Value(-right_offset - Self::WIDTH),
                AnimatableFloat::Value(0.0),
            ],
            relative_size_adjustment: [0.0, 1.0],
            size: [
                AnimatableFloat::Value(Self::WIDTH),
                AnimatableFloat::Value(0.0),
            ],
            ..Default::default()
        });
        let ct_hover = init_ctx.composite_tree.create(CompositeRect {
            relative_size_adjustment: [1.0, 1.0],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value(match init_cmd {
                SystemCommand::Close => [1.0, 0.0, 0.0, 1.0],
                _ => [1.0, 1.0, 1.0, 0.5],
            })),
            opacity: AnimatableFloat::Value(0.0),
            ..Default::default()
        });
        let ct_icon = init_ctx.mount_context.composite_tree.create(CompositeRect {
            base_scale_factor: init_ctx.ui_scale_factor,
            offset: [
                AnimatableFloat::Value(-Self::ICON_SIZE * 0.5),
                AnimatableFloat::Value(-Self::ICON_SIZE * 0.5),
            ],
            relative_offset_adjustment: [0.5, 0.5],
            size: [
                AnimatableFloat::Value(Self::ICON_SIZE),
                AnimatableFloat::Value(Self::ICON_SIZE),
            ],
            has_bitmap: true,
            texatlas_rect_id: Some(texture_id_set.select(init_cmd)),
            composite_mode: CompositeMode::ColorTint(AnimatableColor::Value([0.9, 0.9, 0.9, 1.0])),
            ..Default::default()
        });

        init_ctx.composite_tree.add_child(ct_root, ct_hover);
        init_ctx.composite_tree.add_child(ct_root, ct_icon);

        let ht_root = init_ctx.ht_manager.create(HitTestTreeData {
            left: -right_offset - Self::WIDTH,
            left_adjustment_factor: 1.0,
            width: Self::WIDTH,
            height_adjustment_factor: 1.0,
            role: init_cmd.role(),
            ..Default::default()
        });

        let action_handler = Rc::new(SystemCommandButtonActionHandler {
            ht_root,
            cmd: core::cell::Cell::new(init_cmd),
            ct_hover,
            hovering: core::cell::Cell::new(false),
            pressing: core::cell::Cell::new(false),
            is_dirty: core::cell::Cell::new(false),
        });
        init_ctx
            .ht_manager
            .set_action_handler(ht_root, &action_handler);

        Self {
            ct_root,
            ct_icon,
            ct_hover,
            action_handler,
        }
    }

    fn mount(
        &self,
        ctx: &mut MountContext,
        ct_parent: CompositeTreeRef,
        ht_parent: HitTestTreeRef,
    ) {
        ctx.composite_tree.add_child(ct_parent, self.ct_root);
        ctx.ht_manager
            .add_child(ht_parent, self.action_handler.ht_root);
    }

    fn rescale(
        &self,
        composite_tree: &mut CompositeTree<SyncEvent>,
        texture_id_set: &SystemCommandTextureIDSet,
        ui_scale_factor: f32,
    ) {
        composite_tree.get_mut(self.ct_icon).texatlas_rect_id =
            Some(texture_id_set.select(self.action_handler.cmd.get()));
        composite_tree.get_mut(self.ct_icon).base_scale_factor = ui_scale_factor;
        composite_tree.get_mut(self.ct_root).base_scale_factor = ui_scale_factor;
        composite_tree.mark_dirty(self.ct_icon);
        composite_tree.mark_dirty(self.ct_root);
    }

    fn replace_cmd(
        &self,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
        texture_id_set: &SystemCommandTextureIDSet,
        cmd: SystemCommand,
    ) {
        if self.action_handler.cmd.replace(cmd) == cmd {
            // no changes
            return;
        }

        composite_tree.get_mut(self.ct_icon).texatlas_rect_id = Some(texture_id_set.select(cmd));
        composite_tree.mark_dirty(self.ct_icon);
        composite_tree.mark_dirty(self.ct_hover);
        ht_manager.get_data_mut(self.action_handler.ht_root).role = cmd.role();
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
}

pub struct SystemCommandTextureIDSet {
    close: TextureID,
    minimize: TextureID,
    maximize: TextureID,
    restore: TextureID,
}
impl SystemCommandTextureIDSet {
    pub fn new(
        tid_issuer: &mut MainThreadTextureIDIssuer,
        rt_sender: &RenderMessageSender,
    ) -> Self {
        let close = tid_issuer.issue();
        rt_sender
            .send(RenderMessage::RegisterNormalized2DStaticMeshTexture {
                id: close,
                data: SystemCommandButtonView::CLOSE_ICON,
            })
            .expect("rt_sender.send");
        let minimize = tid_issuer.issue();
        rt_sender
            .send(RenderMessage::RegisterNormalized2DStaticMeshTexture {
                id: minimize,
                data: SystemCommandButtonView::MINIMIZE_ICON,
            })
            .expect("rt_sender.send");
        let maximize = tid_issuer.issue();
        rt_sender
            .send(RenderMessage::RegisterNormalized2DStaticMeshTexture {
                id: maximize,
                data: SystemCommandButtonView::MAXIMIZE_ICON,
            })
            .expect("rt_sender.send");
        let restore = tid_issuer.issue();
        rt_sender
            .send(RenderMessage::RegisterNormalized2DStaticMeshTexture {
                id: restore,
                data: SystemCommandButtonView::RESTORE_ICON,
            })
            .expect("rt_sender.send");

        Self {
            close,
            minimize,
            maximize,
            restore,
        }
    }

    #[inline(always)]
    const fn select(&self, cmd: SystemCommand) -> TextureID {
        match cmd {
            SystemCommand::Close => self.close,
            SystemCommand::Minimize => self.minimize,
            SystemCommand::Maximize => self.maximize,
            SystemCommand::Restore => self.restore,
        }
    }
}
