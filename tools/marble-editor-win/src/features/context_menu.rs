use std::{
    borrow::Cow,
    cell::{Ref, RefCell, RefMut},
    sync::Arc,
};

use windows::{
    core::{h, s, Interface, PCSTR},
    Foundation::{
        Numerics::{Vector2, Vector3},
        Rect, TimeSpan, TypedEventHandler,
    },
    Graphics::IGeometrySource2D,
    System::DispatcherQueueTimer,
    Win32::{
        Foundation::{HWND, LPARAM, LRESULT, POINT, RECT, WPARAM},
        Graphics::{
            Direct2D::Common::{D2D1_FIGURE_BEGIN_FILLED, D2D1_FIGURE_END_CLOSED, D2D_POINT_2F},
            DirectWrite::{DWRITE_FONT_WEIGHT_BOLD, DWRITE_FONT_WEIGHT_NORMAL},
            Gdi::PtInRect,
        },
        System::{
            LibraryLoader::GetModuleHandleA, Threading::GetCurrentThreadId,
            WinRT::Composition::ICompositorDesktopInterop,
        },
        UI::{
            Controls::{HOVER_DEFAULT, WM_MOUSELEAVE},
            HiDpi::GetDpiForWindow,
            Input::KeyboardAndMouse::{TrackMouseEvent, TME_LEAVE, TRACKMOUSEEVENT},
            WindowsAndMessaging::{
                CallNextHookEx, DefWindowProcA, DestroyWindow, FindWindowExA, GetCursorPos,
                GetWindowLongPtrA, GetWindowRect, SetWindowLongPtrA, SetWindowPos,
                SetWindowsHookExA, ShowWindow, UnhookWindowsHookEx, HHOOK, MA_NOACTIVATE,
                SWP_HIDEWINDOW, SWP_NOACTIVATE, SWP_NOMOVE, SWP_NOSIZE, SWP_NOZORDER,
                SWP_SHOWWINDOW, SW_SHOWNA, WH_MOUSE, WINDOWPOS, WINDOW_LONG_PTR_INDEX, WM_DESTROY,
                WM_DPICHANGED, WM_LBUTTONDOWN, WM_LBUTTONUP, WM_MBUTTONDBLCLK, WM_MOUSEACTIVATE,
                WM_MOUSEMOVE, WM_WINDOWPOSCHANGED, WNDCLASSEXA,
            },
        },
    },
    UI::{
        Color, Colors,
        Composition::{
            AnimationDelayBehavior, CompositionAnimationGroup, CompositionEffectSourceParameter,
            CompositionMappingMode, CompositionPath, ContainerVisual, Desktop::DesktopWindowTarget,
            ScalarKeyFrameAnimation, ShapeVisual, SpriteVisual, Vector3KeyFrameAnimation,
            VisualCollection,
        },
    },
};

use crate::{
    app_subsystem_instances::AppSubsystemInstances,
    bindgen::Graphics::Canvas::Effects::{EffectOptimization, GaussianBlurEffect},
    new_cyclic_shared_mut, new_mt_shared_mut, new_shared_mut,
    uikit::{
        HitTestTree, HitTestTreeContext, InputContext, InputEventHandler, InputState,
        MountableView, ResizeContext, ViewContext,
    },
    utils::RectExtensions,
    winapi_extras::{
        register_window_class, timespan_ms, GeometryInterop, KeyFrameAnimationExtension,
        KeyFrameAnimationPropertySetterExtension, Vector3Extension, VectorScalarConstructor,
        VisualExtensions, WindowBuilder,
    },
    MTSharedMut, MTWeakMut, SharedMut, WeakMut,
};

trait ContextMenuEntryView: MountableView {
    fn set_menu_position(&mut self, x: f32, y: f32);
}

pub struct ContextMenuHeaderView {
    root: SpriteVisual,
    label: SpriteVisual,
    enter_opacity_animation: ScalarKeyFrameAnimation,
    enter_offset_animation: Vector3KeyFrameAnimation,
    height: f32,
    required_width: f32,
}
impl ContextMenuHeaderView {
    const PADDING_X: f32 = 8.0;
    const PADDING_Y: f32 = 4.0;
    const BACK_COLOR: Color = Color {
        A: 160,
        R: 160,
        G: 160,
        B: 160,
    };

    pub fn new(
        text: impl Into<Cow<'static, str>>,
        y: f32,
        enter_animation_delay: TimeSpan,
        view_ctx: &(impl ViewContext + ?Sized),
    ) -> windows::core::Result<Self> {
        let text_fmt = AppSubsystemInstances::get()
            .text_format_stock
            .borrow_mut()
            .get("system-ui", 10.0, DWRITE_FONT_WEIGHT_BOLD)?;
        let text = text.into().to_uppercase();
        let text_surface = AppSubsystemInstances::get()
            .text_surface_stock
            .borrow_mut()
            .get(&text_fmt, view_ctx.current_dpi(), text)?;

        let root = AppSubsystemInstances::get()
            .compositor
            .CreateSpriteVisual()?;
        root.set_properties()
            .brush(
                &AppSubsystemInstances::get()
                    .compositor
                    .CreateColorBrushWithColor(Self::BACK_COLOR)?,
            )?
            .offset(Vector3 {
                X: 0.0,
                Y: y,
                Z: 0.0,
            })?
            .relative_size_adjustment(Vector2 { X: 1.0, Y: 0.0 })?
            .size(Vector2 {
                X: 0.0,
                Y: text_surface.height + Self::PADDING_Y * 2.0,
            })?;

        let label = AppSubsystemInstances::get()
            .compositor
            .CreateSpriteVisual()?;
        label
            .set_properties()
            .brush(
                &AppSubsystemInstances::get()
                    .compositor
                    .CreateSurfaceBrushWithSurface(&text_surface.surface)?,
            )?
            .size(text_surface.visual_size())?;

        root.Children()?.InsertAtTop(&label)?;

        let enter_opacity_animation = AppSubsystemInstances::get()
            .compositor
            .CreateScalarKeyFrameAnimation()?;
        enter_opacity_animation
            .keyframe(0.0, 0.0)?
            .interpolate(
                1.0,
                1.0,
                &AppSubsystemInstances::get()
                    .ui_common_objects
                    .menu_item_enter_opacity_easing_fn,
            )?
            .set_properties()
            .duration(ContextMenuCommandView::ENTER_ANIMATION_DURARION)?
            .delay(
                enter_animation_delay,
                AnimationDelayBehavior::SetInitialValueBeforeDelay,
            )?;
        let enter_offset_animation = AppSubsystemInstances::get()
            .compositor
            .CreateVector3KeyFrameAnimation()?;
        enter_offset_animation
            .keyframe(
                0.0,
                Vector3 {
                    X: Self::PADDING_X + 8.0,
                    Y: Self::PADDING_Y,
                    Z: 0.0,
                },
            )?
            .interpolate(
                1.0,
                Vector3 {
                    X: Self::PADDING_X,
                    Y: Self::PADDING_Y,
                    Z: 0.0,
                },
                &AppSubsystemInstances::get()
                    .ui_common_objects
                    .menu_item_enter_offset_easing_fn,
            )?
            .set_properties()
            .duration(ContextMenuCommandView::ENTER_ANIMATION_DURARION)?
            .delay(
                enter_animation_delay,
                AnimationDelayBehavior::SetInitialValueBeforeDelay,
            )?;

        Ok(Self {
            root,
            label,
            enter_opacity_animation,
            enter_offset_animation,
            height: text_surface.height + Self::PADDING_Y * 2.0,
            required_width: text_surface.width + Self::PADDING_X * 2.0,
        })
    }

    pub fn height(&self) -> f32 {
        self.height
    }

    pub fn required_width(&self) -> f32 {
        self.required_width
    }
}
impl ContextMenuEntryView for ContextMenuHeaderView {
    fn set_menu_position(&mut self, _x: f32, _y: f32) {}
}
impl MountableView for ContextMenuHeaderView {
    fn mount(
        &self,
        onto: &VisualCollection,
        _onto_ht: &HitTestTree,
        _view_context: &dyn ViewContext,
    ) -> windows::core::Result<()> {
        onto.InsertAtTop(&self.root)?;
        self.root
            .StartAnimation(h!("Opacity"), &self.enter_opacity_animation)?;
        self.label
            .StartAnimation(h!("Offset"), &self.enter_offset_animation)?;

        Ok(())
    }

    fn unmount(&self, _view_context: &dyn ViewContext) -> windows::core::Result<()> {
        self.root.Parent()?.Children()?.Remove(&self.root)?;

        Ok(())
    }
}

pub struct ContextMenuSeparatorView {
    root: ContainerVisual,
}
impl ContextMenuSeparatorView {
    const PADDING_X: f32 = 4.0;
    const PADDING_Y: f32 = 1.0;

    pub fn new(y: f32, _view_ctx: &(impl ViewContext + ?Sized)) -> windows::core::Result<Self> {
        let root = AppSubsystemInstances::get()
            .compositor
            .CreateContainerVisual()?;
        root.set_properties()
            .relative_size_adjustment(Vector2 { X: 1.0, Y: 0.0 })?
            .size(Vector2 {
                X: 0.0,
                Y: Self::PADDING_Y * 2.0 + 1.0,
            })?
            .offset(Vector3 {
                X: 0.0,
                Y: y,
                Z: 0.0,
            })?;

        let line = AppSubsystemInstances::get()
            .compositor
            .CreateSpriteVisual()?;
        line.set_properties()
            .brush(
                &AppSubsystemInstances::get()
                    .compositor
                    .CreateColorBrushWithColor(Color {
                        A: 192,
                        R: 160,
                        G: 160,
                        B: 160,
                    })?,
            )?
            .size(Vector2 {
                X: -Self::PADDING_X * 2.0,
                Y: 1.0,
            })?
            .relative_size_adjustment(Vector2 { X: 1.0, Y: 0.0 })?
            .offset(Vector3 {
                X: Self::PADDING_X,
                Y: Self::PADDING_Y,
                Z: 0.0,
            })?;

        root.Children()?.InsertAtTop(&line)?;

        Ok(Self { root })
    }

    pub fn height(&self) -> f32 {
        Self::PADDING_Y * 2.0 + 1.0
    }
}
impl ContextMenuEntryView for ContextMenuSeparatorView {
    fn set_menu_position(&mut self, _x: f32, _y: f32) {}
}
impl MountableView for ContextMenuSeparatorView {
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

pub struct ContextMenuCommandViewInputEventDelegate {
    this_ref: WeakMut<ContextMenuCommandView>,
    menu_ref: MTWeakMut<ContextMenuInstance>,
}
pub struct ContextMenuCommandView {
    root: ContainerVisual,
    label: SpriteVisual,
    back: SpriteVisual,
    submenu_icon: Option<(ShapeVisual, CompositionAnimationGroup)>,
    enter_animation: CompositionAnimationGroup,
    hover_animation: ScalarKeyFrameAnimation,
    hover_end_animation: ScalarKeyFrameAnimation,
    ht: HitTestTree,
    active: bool,
    y: f32,
    height: f32,
    required_width: f32,
    current_dpi: f32,
    submenu_contents: Vec<MenuItem>,
    select_action: Option<fn()>,
}
impl ContextMenuCommandView {
    const ENTER_ANIMATION_DURARION: TimeSpan = timespan_ms(100);
    const HOVER_ANIMATION_DURATION: TimeSpan = timespan_ms(100);
    const PADDING_X: f32 = 12.0;
    const PADDING_Y: f32 = 4.0;
    const BACK_INSET: f32 = 1.0;
    const SUBMENU_ICON_SIZE: f32 = 10.0;
    const POP_SUBMENU_DELAY: TimeSpan = timespan_ms(200);

    pub fn new(
        text: impl Into<Cow<'static, str>>,
        submenu_contents: Vec<MenuItem>,
        select_action: Option<fn()>,
        active: bool,
        enter_animation_delay: TimeSpan,
        y: f32,
        view_ctx: &(impl ViewContext + ?Sized),
        menu_instance: &MTSharedMut<ContextMenuInstance>,
    ) -> windows::core::Result<SharedMut<Self>> {
        let text_fmt = AppSubsystemInstances::get()
            .text_format_stock
            .borrow_mut()
            .get("system-ui", 12.0, DWRITE_FONT_WEIGHT_NORMAL)?;
        let text = AppSubsystemInstances::get()
            .text_surface_stock
            .borrow_mut()
            .get(&text_fmt, view_ctx.current_dpi(), text)?;

        let root = AppSubsystemInstances::get()
            .compositor
            .CreateContainerVisual()?;
        root.set_properties()
            .relative_size_adjustment(Vector2 { X: 1.0, Y: 0.0 })?
            .size(Vector2 {
                X: 0.0,
                Y: text.height + Self::PADDING_Y * 2.0,
            })?
            .offset(Vector3 {
                X: 0.0,
                Y: y,
                Z: 0.0,
            })?;

        let label_brush = AppSubsystemInstances::get().compositor.CreateMaskBrush()?;
        label_brush.SetMask(
            &AppSubsystemInstances::get()
                .compositor
                .CreateSurfaceBrushWithSurface(&text.surface)?,
        )?;
        label_brush.SetSource(
            &AppSubsystemInstances::get()
                .compositor
                .CreateColorBrushWithColor(if active {
                    Colors::White()?
                } else {
                    Colors::Gray()?
                })?,
        )?;

        let label = AppSubsystemInstances::get()
            .compositor
            .CreateSpriteVisual()?;
        label
            .set_properties()
            .brush(&label_brush)?
            .size(text.visual_size())?;

        let submenu_icon = if !submenu_contents.is_empty() {
            let icon_geometry = unsafe {
                AppSubsystemInstances::get()
                    .d2d1_factory
                    .CreatePathGeometry()?
            };
            unsafe {
                let sink = icon_geometry.Open()?;
                sink.BeginFigure(
                    D2D_POINT_2F {
                        x: Self::SUBMENU_ICON_SIZE,
                        y: Self::SUBMENU_ICON_SIZE * 0.5,
                    },
                    D2D1_FIGURE_BEGIN_FILLED,
                );
                sink.AddLines(&[
                    D2D_POINT_2F {
                        x: Self::SUBMENU_ICON_SIZE * 0.5,
                        y: 0.0,
                    },
                    D2D_POINT_2F {
                        x: Self::SUBMENU_ICON_SIZE * 0.5,
                        y: Self::SUBMENU_ICON_SIZE,
                    },
                ]);
                sink.EndFigure(D2D1_FIGURE_END_CLOSED);
                sink.Close()?;
            }

            let icon_geometry: IGeometrySource2D = GeometryInterop(icon_geometry.into()).into();
            let icon_geometry = AppSubsystemInstances::get()
                .compositor
                .CreatePathGeometryWithPath(&CompositionPath::Create(&icon_geometry)?)?;
            let icon_shape = AppSubsystemInstances::get()
                .compositor
                .CreateSpriteShapeWithGeometry(&icon_geometry)?;
            icon_shape.SetFillBrush(
                &AppSubsystemInstances::get()
                    .compositor
                    .CreateColorBrushWithColor(Color {
                        A: 255,
                        R: 224,
                        G: 224,
                        B: 224,
                    })?,
            )?;
            let v = AppSubsystemInstances::get()
                .compositor
                .CreateShapeVisual()?;
            v.Shapes()?.Append(&icon_shape)?;
            v.set_properties()
                .size(Vector2::scalar(Self::SUBMENU_ICON_SIZE))?
                .offset(Vector3 {
                    X: -Self::PADDING_X,
                    Y: 0.0,
                    Z: 0.0,
                })?
                .anchor_point(Vector2 { X: 1.0, Y: 0.5 })?
                .relative_offset_adjustment(Vector3 {
                    X: 1.0,
                    Y: 0.5,
                    Z: 0.0,
                })?;

            Some(v)
        } else {
            None
        };

        let back = AppSubsystemInstances::get()
            .compositor
            .CreateSpriteVisual()?;
        let back_brush = AppSubsystemInstances::get().compositor.CreateMaskBrush()?;
        back_brush.SetMask(
            &AppSubsystemInstances::get()
                .ui_common_objects
                .menu_item_back_mask_brush,
        )?;
        back_brush.SetSource(&{
            let b = AppSubsystemInstances::get()
                .compositor
                .CreateRadialGradientBrush()?;
            b.ColorStops()?.Append(
                &AppSubsystemInstances::get()
                    .compositor
                    .CreateColorGradientStopWithOffsetAndColor(
                        0.0,
                        Color {
                            A: 255,
                            R: 128,
                            G: 255,
                            B: 255,
                        },
                    )?,
            )?;
            b.ColorStops()?.Append(
                &AppSubsystemInstances::get()
                    .compositor
                    .CreateColorGradientStopWithOffsetAndColor(
                        0.3,
                        Color {
                            A: 192,
                            R: 32,
                            G: 160,
                            B: 224,
                        },
                    )?,
            )?;
            b.ColorStops()?.Append(
                &AppSubsystemInstances::get()
                    .compositor
                    .CreateColorGradientStopWithOffsetAndColor(
                        1.0,
                        Color {
                            A: 0,
                            R: 32,
                            G: 144,
                            B: 224,
                        },
                    )?,
            )?;
            b.SetMappingMode(CompositionMappingMode::Relative)?;
            b.SetEllipseRadius(Vector2 { X: 0.5, Y: 0.25 })?;
            b.SetEllipseCenter(Vector2 { X: 0.5, Y: 0.75 })?;
            b
        })?;
        back.set_properties()
            .brush(&back_brush)?
            .expand_to_parent()?
            .offset(Vector3 {
                X: Self::BACK_INSET,
                Y: Self::BACK_INSET,
                Z: 0.0,
            })?
            .size(Vector2 {
                X: -Self::BACK_INSET * 2.0,
                Y: -Self::BACK_INSET * 2.0,
            })?
            .opacity(0.0)?;

        root.Children()?.InsertAtTop(&back)?;
        if let Some(v) = submenu_icon.as_ref() {
            root.Children()?.InsertAtTop(v)?;
        }
        root.Children()?.InsertAtTop(&label)?;

        let enter_opacity_animation = AppSubsystemInstances::get()
            .compositor
            .CreateScalarKeyFrameAnimation()?;
        enter_opacity_animation
            .keyframe(0.0, 0.0)?
            .interpolate(
                1.0,
                1.0,
                &AppSubsystemInstances::get()
                    .ui_common_objects
                    .menu_item_enter_opacity_easing_fn,
            )?
            .set_properties()
            .duration(Self::ENTER_ANIMATION_DURARION)?
            .target(h!("Opacity"))?;
        enter_opacity_animation.SetDelayTime(enter_animation_delay)?;
        enter_opacity_animation
            .SetDelayBehavior(AnimationDelayBehavior::SetInitialValueBeforeDelay)?;
        let enter_offset_animation = AppSubsystemInstances::get()
            .compositor
            .CreateVector3KeyFrameAnimation()?;
        enter_offset_animation.InsertKeyFrame(
            0.0,
            Vector3 {
                X: 8.0 + Self::PADDING_X,
                Y: Self::PADDING_Y,
                Z: 0.0,
            },
        )?;
        enter_offset_animation.InsertKeyFrameWithEasingFunction(
            1.0,
            Vector3 {
                X: Self::PADDING_X,
                Y: Self::PADDING_Y,
                Z: 0.0,
            },
            &AppSubsystemInstances::get()
                .ui_common_objects
                .menu_item_enter_offset_easing_fn,
        )?;
        enter_offset_animation.SetDelayTime(enter_animation_delay)?;
        enter_offset_animation
            .SetDelayBehavior(AnimationDelayBehavior::SetInitialValueBeforeDelay)?;
        enter_offset_animation.SetDuration(Self::ENTER_ANIMATION_DURARION)?;
        enter_offset_animation.SetTarget(h!("Offset"))?;
        let enter_animation = AppSubsystemInstances::get()
            .compositor
            .CreateAnimationGroup()?;
        enter_animation.Add(&enter_opacity_animation)?;
        enter_animation.Add(&enter_offset_animation)?;

        let submenu_icon_enter_animation = if !submenu_contents.is_empty() {
            let enter_offset_animation = AppSubsystemInstances::get()
                .compositor
                .CreateVector3KeyFrameAnimation()?;
            enter_offset_animation.InsertKeyFrame(
                0.0,
                Vector3 {
                    X: -Self::PADDING_X - 8.0,
                    Y: 0.0,
                    Z: 0.0,
                },
            )?;
            enter_offset_animation.InsertKeyFrameWithEasingFunction(
                1.0,
                Vector3 {
                    X: -Self::PADDING_X,
                    Y: 0.0,
                    Z: 0.0,
                },
                &AppSubsystemInstances::get()
                    .ui_common_objects
                    .menu_item_enter_offset_easing_fn,
            )?;
            enter_offset_animation.SetDelayTime(enter_animation_delay)?;
            enter_offset_animation
                .SetDelayBehavior(AnimationDelayBehavior::SetInitialValueBeforeDelay)?;
            enter_offset_animation.SetDuration(Self::ENTER_ANIMATION_DURARION)?;
            enter_offset_animation.SetTarget(h!("Offset"))?;
            let enter_animation = AppSubsystemInstances::get()
                .compositor
                .CreateAnimationGroup()?;
            enter_animation.Add(&enter_opacity_animation)?;
            enter_animation.Add(&enter_offset_animation)?;

            Some(enter_animation)
        } else {
            None
        };

        let linear_easing_fn = AppSubsystemInstances::get()
            .compositor
            .CreateLinearEasingFunction()?;

        let hover_animation = AppSubsystemInstances::get()
            .compositor
            .CreateScalarKeyFrameAnimation()?;
        hover_animation
            .keyframe(0.0, 0.0)?
            .interpolate(1.0, 1.0, &linear_easing_fn)?
            .set_properties()
            .duration(Self::HOVER_ANIMATION_DURATION)?;
        let hover_end_animation = AppSubsystemInstances::get()
            .compositor
            .CreateScalarKeyFrameAnimation()?;
        hover_end_animation
            .keyframe(0.0, 1.0)?
            .interpolate(1.0, 0.0, &linear_easing_fn)?
            .set_properties()
            .duration(Self::HOVER_ANIMATION_DURATION)?;

        Ok(new_cyclic_shared_mut(|wthis| {
            let ht = HitTestTree::new(
                Some(ContextMenuCommandViewInputEventDelegate {
                    this_ref: wthis.clone(),
                    menu_ref: Arc::downgrade(menu_instance),
                }),
                view_ctx.hittest_context().new_id(),
                Rect {
                    X: 0.0,
                    Y: y,
                    Width: 0.0,
                    Height: text.height + Self::PADDING_Y * 2.0,
                },
                Rect {
                    X: 0.0,
                    Y: 0.0,
                    Width: 1.0,
                    Height: 0.0,
                },
            );

            Self {
                root,
                label,
                back,
                submenu_icon: submenu_icon
                    .and_then(|v| submenu_icon_enter_animation.map(move |a| (v, a))),
                enter_animation,
                hover_animation,
                hover_end_animation,
                ht,
                active,
                y,
                height: text.height + Self::PADDING_Y * 2.0,
                required_width: text.width + Self::PADDING_X * 2.0,
                current_dpi: view_ctx.current_dpi(),
                submenu_contents,
                select_action,
            }
        }))
    }

    pub fn y(&self) -> f32 {
        self.y
    }

    pub fn height(&self) -> f32 {
        self.height
    }

    pub fn required_width(&self) -> f32 {
        self.required_width
    }

    pub fn lit(&self) -> windows::core::Result<()> {
        self.back
            .StartAnimation(h!("Opacity"), &self.hover_animation)
    }

    pub fn unlit(&self) -> windows::core::Result<()> {
        self.back
            .StartAnimation(h!("Opacity"), &self.hover_end_animation)
    }

    pub fn pop_submenu(
        contents: &[MenuItem],
        ref_dpi: f32,
        menu_pos: Vector2,
        menu_width: f32,
        element_y: f32,
    ) -> windows::core::Result<MTSharedMut<ContextMenuInstance>> {
        ContextMenu::get_mut().pop_new(
            contents,
            menu_pos.X + menu_width,
            menu_pos.Y + element_y,
            ref_dpi,
        )
    }
}
impl ContextMenuEntryView for ContextMenuCommandView {
    fn set_menu_position(&mut self, _x: f32, _y: f32) {}
}
impl MountableView for ContextMenuCommandView {
    fn mount(
        &self,
        onto: &VisualCollection,
        onto_ht: &HitTestTree,
        _view_context: &dyn ViewContext,
    ) -> windows::core::Result<()> {
        onto.InsertAtTop(&self.root)?;
        self.label.StartAnimationGroup(&self.enter_animation)?;
        if let Some((v, a)) = self.submenu_icon.as_ref() {
            v.StartAnimationGroup(a)?;
        }

        // 非アクティブ時はHitTestTreeをmountしないことでイベントを受け取らない
        if self.active {
            onto_ht.add_child(&self.ht);
        }

        Ok(())
    }

    fn unmount(&self, _view_context: &dyn ViewContext) -> windows::core::Result<()> {
        self.root.Parent()?.Children()?.Remove(&self.root)?;

        if self.active {
            self.ht.unmount();
        }

        Ok(())
    }
}
impl InputEventHandler for ContextMenuCommandViewInputEventDelegate {
    fn on_pointer_enter(&self, _ctx: &mut dyn InputContext) {
        let (Some(this), Some(m)) = (self.this_ref.upgrade(), self.menu_ref.upgrade()) else {
            return;
        };

        m.write().on_hover_element(&this);
    }

    fn on_pointer_leave(&self, _ctx: &mut dyn InputContext) {
        let (Some(this), Some(m)) = (self.this_ref.upgrade(), self.menu_ref.upgrade()) else {
            return;
        };

        m.write().on_leave_element(&this);
    }

    fn on_click(&self, _window: HWND, _ctx: &mut dyn InputContext) {
        let Some(this) = self.this_ref.upgrade() else {
            return;
        };

        ContextMenu::get_mut()
            .hide_all()
            .expect("Failed to close context menu");
        let thisref = this.borrow();
        if let Some(a) = thisref.select_action {
            a();
        }
    }
}

#[derive(Clone)]
pub enum MenuItem {
    Command(String, fn(), bool),
    SubMenu(String, Vec<MenuItem>),
    Separator,
    Header(String),
}

static CONTEXT_MENU_WINDOW_CLASS: std::sync::OnceLock<u16> = std::sync::OnceLock::new();

pub struct ContextMenuInputContext {
    input_state_ref: MTSharedMut<InputState>,
    ht_context: Arc<HitTestTreeContext>,
    current_dpi: f32,
}
impl ViewContext for ContextMenuInputContext {
    fn current_dpi(&self) -> f32 {
        self.current_dpi
    }

    fn hittest_context(&self) -> &HitTestTreeContext {
        &*self.ht_context
    }
}
impl InputContext for ContextMenuInputContext {
    fn capture_mouse(&mut self) {
        self.input_state_ref.write().capture_mouse();
    }

    fn make_resize_context(&self) -> ResizeContext {
        ResizeContext {
            current_dpi: self.current_dpi,
        }
    }

    fn release_mouse_capture(&mut self) {
        self.input_state_ref.write().release_mouse_capture();
    }
}

pub struct ContextMenuInstance {
    w: HWND,
    _composition_target: DesktopWindowTarget,
    unscaled_base: SpriteVisual,
    content_root: ContainerVisual,
    ht_root: HitTestTree,
    ht_context: Arc<HitTestTreeContext>,
    entries: Vec<SharedMut<dyn ContextMenuEntryView>>,
    current_dpi: f32,
    input_state: MTSharedMut<InputState>,
    content_size: Vector2,
    pos: Vector2,
    submenu_delay_timer: MTSharedMut<Option<DispatcherQueueTimer>>,
    submenu_instance_ref: MTSharedMut<Option<MTSharedMut<ContextMenuInstance>>>,
}
// TODO: これあとでなんとかしたい
unsafe impl Sync for ContextMenuInstance {}
unsafe impl Send for ContextMenuInstance {}
impl ContextMenuInstance {
    const SHADOW_SIZE: f32 = 32.0;
    const TINT_COLOR: Color = Color {
        A: 96,
        R: 0,
        G: 0,
        B: 0,
    };

    pub fn new(ref_dpi: f32) -> windows::core::Result<MTSharedMut<Self>> {
        let w = WindowBuilder::new(
            unsafe { GetModuleHandleA(None)?.into() },
            ContextMenu::window_class(),
            s!(""),
        )
        .no_activate()
        .no_redirection_bitmap()
        .topmost()
        .popup()
        .create()?;
        let composition_target = unsafe {
            AppSubsystemInstances::get()
                .compositor
                .cast::<ICompositorDesktopInterop>()?
                .CreateDesktopWindowTarget(w, true)?
        };

        let root = AppSubsystemInstances::get()
            .compositor
            .CreateContainerVisual()?;
        root.SetOffset(Vector3 {
            X: Self::SHADOW_SIZE,
            Y: Self::SHADOW_SIZE,
            Z: 0.0,
        })?;
        composition_target.SetRoot(&root)?;

        let fx = GaussianBlurEffect::new()?;
        fx.SetSource(&CompositionEffectSourceParameter::Create(h!("source"))?)?;
        fx.SetBlurAmount(16.0)?;
        fx.SetOptimization(EffectOptimization::Balanced)?;
        let effect_factory = AppSubsystemInstances::get()
            .compositor
            .CreateEffectFactory(&fx)?;
        let backdrop_brush = AppSubsystemInstances::get()
            .compositor
            .CreateBackdropBrush()?;
        let blur_brush = effect_factory.CreateBrush()?;
        blur_brush.SetSourceParameter(h!("source"), &backdrop_brush)?;

        let blur_visual = AppSubsystemInstances::get()
            .compositor
            .CreateSpriteVisual()?;
        blur_visual.set_properties().brush(&blur_brush)?;

        let color_tint = AppSubsystemInstances::get()
            .compositor
            .CreateSpriteVisual()?;
        color_tint
            .set_properties()
            .brush(
                &AppSubsystemInstances::get()
                    .compositor
                    .CreateColorBrushWithColor(Self::TINT_COLOR)?,
            )?
            .expand_to_parent()?;
        blur_visual.Children()?.InsertAtTop(&color_tint)?;

        blur_visual.SetShadow(&{
            let x = AppSubsystemInstances::get().compositor.CreateDropShadow()?;
            x.SetBlurRadius(Self::SHADOW_SIZE)?;
            x.SetOffset(Vector3::down(Self::SHADOW_SIZE * 0.5))?;
            x.SetOpacity(0.3)?;
            x
        })?;

        let content_root = AppSubsystemInstances::get()
            .compositor
            .CreateContainerVisual()?;

        let children = root.Children()?;
        children.InsertAtBottom(&blur_visual)?;
        children.InsertAtTop(&content_root)?;

        let ht_root = HitTestTree::new(None::<()>, 0, Rect::from_size(128.0, 160.0), Rect::empty());
        let this = new_mt_shared_mut(Self {
            w,
            _composition_target: composition_target,
            unscaled_base: blur_visual,
            content_root,
            input_state: new_mt_shared_mut(InputState::new(w, &ht_root)),
            ht_root,
            ht_context: Arc::new(HitTestTreeContext::new()),
            entries: Vec::new(),
            current_dpi: ref_dpi,
            content_size: Vector2 { X: 128.0, Y: 160.0 },
            pos: Vector2 { X: 0.0, Y: 0.0 },
            submenu_delay_timer: new_mt_shared_mut(None),
            submenu_instance_ref: new_mt_shared_mut(None),
        });
        ContextMenu::bind_window_instance(w, &this);

        Ok(this)
    }

    pub fn setup_contents(
        this: &MTSharedMut<Self>,
        content: &[MenuItem],
    ) -> windows::core::Result<()> {
        let thisref = this.read();
        for e in thisref.entries.iter() {
            e.borrow().unmount(&*thisref)?;
        }
        drop(thisref);
        this.write().entries.clear();

        // Viewのpoolingとかはあとで
        let mut yo = 0.0f32;
        let mut xr = 128.0f32;
        let mut delay = timespan_ms(0);
        for c in content {
            match c {
                MenuItem::Command(title, select_action, active) => {
                    let e = ContextMenuCommandView::new(
                        title.to_owned(),
                        Vec::new(),
                        Some(select_action.clone()),
                        *active,
                        delay,
                        yo,
                        &*this.read(),
                        this,
                    )?;
                    yo += e.borrow().height();
                    xr = xr.max(e.borrow().required_width());
                    delay.Duration += timespan_ms(5).Duration;
                    this.write().entries.push(e);
                }
                MenuItem::SubMenu(title, contents) => {
                    let e = ContextMenuCommandView::new(
                        title.to_owned(),
                        contents.clone(),
                        None,
                        true,
                        delay,
                        yo,
                        &*this.read(),
                        this,
                    )?;
                    yo += e.borrow().height();
                    xr = xr.max(e.borrow().required_width());
                    delay.Duration += timespan_ms(5).Duration;
                    this.write().entries.push(e);
                }
                MenuItem::Separator => {
                    let e = new_shared_mut(ContextMenuSeparatorView::new(yo, &*this.read())?);
                    yo += e.borrow().height();
                    this.write().entries.push(e);
                }
                MenuItem::Header(h) => {
                    let e = new_shared_mut(ContextMenuHeaderView::new(
                        h.to_owned(),
                        yo,
                        delay,
                        &*this.read(),
                    )?);
                    yo += e.borrow().height();
                    xr = xr.max(e.borrow().required_width());
                    delay.Duration += timespan_ms(5).Duration;
                    this.write().entries.push(e);
                }
            }
        }

        let thisref = this.read();
        let children = thisref.content_root.Children()?;
        for e in thisref.entries.iter() {
            e.borrow().mount(&children, &thisref.ht_root, &*thisref)?;
        }
        drop(thisref);

        this.write().content_size = Vector2 { X: xr, Y: yo };

        Ok(())
    }

    pub fn pop_at(&mut self, x: f32, y: f32) -> windows::core::Result<()> {
        // 影の分を考慮してウィンドウ位置を計算する
        unsafe {
            SetWindowPos(
                self.w,
                None,
                (x - Self::SHADOW_SIZE) as _,
                (y - Self::SHADOW_SIZE) as _,
                0,
                0,
                SWP_NOZORDER | SWP_NOACTIVATE | SWP_NOSIZE,
            )?;
        }
        unsafe {
            let _ = ShowWindow(self.w, SW_SHOWNA);
        }
        self.content_root.SetScale(Vector3::scalar(
            unsafe { GetDpiForWindow(self.w) as f32 } / 96.0,
        ))?;
        self.set_size(self.content_size.X, self.content_size.Y, unsafe {
            GetDpiForWindow(self.w) as f32
        })?;
        self.pos = Vector2 { X: x, Y: y };

        Ok(())
    }

    pub fn destroy(&mut self) -> windows::core::Result<()> {
        for e in self.entries.iter() {
            e.borrow().unmount(self)?;
        }
        self.entries.clear();

        if let Some(sub) = self.submenu_instance_ref.write().take() {
            sub.write().destroy()?;
        }

        unsafe {
            DestroyWindow(self.w)?;
        }

        Ok(())
    }

    pub fn set_size(&self, width: f32, height: f32, for_dpi: f32) -> windows::core::Result<()> {
        self.unscaled_base.SetSize(Vector2 {
            X: width * for_dpi / 96.0,
            Y: height * for_dpi / 96.0,
        })?;
        self.content_root.SetSize(Vector2 {
            X: width,
            Y: height,
        })?;
        self.ht_root.set_size(width, height);

        // 影の分を考慮してウィンドウサイズを計算する
        unsafe {
            SetWindowPos(
                self.w,
                None,
                0,
                0,
                (width * for_dpi / 96.0 + Self::SHADOW_SIZE * 2.0) as _,
                (height * for_dpi / 96.0 + Self::SHADOW_SIZE * 2.0) as _,
                SWP_NOZORDER | SWP_NOACTIVATE | SWP_NOMOVE,
            )?;
        }

        Ok(())
    }

    pub fn on_hover_element(&self, e: &SharedMut<ContextMenuCommandView>) {
        e.borrow().lit().expect("Failed to lit element");

        let dq = e.borrow().root.DispatcherQueue().unwrap();
        let tmr = dq.CreateTimer().expect("Failed to create dispatcher timer");
        tmr.SetInterval(ContextMenuCommandView::POP_SUBMENU_DELAY)
            .expect("Failed to set timeout");
        let ref_dpi = e.borrow().current_dpi;
        let menu_pos = self.pos.clone();
        let menu_size = self.content_size.clone();
        let element_y = e.borrow().y();
        let contents_ref = e.borrow().submenu_contents.clone();
        let tmr_ref = self.submenu_delay_timer.clone();
        let subinst_ref = self.submenu_instance_ref.clone();
        tmr.Tick(&TypedEventHandler::new(move |_, _| {
            *subinst_ref.write() = Some(ContextMenuCommandView::pop_submenu(
                &contents_ref,
                ref_dpi,
                menu_pos,
                (menu_size.X - 8.0) * ref_dpi / 96.0,
                element_y * ref_dpi / 96.0,
            )?);
            *tmr_ref.write() = None;

            Ok(())
        }))
        .expect("Failed to set tick");
        tmr.Start().expect("Failed to start timer");
        *self.submenu_delay_timer.write() = Some(tmr);
    }

    pub fn on_leave_element(&self, e: &SharedMut<ContextMenuCommandView>) {
        e.borrow().unlit().expect("Failed to unlit element");

        if let Some(sub) = self.submenu_instance_ref.write().take() {
            sub.write()
                .destroy()
                .expect("Failed to destroy spawned submenu");
        }
        *self.submenu_delay_timer.write() = None;
    }
}
impl ViewContext for ContextMenuInstance {
    fn current_dpi(&self) -> f32 {
        self.current_dpi
    }

    fn hittest_context(&self) -> &HitTestTreeContext {
        &*self.ht_context
    }
}

struct ContextMenuSharedState {
    mouse_hook_handle: Option<HHOOK>,
}

static mut CONTEXT_MENU_MANAGER: *mut RefCell<ContextMenu> = core::ptr::null_mut();

pub struct ContextMenuManagerFinalizer;
impl Drop for ContextMenuManagerFinalizer {
    fn drop(&mut self) {
        ContextMenu::finalize();
    }
}

pub struct ContextMenu {
    shared_state_root_ref: SharedMut<ContextMenuSharedState>,
    instances: Vec<MTWeakMut<ContextMenuInstance>>,
}
impl ContextMenu {
    pub fn initialize() -> ContextMenuManagerFinalizer {
        unsafe {
            CONTEXT_MENU_MANAGER = Box::into_raw(Box::new(RefCell::new(Self::new())));
        }

        ContextMenuManagerFinalizer
    }

    pub fn finalize() {
        unsafe {
            if CONTEXT_MENU_MANAGER.is_null() {
                return;
            }

            drop(Box::from_raw(core::mem::replace(
                &mut *core::ptr::addr_of_mut!(CONTEXT_MENU_MANAGER),
                core::ptr::null_mut(),
            )));
        }
    }

    #[inline(always)]
    pub fn get<'a>() -> Ref<'a, Self> {
        unsafe { (&*CONTEXT_MENU_MANAGER).borrow() }
    }

    #[inline(always)]
    pub fn get_mut<'a>() -> RefMut<'a, Self> {
        unsafe { (&*CONTEXT_MENU_MANAGER).borrow_mut() }
    }

    fn window_class() -> u16 {
        *CONTEXT_MENU_WINDOW_CLASS.get_or_init(|| {
            register_window_class(&WNDCLASSEXA {
                cbSize: core::mem::size_of::<WNDCLASSEXA>() as _,
                lpfnWndProc: Some(Self::wndproc),
                hInstance: unsafe {
                    GetModuleHandleA(None)
                        .expect("Failed to get instance handle")
                        .into()
                },
                lpszClassName: s!("io.ct2.peridot.marble.windows.overlays.context_menu"),
                cbWndExtra: core::mem::size_of::<[*const (); 2]>() as _,
                ..unsafe { core::mem::MaybeUninit::zeroed().assume_init() }
            })
            .expect("Failed to register context menu window class")
        })
    }

    const WINDOW_PTR_INSTANCE: WINDOW_LONG_PTR_INDEX = WINDOW_LONG_PTR_INDEX(0);
    const WINDOW_PTR_SHARED_STATE: WINDOW_LONG_PTR_INDEX =
        WINDOW_LONG_PTR_INDEX(core::mem::size_of::<*const ()>() as _);

    #[inline(always)]
    fn set_shared_state_ref(&self, window: HWND) {
        unsafe {
            SetWindowLongPtrA(
                window,
                Self::WINDOW_PTR_SHARED_STATE,
                Box::into_raw(Box::new(self.shared_state_root_ref.clone())) as _,
            );
        }
    }

    #[inline(always)]
    fn get_shared_state_ref<'a>(window: HWND) -> &'a SharedMut<ContextMenuSharedState> {
        unsafe {
            &*(GetWindowLongPtrA(window, Self::WINDOW_PTR_SHARED_STATE)
                as *const SharedMut<ContextMenuSharedState>)
        }
    }

    #[inline(always)]
    fn bind_window_instance(window: HWND, instance_ref: &MTSharedMut<ContextMenuInstance>) {
        unsafe {
            SetWindowLongPtrA(
                window,
                Self::WINDOW_PTR_INSTANCE,
                Box::into_raw(Box::new(instance_ref.clone())) as _,
            );
        }
    }

    #[inline(always)]
    fn get_window_instance<'a>(window: HWND) -> &'a MTSharedMut<ContextMenuInstance> {
        unsafe {
            &*(GetWindowLongPtrA(window, Self::WINDOW_PTR_INSTANCE)
                as *const MTSharedMut<ContextMenuInstance>)
        }
    }

    fn new() -> Self {
        Self {
            shared_state_root_ref: new_shared_mut(ContextMenuSharedState {
                mouse_hook_handle: None,
            }),
            instances: Vec::new(),
        }
    }

    pub fn pop_new(
        &mut self,
        content: &[MenuItem],
        x: f32,
        y: f32,
        ref_dpi: f32,
    ) -> windows::core::Result<MTSharedMut<ContextMenuInstance>> {
        let new_instance = ContextMenuInstance::new(ref_dpi)?;
        self.set_shared_state_ref(new_instance.read().w);
        ContextMenuInstance::setup_contents(&new_instance, content)?;
        new_instance.write().pop_at(x, y)?;

        // 死んだWeakがたまっていくのでこのタイミングで綺麗にする
        self.instances.retain(|x| x.strong_count() > 0);
        self.instances.push(Arc::downgrade(&new_instance));
        Ok(new_instance)
    }

    pub fn hide_all(&mut self) -> windows::core::Result<()> {
        for x in self.instances.drain(..) {
            if let Some(x) = x.upgrade() {
                x.write().destroy()?;
            }
        }

        Ok(())
    }

    extern "system" fn wndproc(w: HWND, m: u32, wp: WPARAM, lp: LPARAM) -> LRESULT {
        if m == WM_DESTROY {
            drop(unsafe {
                Box::from_raw(GetWindowLongPtrA(w, Self::WINDOW_PTR_INSTANCE)
                    as *mut SharedMut<ContextMenuInstance>)
            });
            drop(unsafe {
                Box::from_raw(GetWindowLongPtrA(w, Self::WINDOW_PTR_SHARED_STATE)
                    as *mut SharedMut<ContextMenuSharedState>)
            });
        }
        if m == WM_DPICHANGED {
            Self::get_window_instance(w).write().current_dpi = (wp.0 & 0xffff) as f32;
        }
        if m == WM_MOUSEACTIVATE {
            return LRESULT(MA_NOACTIVATE as _);
        }
        if m == WM_WINDOWPOSCHANGED {
            let shared_state = Self::get_shared_state_ref(w);

            // hiding by mouse hook: https://www.codeproject.com/Tips/751520/Custom-Context-Menu
            let windowpos = unsafe { &*(lp.0 as usize as *const WINDOWPOS) };

            if windowpos.flags.contains(SWP_SHOWWINDOW) {
                shared_state.borrow_mut().mouse_hook_handle = Some(unsafe {
                    SetWindowsHookExA(WH_MOUSE, Some(Self::mouse_hook), None, GetCurrentThreadId())
                        .expect("Failed to register mouse hook")
                });
            } else if windowpos.flags.contains(SWP_HIDEWINDOW) {
                if let Some(hook_handle) = shared_state.borrow_mut().mouse_hook_handle.take() {
                    unsafe { UnhookWindowsHookEx(hook_handle).expect("Failed to unhook mouse") };
                }
            }
        }
        if m == WM_MOUSEMOVE {
            let state = Self::get_window_instance(w);
            let dpi = unsafe { GetDpiForWindow(w) as f32 };

            let (x, y) = ((lp.0 & 0xffff) as i16, ((lp.0 >> 16) & 0xffff) as i16);
            // ドロップシャドウの分あけているのでそのぶんずらす
            let (x, y) = (x as f32 - 32.0, y as f32 - 32.0);
            let actions = state
                .write()
                .input_state
                .write()
                .on_mouse_move((x as f32) * 96.0 / dpi, (y as f32) * 96.0 / dpi);
            let mut input_context = ContextMenuInputContext {
                current_dpi: state.read().current_dpi,
                ht_context: state.read().ht_context.clone(),
                input_state_ref: state.read().input_state.clone(),
            };
            for a in actions {
                a.execute(x as _, y as _, &mut input_context, w);
            }

            let mut tme = TRACKMOUSEEVENT {
                cbSize: core::mem::size_of::<TRACKMOUSEEVENT>() as _,
                dwFlags: TME_LEAVE,
                hwndTrack: w,
                dwHoverTime: HOVER_DEFAULT,
            };
            unsafe {
                TrackMouseEvent(&mut tme).expect("Failed to track mouse event");
            }

            return LRESULT(0);
        }
        if m == WM_LBUTTONDOWN {
            let state = Self::get_window_instance(w);
            let dpi = unsafe { GetDpiForWindow(w) as f32 };

            // ドロップシャドウの分あけているのでそのぶんずらす
            let (x, y) = (
                (lp.0 & 0xffff) as i16 - 32,
                ((lp.0 >> 16) & 0xffff) as i16 - 32,
            );
            let actions = state
                .write()
                .input_state
                .write()
                .on_mouse_down((x as f32) * 96.0 / dpi, (y as f32) * 96.0 / dpi);
            let mut input_context = ContextMenuInputContext {
                current_dpi: state.read().current_dpi,
                ht_context: state.read().ht_context.clone(),
                input_state_ref: state.read().input_state.clone(),
            };
            for a in actions {
                a.execute(x as _, y as _, &mut input_context, w);
            }

            return LRESULT(0);
        }
        if m == WM_LBUTTONUP {
            let state = Self::get_window_instance(w);
            let dpi = unsafe { GetDpiForWindow(w) as f32 };

            // ドロップシャドウの分あけているのでそのぶんずらす
            let (x, y) = (
                (lp.0 & 0xffff) as i16 - 32,
                ((lp.0 >> 16) & 0xffff) as i16 - 32,
            );
            let actions = state
                .write()
                .input_state
                .write()
                .on_mouse_up((x as f32) * 96.0 / dpi, (y as f32) * 96.0 / dpi);
            let mut input_context = ContextMenuInputContext {
                current_dpi: state.read().current_dpi,
                ht_context: state.read().ht_context.clone(),
                input_state_ref: state.read().input_state.clone(),
            };
            for a in actions {
                a.execute(x as _, y as _, &mut input_context, w);
            }

            return LRESULT(0);
        }
        if m == WM_MOUSELEAVE {
            let state = Self::get_window_instance(w);

            if state.read().submenu_instance_ref.read().is_some() {
                // サブメニューが開いている場合はleaveイベントを処理しない
                return LRESULT(0);
            }

            let actions = state.write().input_state.write().on_mouse_leave();
            let mut input_context = ContextMenuInputContext {
                current_dpi: state.read().current_dpi,
                ht_context: state.read().ht_context.clone(),
                input_state_ref: state.read().input_state.clone(),
            };
            for a in actions {
                a.execute(0.0, 0.0, &mut input_context, w);
            }

            return LRESULT(0);
        }

        unsafe { DefWindowProcA(w, m, wp, lp) }
    }

    // hiding by mouse hook: https://www.codeproject.com/Tips/751520/Custom-Context-Menu
    extern "system" fn mouse_hook(code: i32, wp: WPARAM, lp: LPARAM) -> LRESULT {
        if WM_LBUTTONDOWN as usize <= wp.0 && wp.0 <= WM_MBUTTONDBLCLK as usize {
            let mut p = core::mem::MaybeUninit::<POINT>::uninit();
            unsafe {
                GetCursorPos(p.as_mut_ptr()).expect("Failed to get cursor pos");
            }
            let p = unsafe { p.assume_init() };

            let has_any_pointing =
                WindowByClassIter::new(PCSTR(Self::window_class() as _)).any(|w| {
                    let mut rc = core::mem::MaybeUninit::<RECT>::uninit();
                    unsafe {
                        GetWindowRect(w, rc.as_mut_ptr()).expect("Failed to get window rect");
                    }

                    unsafe { PtInRect(rc.assume_init_ref(), p).as_bool() }
                });

            if !has_any_pointing {
                Self::get_mut()
                    .hide_all()
                    .expect("Failed to hide all context menus");
            }
        }

        unsafe { CallNextHookEx(None, code, wp, lp) }
    }
}

struct WindowByClassIter {
    class: PCSTR,
    window_after: HWND,
}
impl WindowByClassIter {
    fn new(class: PCSTR) -> Self {
        Self {
            class,
            window_after: HWND(0),
        }
    }
}
impl Iterator for WindowByClassIter {
    type Item = HWND;

    fn next(&mut self) -> Option<Self::Item> {
        let w = unsafe { FindWindowExA(None, self.window_after, self.class, None) };

        if w.0 == 0 {
            None
        } else {
            self.window_after = w;
            Some(w)
        }
    }
}
