use std::borrow::Cow;

use windows::{
    core::{h, s, Interface, PCSTR},
    Foundation::{
        Numerics::{Vector2, Vector3},
        Rect, TimeSpan,
    },
    Graphics::IGeometrySource2D,
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
                CallNextHookEx, DefWindowProcA, FindWindowA, GetCursorPos, GetWindowLongPtrA,
                GetWindowRect, SetWindowLongPtrA, SetWindowPos, SetWindowsHookExA, ShowWindow,
                UnhookWindowsHookEx, GWLP_USERDATA, HHOOK, MA_NOACTIVATE, SWP_HIDEWINDOW,
                SWP_NOACTIVATE, SWP_NOMOVE, SWP_NOSIZE, SWP_NOZORDER, SWP_SHOWWINDOW, SW_HIDE,
                SW_SHOWNA, WH_MOUSE, WINDOWPOS, WM_DPICHANGED, WM_LBUTTONDOWN, WM_LBUTTONUP,
                WM_MBUTTONDBLCLK, WM_MOUSEACTIVATE, WM_MOUSEMOVE, WM_WINDOWPOSCHANGED, WNDCLASSEXA,
            },
        },
    },
    UI::{
        Color,
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
    new_cyclic_shared_mut, new_shared_mut,
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
    SharedMut, WeakMut,
};

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
            .duration(ContextMenuEntryView::ENTER_ANIMATION_DURARION)?
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
            .duration(ContextMenuEntryView::ENTER_ANIMATION_DURARION)?
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
impl MountableView for ContextMenuHeaderView {
    fn mount(
        &self,
        onto: &VisualCollection,
        _onto_ht: &SharedMut<HitTestTree>,
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
impl MountableView for ContextMenuSeparatorView {
    fn mount(
        &self,
        onto: &VisualCollection,
        _onto_ht: &SharedMut<HitTestTree>,
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

pub struct ContextMenuEntryView {
    root: ContainerVisual,
    label: SpriteVisual,
    back: SpriteVisual,
    submenu_icon: Option<(ShapeVisual, CompositionAnimationGroup)>,
    enter_animation: CompositionAnimationGroup,
    hover_animation: ScalarKeyFrameAnimation,
    hover_end_animation: ScalarKeyFrameAnimation,
    ht: SharedMut<HitTestTree>,
    height: f32,
    required_width: f32,
}
impl ContextMenuEntryView {
    const ENTER_ANIMATION_DURARION: TimeSpan = timespan_ms(100);
    const HOVER_ANIMATION_DURATION: TimeSpan = timespan_ms(100);
    const PADDING_X: f32 = 12.0;
    const PADDING_Y: f32 = 4.0;
    const BACK_INSET: f32 = 1.0;
    const SUBMENU_ICON_SIZE: f32 = 10.0;

    pub fn new(
        text: impl Into<Cow<'static, str>>,
        has_submenu: bool,
        enter_animation_delay: TimeSpan,
        y: f32,
        view_ctx: &(impl ViewContext + ?Sized),
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

        let label = AppSubsystemInstances::get()
            .compositor
            .CreateSpriteVisual()?;
        label
            .set_properties()
            .brush(
                &AppSubsystemInstances::get()
                    .compositor
                    .CreateSurfaceBrushWithSurface(&text.surface)?,
            )?
            .size(text.visual_size())?;

        let submenu_icon = if has_submenu {
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

        let submenu_icon_enter_animation = if has_submenu {
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
                Some(wthis.clone()),
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
                height: text.height + Self::PADDING_Y * 2.0,
                required_width: text.width + Self::PADDING_X * 2.0,
            }
        }))
    }

    pub fn height(&self) -> f32 {
        self.height
    }

    pub fn required_width(&self) -> f32 {
        self.required_width
    }
}
impl MountableView for ContextMenuEntryView {
    fn mount(
        &self,
        onto: &VisualCollection,
        onto_ht: &SharedMut<HitTestTree>,
        _view_context: &dyn ViewContext,
    ) -> windows::core::Result<()> {
        onto.InsertAtTop(&self.root)?;
        self.label.StartAnimationGroup(&self.enter_animation)?;
        if let Some((v, a)) = self.submenu_icon.as_ref() {
            v.StartAnimationGroup(a)?;
        }

        HitTestTree::add_child(onto_ht, self.ht.clone());

        Ok(())
    }

    fn unmount(&self, _view_context: &dyn ViewContext) -> windows::core::Result<()> {
        self.root.Parent()?.Children()?.Remove(&self.root)?;
        self.ht.borrow_mut().unmount();

        Ok(())
    }
}
impl InputEventHandler for WeakMut<ContextMenuEntryView> {
    fn on_pointer_enter(&self, _ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };

        this.borrow()
            .back
            .StartAnimation(h!("Opacity"), &this.borrow().hover_animation)
            .expect("Failed to start hover animation");
    }

    fn on_pointer_leave(&self, _ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };

        this.borrow()
            .back
            .StartAnimation(h!("Opacity"), &this.borrow().hover_end_animation)
            .expect("Failed to start hover end animation");
    }
}

pub enum MenuItem {
    Command(String),
    SubMenu(String),
    Separator,
    Header(String),
}

static CONTEXT_MENU_WINDOW_CLASS: std::sync::OnceLock<u16> = std::sync::OnceLock::new();

pub struct ContextMenuWindowState {
    input_state: InputState,
    ht_context: HitTestTreeContext,
    mouse_hook_handle: Option<HHOOK>,
    current_dpi: f32,
}
impl ViewContext for ContextMenuWindowState {
    fn current_dpi(&self) -> f32 {
        self.current_dpi
    }

    fn hittest_context(&self) -> &HitTestTreeContext {
        &self.ht_context
    }
}
impl InputContext for ContextMenuWindowState {
    fn capture_mouse(&mut self) {
        self.input_state.capture_mouse();
    }

    fn make_resize_context(&self) -> ResizeContext {
        ResizeContext {
            current_dpi: self.current_dpi,
        }
    }

    fn release_mouse_capture(&mut self) {
        self.input_state.release_mouse_capture()
    }
}

pub struct ContextMenu {
    w: HWND,
    _composition_target: DesktopWindowTarget,
    root: ContainerVisual,
    unscaled_base: SpriteVisual,
    content_root: ContainerVisual,
    ht_root: SharedMut<HitTestTree>,
    entries: Vec<SharedMut<dyn MountableView>>,
    window_state: Box<SharedMut<ContextMenuWindowState>>,
    content_size: Vector2,
}
impl ContextMenu {
    const TINT_COLOR: Color = Color {
        A: 96,
        R: 0,
        G: 0,
        B: 0,
    };

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
                cbWndExtra: core::mem::size_of::<*const ContextMenuWindowState>() as _,
                ..unsafe { core::mem::MaybeUninit::zeroed().assume_init() }
            })
            .expect("Failed to register context menu window class")
        })
    }

    pub fn new(app_window_dpi: f32) -> windows::core::Result<Self> {
        let w = WindowBuilder::new(
            unsafe { GetModuleHandleA(None)?.into() },
            Self::window_class(),
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
            X: 32.0,
            Y: 32.0,
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
            x.SetBlurRadius(32.0)?;
            x.SetOffset(Vector3::down(16.0))?;
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
        let window_state = Box::new(new_shared_mut(ContextMenuWindowState {
            input_state: InputState::new(w, &ht_root),
            mouse_hook_handle: None,
            ht_context: HitTestTreeContext::new(),
            current_dpi: app_window_dpi,
        }));
        unsafe {
            SetWindowLongPtrA(
                w,
                GWLP_USERDATA,
                (&*window_state) as *const SharedMut<ContextMenuWindowState> as _,
            );
        }

        Ok(Self {
            w,
            _composition_target: composition_target,
            root,
            unscaled_base: blur_visual,
            content_root,
            ht_root,
            entries: Vec::new(),
            window_state,
            content_size: Vector2 { X: 128.0, Y: 128.0 },
        })
    }

    pub fn setup_contents(&mut self, content: &[MenuItem]) -> windows::core::Result<()> {
        for e in self.entries.drain(..) {
            e.borrow().unmount(&*self.window_state.borrow())?;
        }

        // Viewのpoolingとかはあとで
        let mut yo = 0.0f32;
        let mut xr = 128.0f32;
        let mut delay = timespan_ms(0);
        for c in content {
            match c {
                MenuItem::Command(title) => {
                    let e = ContextMenuEntryView::new(
                        title.to_owned(),
                        false,
                        delay,
                        yo,
                        &*self.window_state.borrow(),
                    )?;
                    yo += e.borrow().height();
                    xr = xr.max(e.borrow().required_width());
                    delay.Duration += timespan_ms(5).Duration;
                    self.entries.push(e);
                }
                MenuItem::SubMenu(title) => {
                    let e = ContextMenuEntryView::new(
                        title.to_owned(),
                        true,
                        delay,
                        yo,
                        &*self.window_state.borrow(),
                    )?;
                    yo += e.borrow().height();
                    xr = xr.max(e.borrow().required_width());
                    delay.Duration += timespan_ms(5).Duration;
                    self.entries.push(e);
                }
                MenuItem::Separator => {
                    let e = new_shared_mut(ContextMenuSeparatorView::new(
                        yo,
                        &*self.window_state.borrow(),
                    )?);
                    yo += e.borrow().height();
                    self.entries.push(e);
                }
                MenuItem::Header(h) => {
                    let e = new_shared_mut(ContextMenuHeaderView::new(
                        h.to_owned(),
                        yo,
                        delay,
                        &*self.window_state.borrow(),
                    )?);
                    yo += e.borrow().height();
                    xr = xr.max(e.borrow().required_width());
                    delay.Duration += timespan_ms(5).Duration;
                    self.entries.push(e);
                }
            }
        }

        let children = self.content_root.Children()?;
        for e in self.entries.iter() {
            e.borrow()
                .mount(&children, &self.ht_root, &*self.window_state.borrow())?;
        }

        self.content_size = Vector2 { X: xr, Y: yo };

        Ok(())
    }

    pub fn pop_at(&self, x: f32, y: f32) -> windows::core::Result<()> {
        // 影の分を考慮してウィンドウ位置を計算する
        unsafe {
            SetWindowPos(
                self.w,
                None,
                x as i32 - 32,
                y as i32 - 32,
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

        Ok(())
    }

    pub fn hide(&mut self) -> windows::core::Result<()> {
        unsafe {
            let _ = ShowWindow(self.w, SW_HIDE);
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
        self.ht_root.borrow_mut().set_size(width, height);

        // 影の分を考慮してウィンドウサイズを計算する
        unsafe {
            SetWindowPos(
                self.w,
                None,
                0,
                0,
                (width * for_dpi / 96.0) as i32 + 64,
                (height * for_dpi / 96.0) as i32 + 64,
                SWP_NOZORDER | SWP_NOACTIVATE | SWP_NOMOVE,
            )?;
        }

        Ok(())
    }

    extern "system" fn wndproc(w: HWND, m: u32, wp: WPARAM, lp: LPARAM) -> LRESULT {
        if m == WM_DPICHANGED {
            let state = unsafe {
                &mut *(GetWindowLongPtrA(w, GWLP_USERDATA) as *mut ContextMenuWindowState)
            };
            state.current_dpi = (wp.0 & 0xffff) as f32;
        }
        if m == WM_MOUSEACTIVATE {
            return LRESULT(MA_NOACTIVATE as _);
        }
        if m == WM_WINDOWPOSCHANGED {
            let state = unsafe {
                &*(GetWindowLongPtrA(w, GWLP_USERDATA) as *const SharedMut<ContextMenuWindowState>)
            };

            // hiding by mouse hook: https://www.codeproject.com/Tips/751520/Custom-Context-Menu
            let windowpos = unsafe { &*(lp.0 as usize as *const WINDOWPOS) };

            if windowpos.flags.contains(SWP_SHOWWINDOW) {
                state.borrow_mut().mouse_hook_handle = Some(unsafe {
                    SetWindowsHookExA(WH_MOUSE, Some(Self::mouse_hook), None, GetCurrentThreadId())
                        .expect("Failed to register mouse hook")
                });
            } else if windowpos.flags.contains(SWP_HIDEWINDOW) {
                if let Some(hook_handle) = state.borrow_mut().mouse_hook_handle.take() {
                    unsafe { UnhookWindowsHookEx(hook_handle).expect("Failed to unhook mouse") };
                }
            }
        }
        if m == WM_MOUSEMOVE {
            let state = unsafe {
                &*(GetWindowLongPtrA(w, GWLP_USERDATA) as *const SharedMut<ContextMenuWindowState>)
            };
            let dpi = unsafe { GetDpiForWindow(w) as f32 };

            let (x, y) = ((lp.0 & 0xffff) as i16, ((lp.0 >> 16) & 0xffff) as i16);
            // ドロップシャドウの分あけているのでそのぶんずらす
            let (x, y) = (x as f32 - 32.0, y as f32 - 32.0);
            let actions = state
                .borrow_mut()
                .input_state
                .on_mouse_move((x as f32) * 96.0 / dpi, (y as f32) * 96.0 / dpi);
            for a in actions {
                a.execute(x as _, y as _, &mut *state.borrow_mut(), w);
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
            let state = unsafe {
                &*(GetWindowLongPtrA(w, GWLP_USERDATA) as *const SharedMut<ContextMenuWindowState>)
            };
            let dpi = unsafe { GetDpiForWindow(w) as f32 };

            // ドロップシャドウの分あけているのでそのぶんずらす
            let (x, y) = (
                (lp.0 & 0xffff) as i16 - 32,
                ((lp.0 >> 16) & 0xffff) as i16 - 32,
            );
            let actions = state
                .borrow_mut()
                .input_state
                .on_mouse_down((x as f32) * 96.0 / dpi, (y as f32) * 96.0 / dpi);
            for a in actions {
                a.execute(x as _, y as _, &mut *state.borrow_mut(), w);
            }

            return LRESULT(0);
        }
        if m == WM_LBUTTONUP {
            let state = unsafe {
                &*(GetWindowLongPtrA(w, GWLP_USERDATA) as *const SharedMut<ContextMenuWindowState>)
            };
            let dpi = unsafe { GetDpiForWindow(w) as f32 };

            // ドロップシャドウの分あけているのでそのぶんずらす
            let (x, y) = (
                (lp.0 & 0xffff) as i16 - 32,
                ((lp.0 >> 16) & 0xffff) as i16 - 32,
            );
            let actions = state
                .borrow_mut()
                .input_state
                .on_mouse_up((x as f32) * 96.0 / dpi, (y as f32) * 96.0 / dpi);
            for a in actions {
                a.execute(x as _, y as _, &mut *state.borrow_mut(), w);
            }

            return LRESULT(0);
        }
        if m == WM_MOUSELEAVE {
            let state = unsafe {
                &*(GetWindowLongPtrA(w, GWLP_USERDATA) as *const SharedMut<ContextMenuWindowState>)
            };

            let actions = state.borrow_mut().input_state.on_mouse_leave();
            for a in actions {
                a.execute(0.0, 0.0, &mut *state.borrow_mut(), w);
            }

            return LRESULT(0);
        }

        unsafe { DefWindowProcA(w, m, wp, lp) }
    }

    // hiding by mouse hook: https://www.codeproject.com/Tips/751520/Custom-Context-Menu
    extern "system" fn mouse_hook(code: i32, wp: WPARAM, lp: LPARAM) -> LRESULT {
        if WM_LBUTTONDOWN as usize <= wp.0 && wp.0 <= WM_MBUTTONDBLCLK as usize {
            let w = unsafe { FindWindowA(PCSTR(Self::window_class() as _), None) };
            if w.0 != 0 {
                let mut p = core::mem::MaybeUninit::<POINT>::uninit();
                let mut rc = core::mem::MaybeUninit::<RECT>::uninit();
                unsafe {
                    GetCursorPos(p.as_mut_ptr()).expect("Failed to get cursor pos");
                    GetWindowRect(w, rc.as_mut_ptr()).expect("Failed to get window rect");
                }

                if unsafe { !PtInRect(rc.assume_init_ref(), p.assume_init()).as_bool() } {
                    let _ = unsafe { ShowWindow(w, SW_HIDE) };
                }
            }
        }

        unsafe { CallNextHookEx(None, code, wp, lp) }
    }
}
