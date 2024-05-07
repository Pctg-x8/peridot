use std::{borrow::Cow, collections::HashMap};

use object_cache::{TextFormatStock, TextSurfaceStock};
use uikit::{InputEventHandler, ViewContext};
use windows::{
    core::*,
    Foundation::{
        Numerics::{Vector2, Vector3},
        Rect, TimeSpan, TypedEventHandler,
    },
    System::DispatcherQueueTimer,
    Win32::{
        Foundation::{BOOL, HWND, LPARAM, LRESULT, POINT, WPARAM},
        Graphics::{
            Direct2D::{
                D2D1CreateFactory, ID2D1Factory1, D2D1_DEBUG_LEVEL_WARNING, D2D1_FACTORY_OPTIONS,
                D2D1_FACTORY_TYPE_SINGLE_THREADED,
            },
            Direct3D::{D3D_DRIVER_TYPE_HARDWARE, D3D_FEATURE_LEVEL},
            Direct3D11::{
                D3D11CreateDevice, ID3D11Device, ID3D11DeviceContext,
                D3D11_CREATE_DEVICE_BGRA_SUPPORT, D3D11_SDK_VERSION,
            },
            DirectWrite::{
                DWriteCreateFactory, IDWriteFactory, DWRITE_FACTORY_TYPE_SHARED,
                DWRITE_FONT_WEIGHT_NORMAL, DWRITE_FONT_WEIGHT_SEMI_BOLD,
            },
            Dwm::{DwmSetWindowAttribute, DWMWINDOWATTRIBUTE},
            Dxgi::IDXGIDevice,
            Gdi::{MapWindowPoints, HBRUSH},
        },
        System::{
            LibraryLoader::GetModuleHandleA,
            WinRT::{
                Composition::{ICompositorDesktopInterop, ICompositorInterop},
                CreateDispatcherQueueController, DispatcherQueueOptions, DQTAT_COM_ASTA,
                DQTYPE_THREAD_CURRENT,
            },
        },
        UI::{
            HiDpi::GetDpiForWindow,
            Input::KeyboardAndMouse::{ReleaseCapture, SetCapture},
            WindowsAndMessaging::{
                DefWindowProcA, DispatchMessageA, GetClientRect, GetMessageA, GetWindowLongPtrA,
                LoadCursorA, LoadIconA, PostQuitMessage, SetWindowLongPtrA, SetWindowPos,
                ShowWindow, TranslateMessage, HCURSOR, HICON, IDC_ARROW, IDI_APPLICATION, MSG,
                SWP_NOACTIVATE, SWP_NOSIZE, SWP_NOZORDER, SW_HIDE, SW_SHOWNA, SW_SHOWNORMAL,
                WINDOW_LONG_PTR_INDEX, WM_DESTROY, WM_LBUTTONDOWN, WM_LBUTTONUP, WM_MOUSEMOVE,
                WM_SIZE, WNDCLASSEXA, WNDCLASS_STYLES,
            },
        },
    },
    UI::{
        Color,
        Composition::{
            AnimationIterationBehavior, CompositionAnimationGroup, CompositionEasingFunction,
            CompositionEasingFunctionMode, CompositionEffectSourceParameter,
            CompositionSurfaceBrush, ContainerVisual, Desktop::DesktopWindowTarget,
            ICompositionAnimation2, KeyFrameAnimation, LayerVisual, ScalarKeyFrameAnimation,
            ShapeVisual, SpriteVisual, Vector3KeyFrameAnimation, VisualCollection,
        },
    },
};

use crate::{
    uikit::UICommonObjects,
    winapi_extras::{register_window_class, WindowBuilder},
};

mod bindgen;
mod object_cache;
mod uikit;
mod utils;
mod winapi_extras;

const TAB_MARGIN_X: f32 = 10.0;
const TAB_MARGIN_Y: f32 = 2.0;
const TAB_RADIUS: f32 = 4.0;
const TAB_ACTIVE_LIT_COLOR: Color = Color {
    A: 255,
    R: 96,
    G: 255,
    B: 204,
};
const TAB_ACTIVE_BASE_COLOR: Color = Color {
    A: 255,
    R: 64,
    G: 160,
    B: 255,
};

#[repr(transparent)]
pub struct KeyFrameAnimationPropertySetter<'r, T: 'r + Interface>(&'r T);
impl<T: Interface> KeyFrameAnimationPropertySetter<'_, T> {
    #[inline]
    pub fn duration(&self, duration: TimeSpan) -> windows::core::Result<&Self> {
        self.0
            .cast::<KeyFrameAnimation>()?
            .SetDuration(duration)
            .map(|_| self)
    }

    #[inline]
    pub fn target(&self, target: &HSTRING) -> windows::core::Result<&Self> {
        let this = self.0.cast::<ICompositionAnimation2>()?;
        unsafe {
            (this.vtable().SetTarget)(this.as_raw(), core::mem::transmute_copy(target))
                .ok()
                .map(|_| self)
        }
    }
}

pub trait KeyFrameAnimationExtension {
    type Element;

    fn keyframe(&self, at: f32, value: Self::Element) -> windows::core::Result<&Self>;
    fn interpolate(
        &self,
        to: f32,
        to_value: Self::Element,
        f: impl windows_core::Param<CompositionEasingFunction>,
    ) -> windows::core::Result<&Self>;
}
impl KeyFrameAnimationExtension for ScalarKeyFrameAnimation {
    type Element = f32;

    fn keyframe(&self, at: f32, value: Self::Element) -> windows::core::Result<&Self> {
        self.InsertKeyFrame(at, value).map(|_| self)
    }
    fn interpolate(
        &self,
        to: f32,
        to_value: Self::Element,
        f: impl windows_core::Param<CompositionEasingFunction>,
    ) -> windows::core::Result<&Self> {
        self.InsertKeyFrameWithEasingFunction(to, to_value, f)
            .map(|_| self)
    }
}
impl KeyFrameAnimationExtension for Vector3KeyFrameAnimation {
    type Element = Vector3;

    fn keyframe(&self, at: f32, value: Self::Element) -> windows::core::Result<&Self> {
        self.InsertKeyFrame(at, value).map(|_| self)
    }
    fn interpolate(
        &self,
        to: f32,
        to_value: Self::Element,
        f: impl windows_core::Param<CompositionEasingFunction>,
    ) -> windows::core::Result<&Self> {
        self.InsertKeyFrameWithEasingFunction(to, to_value, f)
            .map(|_| self)
    }
}

pub trait KeyFrameAnimationPropertySetterExtension: Interface {
    fn set_properties(&self) -> KeyFrameAnimationPropertySetter<Self>;
}
impl KeyFrameAnimationPropertySetterExtension for KeyFrameAnimation {
    fn set_properties(&self) -> KeyFrameAnimationPropertySetter<Self> {
        KeyFrameAnimationPropertySetter(self)
    }
}
impl KeyFrameAnimationPropertySetterExtension for ScalarKeyFrameAnimation {
    fn set_properties(&self) -> KeyFrameAnimationPropertySetter<Self> {
        KeyFrameAnimationPropertySetter(self)
    }
}
impl KeyFrameAnimationPropertySetterExtension for Vector3KeyFrameAnimation {
    fn set_properties(&self) -> KeyFrameAnimationPropertySetter<Self> {
        KeyFrameAnimationPropertySetter(self)
    }
}

pub trait Vector2Extension {
    fn scalar(v: f32) -> Self;
    fn with_z(self, z: f32) -> Vector3;
}
impl Vector2Extension for Vector2 {
    #[inline(always)]
    fn scalar(v: f32) -> Self {
        Vector2 { X: v, Y: v }
    }

    #[inline(always)]
    fn with_z(self, z: f32) -> Vector3 {
        Vector3 {
            X: self.X,
            Y: self.Y,
            Z: z,
        }
    }
}

const PANE_SPLITTER_GAP: f32 = 4.0;

pub enum PaneDockState {
    Left(
        std::rc::Rc<core::cell::RefCell<PaneGroupView>>,
        f32,
        Box<PaneDockState>,
    ),
    Right(
        std::rc::Rc<core::cell::RefCell<PaneGroupView>>,
        f32,
        Box<PaneDockState>,
    ),
    Top(
        std::rc::Rc<core::cell::RefCell<PaneGroupView>>,
        f32,
        Box<PaneDockState>,
    ),
    Bottom(
        std::rc::Rc<core::cell::RefCell<PaneGroupView>>,
        f32,
        Box<PaneDockState>,
    ),
    Fill(std::rc::Rc<core::cell::RefCell<PaneGroupView>>),
}
impl PaneDockState {
    fn place_recursive(&self, onto: &VisualCollection) -> windows::core::Result<()> {
        match self {
            Self::Left(g, _, c)
            | Self::Right(g, _, c)
            | Self::Top(g, _, c)
            | Self::Bottom(g, _, c) => {
                onto.InsertAtTop(&g.borrow().root)?;
                c.place_recursive(onto)
            }
            Self::Fill(g) => onto.InsertAtTop(&g.borrow().root),
        }
    }

    fn layout(&self, region: Rect) -> windows::core::Result<()> {
        match self {
            &Self::Left(ref g, w, ref c) => {
                g.borrow_mut().set_offset_size(
                    region.X,
                    region.Y,
                    w.max(1.0),
                    region.Height.max(1.0),
                )?;
                let left_region = Rect {
                    X: region.X + w + PANE_SPLITTER_GAP,
                    Width: region.Width - w - PANE_SPLITTER_GAP,
                    ..region
                };
                c.layout(left_region)
            }
            &Self::Right(ref g, w, ref c) => {
                g.borrow_mut().set_offset_size(
                    region.X + region.Width - w,
                    region.Y,
                    w.max(1.0),
                    region.Height.max(1.0),
                )?;
                let left_region = Rect {
                    Width: region.Width - w - PANE_SPLITTER_GAP,
                    ..region
                };
                c.layout(left_region)
            }
            &Self::Top(ref g, h, ref c) => {
                g.borrow_mut().set_offset_size(
                    region.X,
                    region.Y,
                    region.Width.max(1.0),
                    h.max(1.0),
                )?;
                let left_region = Rect {
                    Y: region.Y + h + PANE_SPLITTER_GAP,
                    Height: region.Height - h - PANE_SPLITTER_GAP,
                    ..region
                };
                c.layout(left_region)
            }
            &Self::Bottom(ref g, h, ref c) => {
                g.borrow_mut().set_offset_size(
                    region.X,
                    region.Y + region.Height - h,
                    region.Width.max(1.0),
                    h.max(1.0),
                )?;
                let left_region = Rect {
                    Height: region.Height - h - PANE_SPLITTER_GAP,
                    ..region
                };
                c.layout(left_region)
            }
            Self::Fill(g) => {
                g.borrow_mut()
                    .set_offset_size(region.X, region.Y, region.Width, region.Height)
            }
        }
    }
}

pub struct PaneGroupDockingManager {
    docks: Option<PaneDockState>,
    placement_visual: ContainerVisual,
    floating_preview_window: HWND,
    _floating_preview_window_target: DesktopWindowTarget,
    pane_drag_preview: SpriteVisual,
    pane_drag_preview_color_tint: SpriteVisual,
    pane_drag_preview_animation: ScalarKeyFrameAnimation,
    pane_drag_preview_show_animation: CompositionAnimationGroup,
    pane_drag_preview_hide_animation: CompositionAnimationGroup,
    pane_drag_preview_hide_delay_timer:
        std::sync::Arc<std::sync::RwLock<Option<DispatcherQueueTimer>>>,
}
impl PaneGroupDockingManager {
    const FLOATING_PREVIEW_INOUT_DURATION: TimeSpan = TimeSpan {
        Duration: 10_000 * 100,
    };

    fn new(ctx: &mut ViewContext) -> windows::core::Result<Self> {
        extern "system" fn window_callback(h: HWND, m: u32, w: WPARAM, l: LPARAM) -> LRESULT {
            unsafe { DefWindowProcA(h, m, w, l) }
        }

        let floating_preview_window_cls = WNDCLASSEXA {
            cbSize: core::mem::size_of::<WNDCLASSEXA>() as _,
            cbClsExtra: 0,
            cbWndExtra: 0,
            style: WNDCLASS_STYLES(0),
            lpfnWndProc: Some(window_callback),
            hInstance: unsafe { windows::Win32::Foundation::HINSTANCE(GetModuleHandleA(None)?.0) },
            hIcon: HICON(0),
            hCursor: HCURSOR(0),
            hbrBackground: HBRUSH(0),
            lpszMenuName: PCSTR::null(),
            lpszClassName: s!("io.ct2.peridot.marble_editor.overlay.floating_preview"),
            hIconSm: HICON(0),
        };
        let floating_preview_window = WindowBuilder::new(
            floating_preview_window_cls.hInstance,
            register_window_class(&floating_preview_window_cls)?,
            s!(""),
        )
        .no_activate()
        .no_redirection_bitmap()
        .transparent()
        .topmost()
        .popup()
        .create()?;
        let floating_preview_window_target = unsafe {
            ctx.compositor
                .cast::<ICompositorDesktopInterop>()?
                .CreateDesktopWindowTarget(floating_preview_window, true)?
        };

        let pane_drag_preview = ctx.compositor.CreateSpriteVisual()?;
        pane_drag_preview.SetCenterPoint(Vector3 {
            X: 0.5,
            Y: 0.5,
            Z: 0.5,
        })?;
        pane_drag_preview.SetAnchorPoint(Vector2 { X: 0.5, Y: 0.5 })?;
        pane_drag_preview.SetRelativeOffsetAdjustment(Vector3 {
            X: 0.5,
            Y: 0.5,
            Z: 0.0,
        })?;
        let fx = bindgen::GaussianBlurEffect::new()?;
        fx.SetSource(&CompositionEffectSourceParameter::Create(h!("source"))?)?;
        fx.SetBlurAmount(16.0)?;
        fx.SetOptimization(bindgen::EffectOptimization::Balanced)?;
        let effect_factory = ctx.compositor.CreateEffectFactory(&fx)?;
        let backdrop_brush = ctx.compositor.CreateBackdropBrush()?;
        let blur_brush = effect_factory.CreateBrush()?;
        blur_brush.SetSourceParameter(h!("source"), &backdrop_brush)?;
        pane_drag_preview.SetBrush(&blur_brush)?;
        let pane_drag_preview_color_tint = ctx.compositor.CreateSpriteVisual()?;
        pane_drag_preview_color_tint.SetBrush(&ctx.compositor.CreateColorBrushWithColor(
            Color {
                A: 128,
                R: 16,
                G: 192,
                B: 255,
            },
        )?)?;
        pane_drag_preview_color_tint.SetRelativeOffsetAdjustment(Vector3::zero())?;
        pane_drag_preview_color_tint.SetRelativeSizeAdjustment(Vector2::one())?;
        pane_drag_preview
            .Children()?
            .InsertAtTop(&pane_drag_preview_color_tint)?;
        let shadow = ctx.compositor.CreateDropShadow()?;
        shadow.SetBlurRadius(32.0)?;
        shadow.SetOffset(Vector3 {
            X: 0.0,
            Y: 16.0,
            Z: 0.0,
        })?;
        shadow.SetOpacity(0.3)?;
        pane_drag_preview.SetShadow(&shadow)?;
        let pane_drag_preview_animation = ctx.compositor.CreateScalarKeyFrameAnimation()?;
        let linear_fn = ctx.compositor.CreateLinearEasingFunction()?;
        pane_drag_preview_animation.SetIterationBehavior(AnimationIterationBehavior::Forever)?;
        pane_drag_preview_animation.InsertKeyFrame(0.0, 1.0)?;
        pane_drag_preview_animation.InsertKeyFrameWithEasingFunction(0.5, 0.75, &linear_fn)?;
        pane_drag_preview_animation.InsertKeyFrameWithEasingFunction(1.0, 1.0, &linear_fn)?;
        pane_drag_preview_animation.SetDuration(TimeSpan {
            Duration: 10_000 * 2600,
        })?;

        let pane_drag_preview_show_animation = ctx.compositor.CreateAnimationGroup()?;
        let linear_easing = ctx.compositor.CreateLinearEasingFunction()?;
        pane_drag_preview_show_animation.Add(&{
            let a = ctx.compositor.CreateScalarKeyFrameAnimation()?;
            a.set_properties()
                .duration(Self::FLOATING_PREVIEW_INOUT_DURATION)?
                .target(h!("Opacity"))?;
            a.keyframe(0.0, 0.0)?
                .interpolate(1.0, 1.0, &linear_easing)?;

            a
        })?;
        pane_drag_preview_show_animation.Add(&{
            let a = ctx.compositor.CreateVector3KeyFrameAnimation()?;
            a.set_properties()
                .duration(Self::FLOATING_PREVIEW_INOUT_DURATION)?
                .target(h!("Scale"))?;
            a.keyframe(0.0, Vector2::scalar(0.9).with_z(1.0))?
                .interpolate(
                    1.0,
                    Vector3::one(),
                    &CompositionEasingFunction::CreateBackEasingFunction(
                        ctx.compositor,
                        CompositionEasingFunctionMode::Out,
                        1.1,
                    )?,
                )?;

            a
        })?;
        let pane_drag_preview_hide_animation = ctx.compositor.CreateAnimationGroup()?;
        pane_drag_preview_hide_animation.Add(&{
            let a = ctx.compositor.CreateScalarKeyFrameAnimation()?;
            a.set_properties()
                .duration(Self::FLOATING_PREVIEW_INOUT_DURATION)?
                .target(h!("Opacity"))?;
            a.keyframe(0.0, 1.0)?
                .interpolate(1.0, 0.0, &linear_easing)?;

            a
        })?;
        pane_drag_preview_hide_animation.Add(&{
            let a = ctx.compositor.CreateVector3KeyFrameAnimation()?;
            a.set_properties()
                .duration(Self::FLOATING_PREVIEW_INOUT_DURATION)?
                .target(h!("Scale"))?;
            a.keyframe(0.0, Vector3::one())?.interpolate(
                1.0,
                Vector2::scalar(0.9).with_z(1.0),
                &linear_easing,
            )?;

            a
        })?;

        floating_preview_window_target.SetRoot(&pane_drag_preview)?;
        let placement_visual = ctx.compositor.CreateContainerVisual()?;

        Ok(Self {
            docks: None,
            placement_visual,
            floating_preview_window,
            _floating_preview_window_target: floating_preview_window_target,
            pane_drag_preview,
            pane_drag_preview_color_tint,
            pane_drag_preview_animation,
            pane_drag_preview_show_animation,
            pane_drag_preview_hide_animation,
            pane_drag_preview_hide_delay_timer: std::sync::Arc::new(std::sync::RwLock::new(None)),
        })
    }

    fn set_layout(&mut self, layout: PaneDockState) -> windows::core::Result<()> {
        let children = self.placement_visual.Children()?;
        children.RemoveAll()?;
        layout.place_recursive(&children)?;

        self.docks = Some(layout);
        Ok(())
    }
    fn resize_root(&mut self, width: f32, height: f32) -> windows::core::Result<()> {
        if let Some(ref docks) = self.docks {
            docks.layout(Rect {
                X: 0.0,
                Y: 0.0,
                Width: width,
                Height: height,
            })?;
        }

        Ok(())
    }

    fn show_preview(&self) -> windows::core::Result<()> {
        drop(
            self.pane_drag_preview_hide_delay_timer
                .write()
                .unwrap()
                .take(),
        );

        unsafe {
            let _ = ShowWindow(self.floating_preview_window, SW_SHOWNA);
        }
        self.pane_drag_preview_color_tint
            .StartAnimation(h!("Opacity"), &self.pane_drag_preview_animation)?;
        self.pane_drag_preview
            .StartAnimationGroup(&self.pane_drag_preview_show_animation)?;

        Ok(())
    }

    fn hide_preview(&self) -> windows::core::Result<()> {
        self.pane_drag_preview
            .StartAnimationGroup(&self.pane_drag_preview_hide_animation)?;
        let delay_hide = self.pane_drag_preview.DispatcherQueue()?.CreateTimer()?;
        delay_hide.SetInterval(Self::FLOATING_PREVIEW_INOUT_DURATION)?;
        let tint = self.pane_drag_preview_color_tint.clone();
        let delay_timer = self.pane_drag_preview_hide_delay_timer.clone();
        let w = self.floating_preview_window;
        delay_hide.Tick(&TypedEventHandler::new(move |_, _| {
            tint.StopAnimation(h!("Opacity"))?;
            unsafe {
                let _ = ShowWindow(w, SW_HIDE);
            }
            drop(delay_timer.write().unwrap().take());

            Ok(())
        }))?;
        *self.pane_drag_preview_hide_delay_timer.write().unwrap() = Some(delay_hide);

        Ok(())
    }

    fn set_preview_pos(&self, left: f32, top: f32) -> windows::core::Result<()> {
        unsafe {
            SetWindowPos(
                self.floating_preview_window,
                None,
                left as i32 - 32,
                top as i32 - 32,
                0,
                0,
                SWP_NOZORDER | SWP_NOSIZE | SWP_NOACTIVATE,
            )?;
        }

        Ok(())
    }

    fn set_preview_rect(
        &self,
        left: f32,
        top: f32,
        width: f32,
        height: f32,
    ) -> windows::core::Result<()> {
        self.pane_drag_preview.SetSize(Vector2 {
            X: width,
            Y: height,
        })?;

        unsafe {
            SetWindowPos(
                self.floating_preview_window,
                None,
                left as i32 - 32,
                top as i32 - 32,
                width as i32 + 64,
                height as i32 + 64,
                SWP_NOZORDER | SWP_NOACTIVATE,
            )?;
        }

        Ok(())
    }
}

pub struct PaneGroupView {
    root: ContainerVisual,
    content_area: ContainerVisual,
    content_area_base: SpriteVisual,
    ht_ref: std::rc::Rc<core::cell::RefCell<HitTestTree>>,
    ht_ref_content: std::rc::Rc<core::cell::RefCell<HitTestTree>>,
    current_active: usize,
    tab_height: f32,
    width: f32,
    height: f32,
    tabs: Vec<(
        std::rc::Rc<core::cell::RefCell<PaneTabHeaderView>>,
        std::rc::Rc<core::cell::RefCell<dyn PaneTabContentPresenter>>,
    )>,
    drag_base_point: Option<(f32, f32, f32, f32)>,
}
impl PaneGroupView {
    pub fn new(
        ctx: &mut ViewContext,
    ) -> windows::core::Result<std::rc::Rc<core::cell::RefCell<Self>>> {
        let root = ctx.compositor.CreateContainerVisual()?;
        root.SetSize(Vector2 { X: 128.0, Y: 128.0 })?;
        let content_area = ctx.compositor.CreateContainerVisual()?;
        content_area.SetRelativeSizeAdjustment(Vector2 { X: 1.0, Y: 1.0 })?;
        root.Children()?.InsertAtBottom(&content_area)?;
        let content_area_base = ctx.compositor.CreateSpriteVisual()?;
        content_area_base.SetBrush(&{
            let b = ctx.compositor.CreateColorBrush()?;
            b.SetColor(Color {
                A: 255,
                R: 64,
                G: 64,
                B: 72,
            })?;
            b
        })?;
        content_area_base.SetRelativeOffsetAdjustment(Vector3 {
            X: 0.0,
            Y: 0.0,
            Z: 0.0,
        })?;
        content_area_base.SetRelativeSizeAdjustment(Vector2 { X: 1.0, Y: 1.0 })?;
        root.Children()?.InsertAtBottom(&content_area_base)?;
        root.SetClip(
            &ctx.compositor
                .CreateInsetClipWithInsets(0.0, 0.0, 0.0, 0.0)?,
        )?;

        Ok(std::rc::Rc::<core::cell::RefCell<Self>>::new_cyclic(
            |wthis| {
                let ht = std::rc::Rc::new(core::cell::RefCell::new(HitTestTree::new(
                    Box::new(wthis.clone()),
                    ctx.hittest_context.new_id(),
                    0.0,
                    0.0,
                    128.0,
                    128.0,
                )));
                ctx.hittest_tree_parent_mut().add_child(&ht);
                let ht_content = std::rc::Rc::new(core::cell::RefCell::new(HitTestTree::new(
                    Box::new(wthis.clone()),
                    ctx.hittest_context.new_id(),
                    0.0,
                    0.0,
                    128.0,
                    128.0,
                )));
                ht.borrow_mut().add_child(&ht_content);

                core::cell::RefCell::new(Self {
                    root,
                    content_area,
                    content_area_base,
                    ht_ref: ht,
                    ht_ref_content: ht_content,
                    current_active: 0,
                    tab_height: 0.0,
                    width: 128.0,
                    height: 128.0,
                    tabs: Vec::new(),
                    drag_base_point: None,
                })
            },
        ))
    }

    pub fn add_tab<T: PaneTabPresenter + 'static>(
        this: &std::rc::Rc<core::cell::RefCell<Self>>,
        ctx: &mut ViewContext,
    ) -> windows::core::Result<std::rc::Rc<core::cell::RefCell<T>>> {
        let mut thisref = this.borrow_mut();
        let header_view = PaneTabHeaderView::new(
            this,
            thisref.tabs.len(),
            T::INIT_TAB_NAME,
            thisref.tabs.is_empty(),
            &mut ctx.on_new_hittest_tree(&thisref.ht_ref),
        )?;
        let content_presenter = std::rc::Rc::new(core::cell::RefCell::new(T::new(&header_view)));
        thisref
            .tabs
            .push((header_view.clone(), content_presenter.clone()));
        thisref
            .root
            .Children()?
            .InsertAtTop(&header_view.borrow().visual)?;

        if thisref.tabs.len() == 1 {
            // first tab
            thisref.tabs[0].1.borrow_mut().build_content_view(
                &thisref.content_area,
                &mut ctx.on_new_hittest_tree(&thisref.ht_ref_content),
            )?;
        }

        Ok(content_presenter)
    }

    fn readjust_content_area(&mut self) -> windows::core::Result<()> {
        self.content_area.SetOffset(Vector3 {
            X: 0.0,
            Y: self.tab_height,
            Z: 0.0,
        })?;
        self.content_area.SetSize(Vector2 {
            X: self.width,
            Y: (self.height - self.tab_height).max(0.0),
        })?;
        self.content_area_base.SetOffset(Vector3 {
            X: 0.0,
            Y: self.tab_height,
            Z: 0.0,
        })?;
        self.content_area_base.SetSize(Vector2 {
            X: self.width,
            Y: (self.height - self.tab_height).max(0.0),
        })?;

        self.ht_ref_content.borrow_mut().top = self.tab_height;
        self.ht_ref_content
            .borrow_mut()
            .set_size(self.width, (self.height - self.tab_height).max(0.0));
        Ok(())
    }

    pub fn rearrange(&mut self) {
        let mut offset = 0.0;
        self.tab_height = 0.0f32;
        for v in self.tabs.iter() {
            v.0.borrow()
                .set_offset(offset, 0.0)
                .expect("Failed to set tab offset");
            offset += v.0.borrow().width;
            self.tab_height = self.tab_height.max(v.0.borrow().height);
        }

        self.readjust_content_area()
            .expect("Failed to readjust content area");
    }

    pub fn set_offset_size(
        &mut self,
        left: f32,
        top: f32,
        width: f32,
        height: f32,
    ) -> windows::core::Result<()> {
        self.root.SetOffset(Vector3 {
            X: left,
            Y: top,
            Z: 0.0,
        })?;
        self.root.SetSize(Vector2 {
            X: width,
            Y: height,
        })?;
        self.ht_ref.borrow_mut().set_rect(left, top, width, height);
        self.width = width;
        self.height = height;

        self.readjust_content_area()?;
        Ok(())
    }

    pub fn switch_active(
        &mut self,
        new_active: usize,
        view_ctx: &mut ViewContext,
    ) -> windows::core::Result<()> {
        let new_active = new_active.min(self.tabs.len());
        if self.current_active == new_active {
            // 変わってないのでなにもしない
            return Ok(());
        }

        self.tabs[self.current_active]
            .1
            .borrow_mut()
            .on_hide_content_view(view_ctx)?;
        self.tabs[self.current_active]
            .0
            .borrow_mut()
            .set_active(false, view_ctx)?;
        self.content_area.Children()?.RemoveAll()?;
        self.current_active = new_active;
        self.tabs[self.current_active]
            .1
            .borrow_mut()
            .build_content_view(
                &self.content_area,
                &mut view_ctx.on_new_hittest_tree(&self.ht_ref_content),
            )?;
        self.tabs[self.current_active]
            .0
            .borrow_mut()
            .set_active(true, view_ctx)?;

        Ok(())
    }
}
impl InputEventHandler for std::rc::Weak<core::cell::RefCell<PaneGroupView>> {
    fn on_begin_drag(
        &self,
        x: f32,
        y: f32,
        window: HWND,
        _view_ctx: &mut ViewContext,
        pane_group_docking_manager: &core::cell::RefCell<PaneGroupDockingManager>,
    ) {
        if let Some(t) = self.upgrade() {
            pane_group_docking_manager
                .borrow()
                .show_preview()
                .expect("Failed to show floating preview");
            let mut thisref = t.borrow_mut();
            let HitTestTree {
                left,
                top,
                width,
                height,
                ..
            } = *thisref.ht_ref.borrow();
            let dpi = unsafe { GetDpiForWindow(window) as f32 };
            thisref.drag_base_point = Some((x, y, left, top));
            let mut loc = [POINT {
                x: (left * dpi / 96.0) as _,
                y: (top * dpi / 96.0) as _,
            }];
            unsafe { MapWindowPoints(window, None, &mut loc) };
            pane_group_docking_manager
                .borrow()
                .set_preview_rect(
                    loc[0].x as _,
                    loc[0].y as _,
                    width * dpi / 96.0,
                    height * dpi / 96.0,
                )
                .expect("Failed to update preview rect");
            unsafe {
                SetCapture(window);
            }
        }
    }
    fn on_drag_move(
        &self,
        x: f32,
        y: f32,
        window: HWND,
        _view_ctx: &mut ViewContext,
        pane_group_docking_manager: &core::cell::RefCell<PaneGroupDockingManager>,
    ) {
        let Some(thisref) = self.upgrade() else {
            return;
        };
        let Some((bx, by, ox, oy)) = thisref.borrow().drag_base_point else {
            return;
        };

        let dpi = unsafe { GetDpiForWindow(window) as f32 };
        let mut loc = [POINT {
            x: ((ox + (x - bx)) * dpi / 96.0) as _,
            y: ((oy + (y - by)) * dpi / 96.0) as _,
        }];
        unsafe { MapWindowPoints(window, None, &mut loc) };
        pane_group_docking_manager
            .borrow()
            .set_preview_pos(loc[0].x as _, loc[0].y as _)
            .expect("Failed to update preview rect");
    }
    fn on_end_drag(
        &self,
        _window: HWND,
        _view_ctx: &mut ViewContext,
        pane_group_docking_manager: &core::cell::RefCell<PaneGroupDockingManager>,
    ) {
        if let Some(_) = self.upgrade() {
            pane_group_docking_manager
                .borrow()
                .hide_preview()
                .expect("Failed to show floating preview");
            unsafe {
                let _ = ReleaseCapture();
            }
        }
    }
}

pub struct PaneTabHeaderView {
    label: Cow<'static, str>,
    visual: LayerVisual,
    bg_visual: ShapeVisual,
    active_overlay_visual: ShapeVisual,
    label_content_brush: CompositionSurfaceBrush,
    bg_hover_animation: ScalarKeyFrameAnimation,
    bg_hover_end_animation: ScalarKeyFrameAnimation,
    active_overlay_enter_animation: ScalarKeyFrameAnimation,
    active_overlay_leave_animation: ScalarKeyFrameAnimation,
    hittest_tree_self: std::rc::Rc<core::cell::RefCell<HitTestTree>>,
    bg_active: bool,
    is_active: bool,
    width: f32,
    height: f32,
}
impl PaneTabHeaderView {
    pub fn new(
        group_view: &std::rc::Rc<core::cell::RefCell<PaneGroupView>>,
        index_in_group: usize,
        title: impl Into<Cow<'static, str>>,
        init_active: bool,
        ctx: &mut ViewContext,
    ) -> windows::core::Result<std::rc::Rc<core::cell::RefCell<Self>>> {
        let base = ctx.compositor.CreateLayerVisual()?;
        let title = title.into();
        let title_text = ctx.text_surface_stock.get(
            if init_active {
                &ctx.common.tab_active_title_font
            } else {
                &ctx.common.tab_title_font
            },
            title.clone(),
        )?;
        let view_size = Vector2 {
            X: title_text.width + TAB_MARGIN_X * 2.0,
            Y: title_text.height + TAB_MARGIN_Y * 2.0,
        };
        let label_content_brush = ctx
            .compositor
            .CreateSurfaceBrushWithSurface(&title_text.surface)?;
        base.Children()
            .expect("Failed to get children collection")
            .InsertAtTop(&{
                let v = ctx.compositor.CreateSpriteVisual()?;
                v.SetBrush(&label_content_brush)?;
                v.SetSize(title_text.visual_size())?;
                v.SetOffset(Vector3 {
                    X: TAB_MARGIN_X,
                    Y: TAB_MARGIN_Y,
                    Z: 0.0,
                })?;

                v
            })
            .expect("Failed to insert visual");

        let geometry = {
            let g = ctx.compositor.CreateRoundedRectangleGeometry()?;
            g.SetCornerRadius(Vector2 {
                X: TAB_RADIUS,
                Y: TAB_RADIUS,
            })
            .expect("Failed to set corner radius");
            g.SetSize(Vector2 {
                X: title_text.width + TAB_MARGIN_X * 2.0,
                Y: (title_text.height + TAB_MARGIN_Y * 2.0) * 2.0,
            })?;

            g
        };

        let bg = {
            let shape = ctx.compositor.CreateSpriteShapeWithGeometry(&geometry)?;
            shape.SetFillBrush(&ctx.common.tab_base_brush)?;

            let v = ctx.compositor.CreateShapeVisual()?;
            v.Shapes()?.Append(&shape)?;
            v.SetSize(view_size.clone())?;
            v
        };
        let active_overlay = {
            let shape = ctx.compositor.CreateSpriteShapeWithGeometry(&geometry)?;
            shape.SetFillBrush(&ctx.common.tab_active_overlay_brush)?;

            let v = ctx.compositor.CreateShapeVisual()?;
            v.Shapes()?.Append(&shape)?;
            v.SetSize(view_size.clone())?;
            v
        };

        if init_active {
            bg.SetOpacity(1.0)?;
            active_overlay.SetOpacity(1.0)?;
        } else {
            bg.SetOpacity(0.0)?;
            active_overlay.SetOpacity(0.0)?;
        }

        let children = base.Children()?;
        children.InsertAtBottom(&active_overlay)?;
        children.InsertAtBottom(&bg)?;

        Ok(std::rc::Rc::<core::cell::RefCell<Self>>::new_cyclic(
            |wthis| {
                let ht_id = ctx.hittest_context.new_id();
                let ht_self = std::rc::Rc::new(core::cell::RefCell::new(HitTestTree::new(
                    Box::new(PaneTabHeaderViewInputEventHandler {
                        group_view: group_view.clone(),
                        index_in_group,
                        self_ref: wthis.clone(),
                    }),
                    ht_id,
                    0.0,
                    0.0,
                    view_size.X,
                    view_size.Y,
                )));
                ctx.hittest_tree_parent_mut().add_child(&ht_self);

                core::cell::RefCell::new(Self {
                    label: title,
                    visual: base,
                    bg_visual: bg,
                    active_overlay_visual: active_overlay,
                    label_content_brush,
                    bg_hover_animation: ctx.common.tab_hover_animation.clone(),
                    bg_hover_end_animation: ctx.common.tab_hover_end_animation.clone(),
                    active_overlay_enter_animation: ctx
                        .common
                        .tab_active_overlay_enter_animation
                        .clone(),
                    active_overlay_leave_animation: ctx
                        .common
                        .tab_active_overlay_leave_animation
                        .clone(),
                    hittest_tree_self: ht_self,
                    bg_active: init_active,
                    is_active: init_active,
                    width: view_size.X,
                    height: view_size.Y,
                })
            },
        ))
    }

    fn activate_bg(&mut self) -> windows::core::Result<()> {
        if self.bg_active {
            return Ok(());
        }

        self.bg_visual
            .StartAnimation(h!("Opacity"), &self.bg_hover_animation)?;
        self.bg_active = true;
        Ok(())
    }
    fn deactivate_bg(&mut self) -> windows::core::Result<()> {
        if !self.bg_active {
            return Ok(());
        }

        if self.is_active {
            // アクティブ状態のときは非アクティブにできない
            return Ok(());
        }

        self.bg_visual
            .StartAnimation(h!("Opacity"), &self.bg_hover_end_animation)?;
        self.bg_active = false;
        Ok(())
    }

    pub fn set_offset(&self, left: f32, top: f32) -> windows::core::Result<()> {
        self.visual.SetOffset(Vector3 {
            X: left,
            Y: top,
            Z: 0.0,
        })?;
        self.hittest_tree_self.borrow_mut().set_offset(left, top);

        Ok(())
    }
    pub fn set_active(
        &mut self,
        is_active: bool,
        view_ctx: &mut ViewContext,
    ) -> windows::core::Result<()> {
        let requires_transition = self.is_active != is_active;
        self.is_active = is_active;

        if self.is_active {
            self.activate_bg()?;
        } else {
            self.deactivate_bg()?;
        }

        if requires_transition {
            self.active_overlay_visual.StartAnimation(
                h!("Opacity"),
                if is_active {
                    &self.active_overlay_enter_animation
                } else {
                    &self.active_overlay_leave_animation
                },
            )?;
            let new_label_surface = view_ctx.text_surface_stock.get(
                if is_active {
                    &view_ctx.common.tab_active_title_font
                } else {
                    &view_ctx.common.tab_title_font
                },
                self.label.clone(),
            )?;
            self.label_content_brush
                .SetSurface(&new_label_surface.surface)?;
        }

        Ok(())
    }
}
pub struct PaneTabHeaderViewInputEventHandler {
    // Note: アクティブ切り替え時にgroup_viewとselfを同時に見るのでgroup_viewの参照ルートを切り離す
    // これが必要なのややこいのでうまい仕組み考えなおしたいな......
    group_view: std::rc::Rc<core::cell::RefCell<PaneGroupView>>,
    index_in_group: usize,
    self_ref: std::rc::Weak<core::cell::RefCell<PaneTabHeaderView>>,
}
impl InputEventHandler for PaneTabHeaderViewInputEventHandler {
    fn on_pointer_enter(&self, _view_ctx: &mut ViewContext) {
        let Some(x) = self.self_ref.upgrade() else {
            return;
        };

        println!("MouseEnter: {}", x.borrow().hittest_tree_self.borrow().id);

        x.borrow_mut().activate_bg().expect("Failed to activate bg");
    }
    fn on_pointer_leave(&self, _view_ctx: &mut ViewContext) {
        let Some(x) = self.self_ref.upgrade() else {
            return;
        };

        println!("MouseLeave: {}", x.borrow().hittest_tree_self.borrow().id);

        x.borrow_mut()
            .deactivate_bg()
            .expect("Failed to deactivate bg");
    }
    fn on_click(&self, view_ctx: &mut ViewContext) {
        self.group_view
            .borrow_mut()
            .switch_active(self.index_in_group, view_ctx)
            .expect("Failed to transition");
    }
}

pub trait PaneTabContentPresenter {
    fn build_content_view(
        &mut self,
        onto: &ContainerVisual,
        view_context: &mut ViewContext,
    ) -> windows::core::Result<()>;
    fn on_hide_content_view(&mut self, view_context: &mut ViewContext)
        -> windows::core::Result<()>;
}
pub trait PaneTabPresenter: PaneTabContentPresenter {
    const INIT_TAB_NAME: &'static str;
    fn new(_tab_header_view: &std::rc::Rc<core::cell::RefCell<PaneTabHeaderView>>) -> Self;
}

pub struct InspectorTabPresenter {}
impl PaneTabContentPresenter for InspectorTabPresenter {
    fn build_content_view(
        &mut self,
        onto: &ContainerVisual,
        view_context: &mut ViewContext,
    ) -> windows::core::Result<()> {
        let ui_font =
            view_context
                .text_format_stock
                .get("system-ui", 12.0, DWRITE_FONT_WEIGHT_NORMAL)?;
        let label_surface = view_context
            .text_surface_stock
            .get(&ui_font, "Inspector Pane")?;
        let brush = view_context
            .compositor
            .CreateSurfaceBrushWithSurface(&label_surface.surface)?;
        let label_visual = view_context.compositor.CreateSpriteVisual()?;
        label_visual.SetBrush(&brush)?;
        label_visual.SetSize(label_surface.visual_size())?;
        onto.Children()?.InsertAtTop(&label_visual)?;

        Ok(())
    }

    fn on_hide_content_view(
        &mut self,
        _view_context: &mut ViewContext,
    ) -> windows::core::Result<()> {
        Ok(())
    }
}
impl PaneTabPresenter for InspectorTabPresenter {
    const INIT_TAB_NAME: &'static str = "Inspector";

    fn new(_tab_header_view: &std::rc::Rc<core::cell::RefCell<PaneTabHeaderView>>) -> Self {
        Self {}
    }
}

pub struct ProjectSettingsTabPresenter {}
impl PaneTabContentPresenter for ProjectSettingsTabPresenter {
    fn build_content_view(
        &mut self,
        _onto: &ContainerVisual,
        _view_context: &mut ViewContext,
    ) -> windows::core::Result<()> {
        Ok(())
    }

    fn on_hide_content_view(
        &mut self,
        _view_context: &mut ViewContext,
    ) -> windows::core::Result<()> {
        Ok(())
    }
}
impl PaneTabPresenter for ProjectSettingsTabPresenter {
    const INIT_TAB_NAME: &'static str = "Project Settings";

    fn new(_tab_header_view: &std::rc::Rc<core::cell::RefCell<PaneTabHeaderView>>) -> Self {
        Self {}
    }
}

pub struct TimelineTabPresenter {}
impl PaneTabContentPresenter for TimelineTabPresenter {
    fn build_content_view(
        &mut self,
        _onto: &ContainerVisual,
        _view_context: &mut ViewContext,
    ) -> windows::core::Result<()> {
        Ok(())
    }

    fn on_hide_content_view(
        &mut self,
        _view_context: &mut ViewContext,
    ) -> windows::core::Result<()> {
        Ok(())
    }
}
impl PaneTabPresenter for TimelineTabPresenter {
    const INIT_TAB_NAME: &'static str = "Timeline";

    fn new(_tab_header_view: &std::rc::Rc<core::cell::RefCell<PaneTabHeaderView>>) -> Self {
        Self {}
    }
}

pub struct StageTabPresenter {}
impl PaneTabContentPresenter for StageTabPresenter {
    fn build_content_view(
        &mut self,
        _onto: &ContainerVisual,
        _view_context: &mut ViewContext,
    ) -> windows::core::Result<()> {
        Ok(())
    }

    fn on_hide_content_view(
        &mut self,
        _view_context: &mut ViewContext,
    ) -> windows::core::Result<()> {
        Ok(())
    }
}
impl PaneTabPresenter for StageTabPresenter {
    const INIT_TAB_NAME: &'static str = "Stage";

    fn new(_tab_header_view: &std::rc::Rc<core::cell::RefCell<PaneTabHeaderView>>) -> Self {
        Self {}
    }
}

pub struct PreviewTabPresenter {}
impl PaneTabContentPresenter for PreviewTabPresenter {
    fn build_content_view(
        &mut self,
        _onto: &ContainerVisual,
        _view_context: &mut ViewContext,
    ) -> windows::core::Result<()> {
        Ok(())
    }

    fn on_hide_content_view(
        &mut self,
        _view_context: &mut ViewContext,
    ) -> windows::core::Result<()> {
        Ok(())
    }
}
impl PaneTabPresenter for PreviewTabPresenter {
    const INIT_TAB_NAME: &'static str = "Preview";

    fn new(_tab_header_view: &std::rc::Rc<core::cell::RefCell<PaneTabHeaderView>>) -> Self {
        Self {}
    }
}

const DRAG_THRESHOLD_DIST2: f32 = 5.0 * 5.0;
struct InputState {
    ht_tree: std::rc::Rc<core::cell::RefCell<HitTestTree>>,
    mouse_current_enter_element: Option<std::rc::Weak<core::cell::RefCell<HitTestTree>>>,
    mouse_down_point: Option<(
        f32,
        f32,
        Option<std::rc::Weak<core::cell::RefCell<HitTestTree>>>,
    )>,
    is_mouse_dragging: bool,
}
impl InputState {
    fn new(ht_tree: &std::rc::Rc<core::cell::RefCell<HitTestTree>>) -> Self {
        Self {
            ht_tree: ht_tree.clone(),
            mouse_current_enter_element: None,
            mouse_down_point: None,
            is_mouse_dragging: false,
        }
    }

    fn update_mouse_pos(&mut self, x: f32, y: f32, view_ctx: &mut ViewContext) {
        let over_tree = HitTestTree::check(&self.ht_tree, x, y);
        let over_changes = over_tree.as_ref().map(|x| x.borrow().id)
            != self
                .mouse_current_enter_element
                .as_ref()
                .and_then(std::rc::Weak::upgrade)
                .map(|x| x.borrow().id);
        if let Some(x) = self
            .mouse_current_enter_element
            .as_ref()
            .and_then(std::rc::Weak::upgrade)
        {
            if Some(x.borrow().id) != over_tree.as_ref().map(|x| x.borrow().id) {
                // leave
                x.borrow().eh.on_pointer_leave(view_ctx);
            }
        }
        self.mouse_current_enter_element = over_tree.as_ref().map(std::rc::Rc::downgrade);
        if over_changes {
            if let Some(x) = self
                .mouse_current_enter_element
                .as_ref()
                .and_then(std::rc::Weak::upgrade)
            {
                x.borrow().eh.on_pointer_enter(view_ctx);
            }
        }
    }

    fn on_mouse_move(
        &mut self,
        x: f32,
        y: f32,
        window: HWND,
        view_ctx: &mut ViewContext,
        pane_group_docking_manager: &core::cell::RefCell<PaneGroupDockingManager>,
    ) {
        self.update_mouse_pos(x, y, view_ctx);

        if let Some((dx, dy, down_element)) = self.mouse_down_point.as_ref() {
            if !self.is_mouse_dragging {
                // 閾値を超えた後は永続的にドラッグ状態になる
                let dist2 = (dx - x).powi(2) + (dy - y).powi(2);
                if dist2 >= DRAG_THRESHOLD_DIST2 {
                    self.is_mouse_dragging = true;
                    if let Some(e) = down_element.as_ref().and_then(std::rc::Weak::upgrade) {
                        e.borrow().eh.on_begin_drag(
                            x,
                            y,
                            window,
                            view_ctx,
                            pane_group_docking_manager,
                        );
                    }
                }
            }

            if self.is_mouse_dragging {
                if let Some(e) = down_element.as_ref().and_then(std::rc::Weak::upgrade) {
                    e.borrow()
                        .eh
                        .on_drag_move(x, y, window, view_ctx, pane_group_docking_manager);
                }
            }
        }
    }

    fn on_mouse_down(&mut self, x: f32, y: f32, view_ctx: &mut ViewContext) {
        self.update_mouse_pos(x, y, view_ctx);
        self.mouse_down_point = Some((x, y, self.mouse_current_enter_element.clone()));
        self.is_mouse_dragging = false;
    }

    fn on_mouse_up(
        &mut self,
        x: f32,
        y: f32,
        window: HWND,
        view_ctx: &mut ViewContext,
        pane_group_docking_manager: &core::cell::RefCell<PaneGroupDockingManager>,
    ) {
        self.update_mouse_pos(x, y, view_ctx);

        if !self.is_mouse_dragging {
            if let Some(x) = self
                .mouse_current_enter_element
                .as_ref()
                .and_then(std::rc::Weak::upgrade)
            {
                x.borrow().eh.on_click(view_ctx);
            }
        } else {
            if let Some(x) = self
                .mouse_down_point
                .as_ref()
                .and_then(|x| x.2.as_ref())
                .and_then(std::rc::Weak::upgrade)
            {
                x.borrow()
                    .eh
                    .on_end_drag(window, view_ctx, pane_group_docking_manager);
            }
        }
        self.mouse_down_point = None;
    }
}

pub struct HitTestTree {
    eh: Box<dyn InputEventHandler>,
    id: usize,
    left: f32,
    top: f32,
    width: f32,
    height: f32,
    children: HashMap<usize, std::rc::Rc<core::cell::RefCell<HitTestTree>>>,
}
impl HitTestTree {
    #[inline]
    pub fn new(
        eh: Box<dyn InputEventHandler>,
        id: usize,
        left: f32,
        top: f32,
        width: f32,
        height: f32,
    ) -> Self {
        Self {
            eh,
            id,
            left,
            top,
            width,
            height,
            children: HashMap::new(),
        }
    }
    #[inline]
    pub fn new_unsized(eh: Box<dyn InputEventHandler>, id: usize, left: f32, top: f32) -> Self {
        Self::new(eh, id, left, top, f32::MAX, f32::MAX)
    }

    #[inline]
    pub fn add_child(&mut self, child: &std::rc::Rc<core::cell::RefCell<HitTestTree>>) {
        self.children.insert(child.borrow().id, child.clone());
    }

    #[inline]
    pub fn set_rect(&mut self, left: f32, top: f32, width: f32, height: f32) {
        self.left = left;
        self.top = top;
        self.width = width;
        self.height = height;
    }
    #[inline]
    pub fn set_size(&mut self, width: f32, height: f32) {
        self.width = width;
        self.height = height;
    }
    #[inline]
    pub fn set_offset(&mut self, left: f32, top: f32) {
        self.left = left;
        self.top = top;
    }

    pub fn check(
        this: &std::rc::Rc<core::cell::RefCell<Self>>,
        x: f32,
        y: f32,
    ) -> Option<std::rc::Rc<core::cell::RefCell<Self>>> {
        let this1 = this.borrow();
        if (this1.left..=(this1.left + this1.width)).contains(&x)
            && (this1.top..=(this1.top + this1.height)).contains(&y)
        {
            let child = this1
                .children
                .values()
                .find_map(|c| Self::check(c, x - this1.left, y - this1.top));
            Some(child.unwrap_or(this.clone()))
        } else {
            None
        }
    }
}
impl core::fmt::Debug for HitTestTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HitTestTree")
            .field("id", &self.id)
            .field("left", &self.left)
            .field("top", &self.top)
            .field("width", &self.width)
            .field("height", &self.height)
            .field("children", &self.children)
            .finish_non_exhaustive()
    }
}
pub struct HitTestTreeContext {
    current_id: usize,
}
impl HitTestTreeContext {
    pub fn new() -> Self {
        Self { current_id: 0 }
    }

    pub fn new_id(&mut self) -> usize {
        self.current_id += 1;
        self.current_id
    }
}

struct WindowState<'r> {
    input_state: InputState,
    view_context: ViewContext<'r>,
    pane_group_docking_manager: core::cell::RefCell<PaneGroupDockingManager>,
}

fn main() {
    let instance_handle = unsafe { GetModuleHandleA(None).expect("Failed to get instance handle") };
    let wndclass = WNDCLASSEXA {
        cbSize: core::mem::size_of::<WNDCLASSEXA>() as _,
        cbClsExtra: 0,
        cbWndExtra: core::mem::size_of::<usize>() as _,
        style: WNDCLASS_STYLES(0),
        lpfnWndProc: Some(window_proc),
        hInstance: instance_handle.into(),
        hIcon: unsafe {
            LoadIconA(None, core::mem::transmute::<_, PCSTR>(IDI_APPLICATION))
                .expect("Failed to load app icon")
        },
        hCursor: unsafe {
            LoadCursorA(None, core::mem::transmute::<_, PCSTR>(IDC_ARROW))
                .expect("Failed to load default cursor")
        },
        hbrBackground: HBRUSH(0),
        lpszMenuName: PCSTR::null(),
        lpszClassName: s!("io.ct2.peridot.marble.windows"),
        hIconSm: unsafe {
            LoadIconA(None, core::mem::transmute::<_, PCSTR>(IDI_APPLICATION))
                .expect("Failed to load app small icon")
        },
    };
    let window_handle = WindowBuilder::new(
        instance_handle.into(),
        register_window_class(&wndclass).expect("Failed to register window class"),
        s!("Peridot Marble Editor"),
    )
    .no_redirection_bitmap()
    .app_window()
    .overlapped_window()
    .create()
    .expect("Failed to create window");

    unsafe {
        let attr: BOOL = BOOL(1);
        DwmSetWindowAttribute(
            window_handle,
            DWMWINDOWATTRIBUTE(20),
            &attr as *const _ as _,
            core::mem::size_of::<BOOL>() as _,
        )
        .expect("Failed to set window attribute");
    }

    let _dispatcher_queue_controller = unsafe {
        CreateDispatcherQueueController(DispatcherQueueOptions {
            dwSize: core::mem::size_of::<DispatcherQueueOptions>() as _,
            threadType: DQTYPE_THREAD_CURRENT,
            apartmentType: DQTAT_COM_ASTA,
        })
        .expect("Failed to create dispatcher queue controller")
    };

    let mut d3d11_device: Option<ID3D11Device> = None;
    let mut feature_level: D3D_FEATURE_LEVEL = D3D_FEATURE_LEVEL(0);
    let mut d3d11_imm_context: Option<ID3D11DeviceContext> = None;
    unsafe {
        D3D11CreateDevice(
            None,
            D3D_DRIVER_TYPE_HARDWARE,
            None,
            D3D11_CREATE_DEVICE_BGRA_SUPPORT,
            None,
            D3D11_SDK_VERSION,
            Some(&mut d3d11_device),
            Some(&mut feature_level),
            Some(&mut d3d11_imm_context),
        )
        .expect("Failed to initialize D3D11");
    }
    let d3d11_device = d3d11_device.expect("No D3D11 device instance");
    let d3d11_imm_context = d3d11_imm_context.expect("No D3D11 device context instance");
    println!("D3D11 Feature Level: {feature_level:?}");

    let d2d1_factory: ID2D1Factory1 = {
        let options = D2D1_FACTORY_OPTIONS {
            debugLevel: D2D1_DEBUG_LEVEL_WARNING,
        };

        unsafe {
            D2D1CreateFactory(D2D1_FACTORY_TYPE_SINGLE_THREADED, Some(&options))
                .expect("Failed to create D2D1 Factory")
        }
    };
    let d2d1_device = unsafe {
        d2d1_factory
            .CreateDevice(
                &d3d11_device
                    .cast::<IDXGIDevice>()
                    .expect("No DXGI Device queried"),
            )
            .expect("Failed to create D2D1 Device")
    };

    let dwrite_factory: IDWriteFactory = unsafe {
        DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED)
            .expect("Failed to create DirectWrite factory")
    };
    let mut text_format_stock = TextFormatStock::new(&dwrite_factory);

    let compositor =
        windows::UI::Composition::Compositor::new().expect("Failed to create ui compositor");
    let desktop_interop = compositor
        .cast::<ICompositorDesktopInterop>()
        .expect("This compositor does not support desktop interop");
    let desktop_window_target = unsafe {
        desktop_interop
            .CreateDesktopWindowTarget(window_handle, false)
            .expect("Failed to create desktop window compositor target")
    };

    let compositor_interop = compositor
        .cast::<ICompositorInterop>()
        .expect("No CompositorInterop interface");
    let composition_graphics_device = unsafe {
        compositor_interop
            .CreateGraphicsDevice(&d2d1_device)
            .expect("Failed to create compositor graphics device")
    };
    let mut text_surface_stock =
        TextSurfaceStock::new(&dwrite_factory, &composition_graphics_device, unsafe {
            GetDpiForWindow(window_handle) as _
        });

    let app_global_scale = unsafe { GetDpiForWindow(window_handle) as f64 / 96.0 };
    let composition_root = compositor
        .CreateContainerVisual()
        .expect("Failed to create root visual");
    composition_root
        .SetScale(Vector3 {
            X: app_global_scale as _,
            Y: app_global_scale as _,
            Z: 1.0,
        })
        .expect("Failed to set global scale");
    composition_root
        .SetRelativeSizeAdjustment(Vector2::one())
        .expect("Failed to set size");
    composition_root
        .SetOffset(Vector3::zero())
        .expect("Failed to set offset");
    desktop_window_target
        .SetRoot(&composition_root)
        .expect("Failed to set root visual");

    let bg = compositor
        .CreateSpriteVisual()
        .expect("Failed to create bg");
    bg.SetBrush(
        &compositor
            .CreateColorBrushWithColor(Color {
                A: 255,
                R: 24,
                G: 24,
                B: 32,
            })
            .expect("Failed to create bg brush"),
    )
    .expect("Failed to set bg brush");
    bg.SetRelativeOffsetAdjustment(Vector3::zero())
        .expect("Failed to set bg offset");
    bg.SetRelativeSizeAdjustment(Vector2::one())
        .expect("Failed to set bg size");
    composition_root
        .Children()
        .expect("Failed to get children collection")
        .InsertAtBottom(&bg)
        .expect("Failed to insert bg");

    // let ui_font = text_format_stock
    //     .get("system-ui", 12.0, DWRITE_FONT_WEIGHT_NORMAL)
    //     .expect("Failed to create default ui format");
    // let title_text_surface = text_surface_stock
    //     .get(&ui_font, "New Project - Peridot Marble Editor v0.1.0")
    //     .expect("Failed to create title text surface");

    // let title_label = compositor
    //     .CreateSpriteVisual()
    //     .expect("Failed to create title label visual");
    // let title_label_brush = compositor
    //     .CreateSurfaceBrushWithSurface(&title_text_surface.surface)
    //     .expect("Failed to create surface brush");
    // title_label
    //     .SetBrush(&title_label_brush)
    //     .expect("Failed to set surface brush");
    // title_label
    //     .SetSize(Vector2 {
    //         X: title_text_surface.width as _,
    //         Y: title_text_surface.height as _,
    //     })
    //     .expect("Failed to set title label size");
    // title_label
    //     .SetOffset(Vector3 {
    //         X: 28.0,
    //         Y: 8.0,
    //         Z: 0.0,
    //     })
    //     .expect("Failed to set title label offset");
    // composition_root
    //     .Children()
    //     .expect("Failed to get children collection")
    //     .InsertAtTop(&title_label)
    //     .expect("Failed to insert title label visual");

    let overlay_layer = compositor
        .CreateContainerVisual()
        .expect("Failed to create overlay layer");
    {
        let children = composition_root
            .Children()
            .expect("Failed to get children collection");

        children
            .InsertAtTop(&overlay_layer)
            .expect("Failed to insert overlay layer");
    }

    let common_objects = UICommonObjects {
        tab_base_brush: compositor
            .CreateColorBrushWithColor(Color {
                R: 255,
                G: 255,
                B: 255,
                A: 32,
            })
            .expect("Failed to create base brush"),
        tab_active_overlay_brush: {
            let brush = compositor
                .CreateLinearGradientBrush()
                .expect("Failed to create active tab brush");
            brush
                .ColorStops()
                .expect("Failed to get color stops collection")
                .Append(
                    &compositor
                        .CreateColorGradientStopWithOffsetAndColor(0.0, TAB_ACTIVE_LIT_COLOR)
                        .expect("Failed to create gradient stop"),
                )
                .expect("Failed to append color stop");
            brush
                .ColorStops()
                .expect("Failed to get color stops collection")
                .Append(
                    &compositor
                        .CreateColorGradientStopWithOffsetAndColor(0.05, TAB_ACTIVE_BASE_COLOR)
                        .expect("Failed to create gradient stop"),
                )
                .expect("Failed to append color stop");
            brush
                .ColorStops()
                .expect("Failed to get color stops collection")
                .Append(
                    &compositor
                        .CreateColorGradientStopWithOffsetAndColor(
                            0.3,
                            Color {
                                A: 0,
                                ..TAB_ACTIVE_BASE_COLOR
                            },
                        )
                        .expect("Failed to create gradient stop"),
                )
                .expect("Failed to append color stop");
            brush
                .SetStartPoint(Vector2 { X: 0.5, Y: 0.0 })
                .expect("Failed to set gradient start point");
            brush
                .SetEndPoint(Vector2 { X: 0.5, Y: 0.5 })
                .expect("Failed to set gradient end point");

            brush
        },
        tab_title_font: text_format_stock
            .get("system-ui", 12.0, DWRITE_FONT_WEIGHT_NORMAL)
            .expect("Failed to create tab title format"),
        tab_active_title_font: text_format_stock
            .get("system-ui", 12.0, DWRITE_FONT_WEIGHT_SEMI_BOLD)
            .expect("Failed to create tab active title format"),
        tab_hover_animation: {
            let a = compositor
                .CreateScalarKeyFrameAnimation()
                .expect("Failed to create hover animation");
            a.InsertKeyFrame(0.0, 0.0)
                .expect("Failed to insert keyframe");
            a.InsertKeyFrameWithEasingFunction(
                1.0,
                1.0,
                &compositor
                    .CreateLinearEasingFunction()
                    .expect("Failed to create easing function"),
            )
            .expect("Failed to insert keyframe");
            a.SetDuration(TimeSpan {
                Duration: 50 * 10_000,
            })
            .expect("Failed to set duration");

            a
        },
        tab_hover_end_animation: {
            let a = compositor
                .CreateScalarKeyFrameAnimation()
                .expect("Failed to create hover animation");
            a.InsertKeyFrame(0.0, 1.0)
                .expect("Failed to insert keyframe");
            a.InsertKeyFrameWithEasingFunction(
                1.0,
                0.0,
                &compositor
                    .CreateLinearEasingFunction()
                    .expect("Failed to create easing function"),
            )
            .expect("Failed to insert keyframe");
            a.SetDuration(TimeSpan {
                Duration: 50 * 10_000,
            })
            .expect("Failed to set duration");

            a
        },
        tab_active_overlay_enter_animation: {
            let a = compositor
                .CreateScalarKeyFrameAnimation()
                .expect("Failed to create hover animation");
            a.InsertKeyFrame(0.0, 0.0)
                .expect("Failed to insert keyframe");
            a.InsertKeyFrameWithEasingFunction(
                1.0,
                1.0,
                &compositor
                    .CreateLinearEasingFunction()
                    .expect("Failed to create easing function"),
            )
            .expect("Failed to insert keyframe");
            a.SetDuration(TimeSpan {
                Duration: 50 * 10_000,
            })
            .expect("Failed to set duration");

            a
        },
        tab_active_overlay_leave_animation: {
            let a = compositor
                .CreateScalarKeyFrameAnimation()
                .expect("Failed to create hover animation");
            a.InsertKeyFrame(0.0, 1.0)
                .expect("Failed to insert keyframe");
            a.InsertKeyFrameWithEasingFunction(
                1.0,
                0.0,
                &compositor
                    .CreateLinearEasingFunction()
                    .expect("Failed to create easing function"),
            )
            .expect("Failed to insert keyframe");
            a.SetDuration(TimeSpan {
                Duration: 50 * 10_000,
            })
            .expect("Failed to set duration");

            a
        },
    };

    let hittest_tree_root = std::rc::Rc::new(core::cell::RefCell::new(HitTestTree::new_unsized(
        Box::new(()),
        0,
        0.0,
        0.0,
    )));
    let mut hittest_context = HitTestTreeContext::new();

    let mut view_context = ViewContext {
        compositor: &compositor,
        common: &common_objects,
        text_format_stock: &mut text_format_stock,
        text_surface_stock: &mut text_surface_stock,
        hittest_tree_parent: &hittest_tree_root,
        hittest_context: &mut hittest_context,
    };

    let mut pane_group_docking_manager = PaneGroupDockingManager::new(&mut view_context)
        .expect("Failed to initialize docking manager");

    let pane_group1 =
        PaneGroupView::new(&mut view_context).expect("Failed to create PaneGroupView");
    PaneGroupView::add_tab::<TimelineTabPresenter>(&pane_group1, &mut view_context)
        .expect("Failed to create SceneViewPaneTabHeader");
    pane_group1.borrow_mut().rearrange();

    let pane_group2 =
        PaneGroupView::new(&mut view_context).expect("Failed to create PaneGroupView");
    PaneGroupView::add_tab::<StageTabPresenter>(&pane_group2, &mut view_context)
        .expect("Failed to create StagePaneTab");
    PaneGroupView::add_tab::<PreviewTabPresenter>(&pane_group2, &mut view_context)
        .expect("Failed to create PreviewPaneTab");
    PaneGroupView::add_tab::<ProjectSettingsTabPresenter>(&pane_group2, &mut view_context)
        .expect("Failed to create ProjectSettingsPaneTabHeader");
    pane_group2.borrow_mut().rearrange();

    let pane_group3 =
        PaneGroupView::new(&mut view_context).expect("Failed to create PaneGroupView");
    PaneGroupView::add_tab::<InspectorTabPresenter>(&pane_group3, &mut view_context)
        .expect("Failed to create InspectorPaneTabHeader");
    pane_group3.borrow_mut().rearrange();

    pane_group_docking_manager
        .set_layout(PaneDockState::Right(
            pane_group3.clone(),
            256.0,
            Box::new(PaneDockState::Top(
                pane_group1.clone(),
                128.0,
                Box::new(PaneDockState::Fill(pane_group2.clone())),
            )),
        ))
        .expect("Failed to setup initial layout");

    composition_root
        .Children()
        .expect("Failed to get children collection")
        .InsertBelow(&pane_group_docking_manager.placement_visual, &overlay_layer)
        .expect("Failed to insert placement visual");

    let mut client_rect = core::mem::MaybeUninit::<windows::Win32::Foundation::RECT>::uninit();
    unsafe {
        GetClientRect(window_handle, client_rect.as_mut_ptr())
            .expect("Failed to get initial client rect")
    };
    let client_rect = unsafe { client_rect.assume_init() };
    let window_dpi = unsafe { GetDpiForWindow(window_handle) };
    let client_width = (client_rect.right - client_rect.left) as f32 * 96.0 / window_dpi as f32;
    let client_height = (client_rect.bottom - client_rect.top) as f32 * 96.0 / window_dpi as f32;
    pane_group_docking_manager
        .resize_root(client_width, client_height)
        .expect("Failed to initial relayout");

    let mut ws = WindowState {
        input_state: InputState::new(&hittest_tree_root),
        view_context,
        pane_group_docking_manager: core::cell::RefCell::new(pane_group_docking_manager),
    };
    unsafe {
        SetWindowLongPtrA(
            window_handle,
            WINDOW_LONG_PTR_INDEX(0),
            &mut ws as *mut _ as _,
        );

        let _ = ShowWindow(window_handle, SW_SHOWNORMAL);
    }

    let mut msg = core::mem::MaybeUninit::<MSG>::uninit();
    while unsafe { GetMessageA(msg.as_mut_ptr(), None, 0, 0).0 > 0 } {
        unsafe {
            let _ = TranslateMessage(msg.as_ptr());
            DispatchMessageA(msg.as_ptr());
        }
    }

    // drop d3d11 before d2d1
    drop(d3d11_imm_context);
    drop(d3d11_device);

    std::process::exit(unsafe { msg.assume_init().wParam.0 as _ });
}

extern "system" fn window_proc(hwnd: HWND, msg: u32, wp: WPARAM, lp: LPARAM) -> LRESULT {
    if msg == WM_DESTROY {
        unsafe { PostQuitMessage(0) };
        return LRESULT(0);
    }
    if msg == WM_MOUSEMOVE {
        let Some(state) = (unsafe {
            (GetWindowLongPtrA(hwnd, WINDOW_LONG_PTR_INDEX(0)) as *mut WindowState).as_mut()
        }) else {
            return LRESULT(0);
        };

        let dpi = unsafe { GetDpiForWindow(hwnd) as f32 };
        let (x, y) = ((lp.0 & 0xffff) as i16, ((lp.0 >> 16) & 0xffff) as i16);
        state.input_state.on_mouse_move(
            x as f32 * 96.0 / dpi,
            y as f32 * 96.0 / dpi,
            hwnd,
            &mut state.view_context,
            &state.pane_group_docking_manager,
        );

        return LRESULT(0);
    }
    if msg == WM_LBUTTONDOWN {
        let Some(state) = (unsafe {
            (GetWindowLongPtrA(hwnd, WINDOW_LONG_PTR_INDEX(0)) as *mut WindowState).as_mut()
        }) else {
            return LRESULT(0);
        };

        let dpi = unsafe { GetDpiForWindow(hwnd) as f32 };
        let (x, y) = ((lp.0 & 0xffff) as i16, ((lp.0 >> 16) & 0xffff) as i16);
        state.input_state.on_mouse_down(
            x as f32 * 96.0 / dpi,
            y as f32 * 96.0 / dpi,
            &mut state.view_context,
        );

        return LRESULT(0);
    }
    if msg == WM_LBUTTONUP {
        let Some(state) = (unsafe {
            (GetWindowLongPtrA(hwnd, WINDOW_LONG_PTR_INDEX(0)) as *mut WindowState).as_mut()
        }) else {
            return LRESULT(0);
        };

        let dpi = unsafe { GetDpiForWindow(hwnd) as f32 };
        let (x, y) = ((lp.0 & 0xffff) as i16, ((lp.0 >> 16) & 0xffff) as i16);
        state.input_state.on_mouse_up(
            x as f32 * 96.0 / dpi,
            y as f32 * 96.0 / dpi,
            hwnd,
            &mut state.view_context,
            &state.pane_group_docking_manager,
        );

        return LRESULT(0);
    }
    if msg == WM_SIZE {
        let Some(state) = (unsafe {
            (GetWindowLongPtrA(hwnd, WINDOW_LONG_PTR_INDEX(0)) as *mut WindowState).as_mut()
        }) else {
            // not initialized
            return LRESULT(0);
        };

        let dpi = unsafe { GetDpiForWindow(hwnd) as f32 };
        let (w, h) = ((lp.0 & 0xffff) as i16, ((lp.0 >> 16) & 0xffff) as i16);
        let (w, h) = (
            (w as f32 * 96.0 / dpi).max(64.0),
            (h as f32 * 96.0 / dpi).max(64.0),
        );
        state
            .pane_group_docking_manager
            .borrow_mut()
            .resize_root(w, h)
            .expect("Failed to resize root");

        return LRESULT(0);
    }

    unsafe { DefWindowProcA(hwnd, msg, wp, lp) }
}
