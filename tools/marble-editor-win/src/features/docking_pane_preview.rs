use std::sync::{Arc, RwLock};

use windows::{
    core::{h, s, Interface},
    Foundation::{
        Numerics::{Vector2, Vector3},
        Rect, TimeSpan, TypedEventHandler,
    },
    System::DispatcherQueueTimer,
    Win32::{
        Foundation::{HWND, LPARAM, LRESULT, WPARAM},
        System::{LibraryLoader::GetModuleHandleA, WinRT::Composition::ICompositorDesktopInterop},
        UI::WindowsAndMessaging::{
            DefWindowProcA, SetWindowPos, ShowWindow, SWP_NOACTIVATE, SWP_NOZORDER, SW_HIDE,
            SW_SHOWNA, WNDCLASSEXA,
        },
    },
    UI::{
        Color,
        Composition::{
            CompositionAnimationGroup, CompositionEasingFunction, CompositionEasingFunctionMode,
            CompositionEffectSourceParameter, Desktop::DesktopWindowTarget,
            ScalarKeyFrameAnimation, SpriteVisual,
        },
    },
};

use crate::{
    app_subsystem_instances::AppSubsystemInstances,
    bindgen::Graphics::Canvas::Effects::{EffectOptimization, GaussianBlurEffect},
    utils::RectExtensions,
    winapi_extras::{
        register_window_class, timespan_ms, KeyFrameAnimationExtension,
        KeyFrameAnimationPropertySetterExtension, Vector2Extension, Vector3Extension,
        VectorScalarConstructor, VisualExtensions, WindowBuilder,
    },
};

pub struct DockingPanePreview {
    window: HWND,
    _composition_target: DesktopWindowTarget,
    root: SpriteVisual,
    color_tint: SpriteVisual,
    blink_animation: ScalarKeyFrameAnimation,
    show_animation: CompositionAnimationGroup,
    hide_animation: CompositionAnimationGroup,
    hide_delay_timer: Arc<RwLock<Option<DispatcherQueueTimer>>>,
}
impl DockingPanePreview {
    const INOUT_DURATION: TimeSpan = timespan_ms(100);
    const TINT_COLOR: Color = Color {
        A: 16,
        R: 16,
        G: 176,
        B: 255,
    };

    extern "system" fn window_callback(h: HWND, m: u32, w: WPARAM, l: LPARAM) -> LRESULT {
        unsafe { DefWindowProcA(h, m, w, l) }
    }
    fn register_window_class() -> windows::core::Result<u16> {
        register_window_class(&WNDCLASSEXA {
            cbSize: core::mem::size_of::<WNDCLASSEXA>() as _,
            lpfnWndProc: Some(Self::window_callback),
            hInstance: unsafe { GetModuleHandleA(None)?.into() },
            lpszClassName: s!("io.ct2.peridot.marble.windows.overlays.floating_preview"),
            ..unsafe { core::mem::MaybeUninit::zeroed().assume_init() }
        })
    }

    pub fn new() -> windows::core::Result<Self> {
        let window = WindowBuilder::new(
            unsafe { GetModuleHandleA(None)?.into() },
            Self::register_window_class()?,
            s!(""),
        )
        .no_activate()
        .no_redirection_bitmap()
        .transparent()
        .topmost()
        .popup()
        .create()?;
        let composition_target = unsafe {
            AppSubsystemInstances::get()
                .compositor
                .cast::<ICompositorDesktopInterop>()?
                .CreateDesktopWindowTarget(window, true)?
        };

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
        blur_visual
            .set_properties()
            .center_point(Vector3::scalar(0.5))?
            .anchor_point(Vector2::scalar(0.5))?
            .relative_offset_adjustment(Vector2::scalar(0.5).with_z(0.0))?
            .brush(&blur_brush)?;

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
            .relative_offset_adjustment(Vector3::zero())?
            .relative_size_adjustment(Vector2::one())?;
        blur_visual.Children()?.InsertAtTop(&color_tint)?;

        blur_visual.SetShadow(&{
            let x = AppSubsystemInstances::get().compositor.CreateDropShadow()?;
            x.SetBlurRadius(32.0)?;
            x.SetOffset(Vector3::down(16.0))?;
            x.SetOpacity(0.3)?;
            x
        })?;

        let linear_easing = AppSubsystemInstances::get()
            .compositor
            .CreateLinearEasingFunction()?;

        let blink_animation = AppSubsystemInstances::get()
            .compositor
            .CreateScalarKeyFrameAnimation()?;
        blink_animation
            .iterate_forever()?
            .keyframe(0.0, 1.0)?
            .interpolate(0.5, 0.75, &linear_easing)?
            .interpolate(1.0, 1.0, &linear_easing)?
            .set_properties()
            .duration(timespan_ms(2600))?;

        let show_animation = AppSubsystemInstances::get()
            .compositor
            .CreateAnimationGroup()?;
        show_animation.Add(&{
            let a = AppSubsystemInstances::get()
                .compositor
                .CreateScalarKeyFrameAnimation()?;
            a.keyframe(0.0, 0.0)?
                .interpolate(1.0, 1.0, &linear_easing)?
                .set_properties()
                .duration(Self::INOUT_DURATION)?
                .target(h!("Opacity"))?;

            a
        })?;
        show_animation.Add(&{
            let a = AppSubsystemInstances::get()
                .compositor
                .CreateVector3KeyFrameAnimation()?;
            a.keyframe(0.0, Vector2::scalar(1.2).with_z(1.0))?
                .interpolate(
                    1.0,
                    Vector3::one(),
                    &CompositionEasingFunction::CreatePowerEasingFunction(
                        &AppSubsystemInstances::get().compositor,
                        CompositionEasingFunctionMode::Out,
                        2.0,
                    )?,
                )?
                .set_properties()
                .duration(Self::INOUT_DURATION)?
                .target(h!("Scale"))?;

            a
        })?;
        let hide_animation = AppSubsystemInstances::get()
            .compositor
            .CreateAnimationGroup()?;
        hide_animation.Add(&{
            let a = AppSubsystemInstances::get()
                .compositor
                .CreateScalarKeyFrameAnimation()?;
            a.keyframe(0.0, 1.0)?
                .interpolate(1.0, 0.0, &linear_easing)?
                .set_properties()
                .duration(Self::INOUT_DURATION)?
                .target(h!("Opacity"))?;

            a
        })?;
        hide_animation.Add(&{
            let a = AppSubsystemInstances::get()
                .compositor
                .CreateVector3KeyFrameAnimation()?;
            a.keyframe(0.0, Vector3::one())?
                .interpolate(1.0, Vector2::scalar(0.9).with_z(1.0), &linear_easing)?
                .set_properties()
                .duration(Self::INOUT_DURATION)?
                .target(h!("Scale"))?;

            a
        })?;

        composition_target.SetRoot(&blur_visual)?;

        Ok(Self {
            window,
            _composition_target: composition_target,
            root: blur_visual,
            color_tint,
            blink_animation,
            show_animation,
            hide_animation,
            hide_delay_timer: Arc::new(RwLock::new(None)),
        })
    }

    pub fn show(&self) -> windows::core::Result<()> {
        *self.hide_delay_timer.write().unwrap() = None;

        unsafe {
            let _ = ShowWindow(self.window, SW_SHOWNA);
        }
        self.color_tint
            .StartAnimation(h!("Opacity"), &self.blink_animation)?;
        self.root.StartAnimationGroup(&self.show_animation)?;

        Ok(())
    }

    pub fn hide(&self) -> windows::core::Result<()> {
        self.root.StartAnimationGroup(&self.hide_animation)?;
        let delay_hide = self.root.DispatcherQueue()?.CreateTimer()?;
        delay_hide.SetInterval(Self::INOUT_DURATION)?;
        let tint = self.color_tint.clone();
        let delay_timer = self.hide_delay_timer.clone();
        let w = self.window;
        delay_hide.Tick(&TypedEventHandler::new(move |_, _| {
            tint.StopAnimation(h!("Opacity"))?;
            unsafe {
                let _ = ShowWindow(w, SW_HIDE);
            }
            *delay_timer.write().unwrap() = None;

            Ok(())
        }))?;
        *self.hide_delay_timer.write().unwrap() = Some(delay_hide);

        Ok(())
    }

    pub fn set_rect(&self, rect: Rect) -> windows::core::Result<()> {
        self.root.SetSize(rect.size())?;

        // 影の分を考慮してウィンドウサイズを計算する
        unsafe {
            SetWindowPos(
                self.window,
                None,
                rect.X as i32 - 32,
                rect.Y as i32 - 32,
                rect.Width as i32 + 64,
                rect.Height as i32 + 64,
                SWP_NOZORDER | SWP_NOACTIVATE,
            )?;
        }

        Ok(())
    }
}
