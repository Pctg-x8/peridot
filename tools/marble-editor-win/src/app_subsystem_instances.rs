use core::ffi::c_void;
use windows::{
    core::Interface,
    Foundation::{Numerics::Vector2, Size},
    Graphics::DirectX::{DirectXAlphaMode, DirectXPixelFormat},
    Win32::{
        Foundation::POINT,
        Graphics::{
            CompositionSwapchain::{
                CreatePresentationFactory, IPresentationFactory, IPresentationManager,
            },
            Direct2D::{
                Common::{D2D1_COLOR_F, D2D_RECT_F},
                D2D1CreateFactory, ID2D1DeviceContext, ID2D1Factory1, D2D1_DEBUG_LEVEL_WARNING,
                D2D1_FACTORY_OPTIONS, D2D1_FACTORY_TYPE_SINGLE_THREADED, D2D1_ROUNDED_RECT,
            },
            Direct3D::{D3D_DRIVER_TYPE_HARDWARE, D3D_FEATURE_LEVEL},
            Direct3D11::{
                D3D11CreateDevice, ID3D11Device, D3D11_CREATE_DEVICE_BGRA_SUPPORT,
                D3D11_SDK_VERSION,
            },
            DirectWrite::{
                DWriteCreateFactory, IDWriteFactory, DWRITE_FACTORY_TYPE_SHARED,
                DWRITE_FONT_WEIGHT_NORMAL, DWRITE_FONT_WEIGHT_SEMI_BOLD,
            },
            Dxgi::IDXGIDevice,
        },
        System::WinRT::Composition::{ICompositionDrawingSurfaceInterop, ICompositorInterop},
    },
    UI::{
        Color,
        Composition::{CompositionGraphicsDevice, CompositionStretch, Compositor},
    },
};

use crate::{
    miniengine::MiniEngine,
    object_cache::{TextFormatStock, TextSurfaceStock},
    uikit::UICommonObjects,
    winapi_extras::{
        timespan_ms, KeyFrameAnimationExtension, KeyFrameAnimationPropertySetterExtension,
    },
    FloatSliderView, TAB_ACTIVE_BASE_COLOR, TAB_ACTIVE_LIT_COLOR,
};

pub struct AppSubsystemInstances {
    pub d3d11_device: ID3D11Device,
    pub d2d1_factory: ID2D1Factory1,
    pub dwrite_factory: IDWriteFactory,
    pub compositor: Compositor,
    pub composition_graphics_device: CompositionGraphicsDevice,
    pub compositor_interop: ICompositorInterop,
    pub ui_common_objects: UICommonObjects,
    pub presentation_manager: IPresentationManager,
    pub mini_engine: MiniEngine,
    pub text_format_stock: TextFormatStock,
    pub text_surface_stock: TextSurfaceStock,
}
impl AppSubsystemInstances {
    pub fn new() -> Self {
        let mut d3d11_device = None;
        let mut feature_level = D3D_FEATURE_LEVEL(0);
        let mut d3d11_imm_context = None;
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
        let _d3d11_imm_context = d3d11_imm_context.expect("No D3D11 device context instance");
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

        let miniengine = MiniEngine::new().expect("Failed to initialize mini engine");

        let compositor = Compositor::new().expect("Failed to create ui compositor");
        let compositor_interop = compositor
            .cast::<ICompositorInterop>()
            .expect("No CompositorInterop interface");
        let composition_graphics_device = unsafe {
            compositor_interop
                .CreateGraphicsDevice(&d2d1_device)
                .expect("Failed to create compositor graphics device")
        };
        let text_surface_stock =
            TextSurfaceStock::new(&dwrite_factory, &composition_graphics_device);

        let mut presentation_factory = core::mem::MaybeUninit::<*mut c_void>::uninit();
        unsafe {
            CreatePresentationFactory(
                &d3d11_device,
                &IPresentationFactory::IID,
                presentation_factory.as_mut_ptr(),
            )
            .expect("Failed to create presentation factory")
        };
        let presentation_factory =
            unsafe { IPresentationFactory::from_raw(presentation_factory.assume_init()) };
        if unsafe { presentation_factory.IsPresentationSupportedWithIndependentFlip() == 0 } {
            panic!("Independent Presentation is not supported on this machine");
        }

        let presentation_manager = unsafe {
            presentation_factory
                .CreatePresentationManager()
                .expect("Failed to create presentation manager")
        };

        let linear_easing_fn = compositor
            .CreateLinearEasingFunction()
            .expect("Failed to create easing function");
        let ui_common_objects = UICommonObjects {
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
                a.keyframe(0.0, 0.0)
                    .expect("Failed to insert keyframe")
                    .interpolate(1.0, 1.0, &linear_easing_fn)
                    .expect("Failed to insert keyframe")
                    .set_properties()
                    .duration(timespan_ms(50))
                    .expect("Failed to set duration");

                a
            },
            tab_hover_end_animation: {
                let a = compositor
                    .CreateScalarKeyFrameAnimation()
                    .expect("Failed to create hover animation");
                a.keyframe(0.0, 1.0)
                    .expect("Failed to insert keyframe")
                    .interpolate(1.0, 0.0, &linear_easing_fn)
                    .expect("Failed to insert keyframe")
                    .set_properties()
                    .duration(timespan_ms(50))
                    .expect("Failed to set duration");

                a
            },
            tab_active_overlay_enter_animation: {
                let a = compositor
                    .CreateScalarKeyFrameAnimation()
                    .expect("Failed to create hover animation");
                a.keyframe(0.0, 0.0)
                    .expect("Failed to insert keyframe")
                    .interpolate(1.0, 1.0, &linear_easing_fn)
                    .expect("Failed to insert keyframe")
                    .set_properties()
                    .duration(timespan_ms(50))
                    .expect("Failed to set duration");

                a
            },
            tab_active_overlay_leave_animation: {
                let a = compositor
                    .CreateScalarKeyFrameAnimation()
                    .expect("Failed to create hover animation");
                a.keyframe(0.0, 1.0)
                    .expect("Failed to insert keyframe")
                    .interpolate(1.0, 0.0, &linear_easing_fn)
                    .expect("Failed to insert keyframe")
                    .set_properties()
                    .duration(timespan_ms(50))
                    .expect("Failed to set duration");

                a
            },
            slider_base_brush: {
                let base_surface = composition_graphics_device
                    .CreateDrawingSurface(
                        Size {
                            Width: FloatSliderView::BORDER_RECT_ROUNDING * 2.0 + 1.0 + 2.0,
                            Height: FloatSliderView::BORDER_RECT_ROUNDING * 2.0 + 1.0 + 2.0,
                        },
                        DirectXPixelFormat::R8G8B8A8UIntNormalized,
                        DirectXAlphaMode::Premultiplied,
                    )
                    .expect("Failed to create slider base surface");
                let surface_interop = base_surface
                    .cast::<ICompositionDrawingSurfaceInterop>()
                    .expect("no ICompositionDrawingSurfaceInterop queried");
                let mut update_offset = POINT { x: 0, y: 0 };
                let dc: ID2D1DeviceContext = unsafe {
                    surface_interop
                        .BeginDraw(None, &mut update_offset)
                        .expect("Failed to begin render slider base surface")
                };
                let res = 'rendering_block: {
                    unsafe {
                        const CLEAR_COLOR: D2D1_COLOR_F = D2D1_COLOR_F {
                            a: 0.0,
                            r: 0.0,
                            g: 0.0,
                            b: 0.0,
                        };
                        const BORDER_COLOR: D2D1_COLOR_F = D2D1_COLOR_F {
                            a: 1.0,
                            r: 0.8,
                            g: 0.8,
                            b: 0.8,
                        };
                        const INNER_COLOR: D2D1_COLOR_F = D2D1_COLOR_F {
                            a: 0.3,
                            r: 0.0,
                            g: 0.0,
                            b: 0.0,
                        };
                        let rounded_rect = D2D1_ROUNDED_RECT {
                            rect: D2D_RECT_F {
                                left: update_offset.x as f32 + 0.5,
                                top: update_offset.y as f32 + 0.5,
                                right: update_offset.x as f32
                                    + FloatSliderView::BORDER_RECT_ROUNDING * 2.0
                                    + 1.0
                                    + 2.0
                                    - 0.5,
                                bottom: update_offset.y as f32
                                    + FloatSliderView::BORDER_RECT_ROUNDING * 2.0
                                    + 1.0
                                    + 2.0
                                    - 0.5,
                            },
                            radiusX: FloatSliderView::BORDER_RECT_ROUNDING,
                            radiusY: FloatSliderView::BORDER_RECT_ROUNDING,
                        };

                        let border_brush = match dc.CreateSolidColorBrush(&BORDER_COLOR, None) {
                            Ok(b) => b,
                            Err(e) => break 'rendering_block Err(e),
                        };
                        let inner_brush = match dc.CreateSolidColorBrush(&INNER_COLOR, None) {
                            Ok(b) => b,
                            Err(e) => break 'rendering_block Err(e),
                        };

                        dc.Clear(Some(&CLEAR_COLOR));
                        dc.FillRoundedRectangle(&rounded_rect, &inner_brush);
                        dc.DrawRoundedRectangle(&rounded_rect, &border_brush, 1.0, None);
                    }

                    Ok(())
                };
                unsafe {
                    surface_interop
                        .EndDraw()
                        .expect("Failed to finish rendering")
                };
                res.expect("Error in rendering");

                let base_brush = compositor
                    .CreateSurfaceBrushWithSurface(&base_surface)
                    .expect("Failed to create base composition brush");
                let brush = compositor
                    .CreateNineGridBrush()
                    .expect("Failed to create slider base brush");
                brush
                    .SetSource(&base_brush)
                    .expect("Failed to set slider brush base");
                brush
                    .SetInsets(FloatSliderView::BORDER_RECT_ROUNDING + 1.0)
                    .expect("Failed to set slider brush insets");
                base_brush
                    .SetStretch(CompositionStretch::Fill)
                    .expect("Failed to base brush stretching mode");

                brush
            },
        };

        Self {
            d3d11_device,
            d2d1_factory,
            dwrite_factory,
            compositor,
            composition_graphics_device,
            compositor_interop,
            ui_common_objects,
            presentation_manager,
            mini_engine: miniengine,
            text_format_stock,
            text_surface_stock,
        }
    }
}
