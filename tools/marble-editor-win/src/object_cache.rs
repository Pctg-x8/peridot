use std::{borrow::Cow, collections::HashMap};

use windows::{
    core::{w, Interface},
    Foundation::{Numerics::Vector2, Size},
    Graphics::DirectX::{DirectXAlphaMode, DirectXPixelFormat},
    Win32::{
        Foundation::POINT,
        Graphics::{
            Direct2D::{
                Common::{D2D1_COLOR_F, D2D_POINT_2F},
                ID2D1DeviceContext, D2D1_DRAW_TEXT_OPTIONS_NONE,
            },
            DirectWrite::{
                IDWriteFactory, IDWriteTextFormat, DWRITE_FONT_STRETCH_NORMAL,
                DWRITE_FONT_STYLE_NORMAL, DWRITE_FONT_WEIGHT, DWRITE_TEXT_METRICS,
            },
        },
        System::WinRT::Composition::ICompositionDrawingSurfaceInterop,
    },
    UI::Composition::{CompositionDrawingSurface, CompositionGraphicsDevice},
};
use windows_core::PCWSTR;

use crate::utils::SafeF32;

#[derive(PartialEq, Eq)]
struct TextFormatStockKey {
    family_name: Cow<'static, str>,
    size: SafeF32,
    weight: DWRITE_FONT_WEIGHT,
}
impl core::hash::Hash for TextFormatStockKey {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (&self.family_name, self.size, self.weight.0).hash(state)
    }
}

pub struct TextFormatStock {
    factory: IDWriteFactory,
    formats: HashMap<TextFormatStockKey, IDWriteTextFormat>,
}
impl TextFormatStock {
    pub fn new(factory: &IDWriteFactory) -> Self {
        Self {
            factory: factory.clone(),
            formats: HashMap::new(),
        }
    }

    pub fn get(
        &mut self,
        family_name: impl Into<Cow<'static, str>>,
        size: impl Into<SafeF32>,
        weight: DWRITE_FONT_WEIGHT,
    ) -> windows::core::Result<IDWriteTextFormat> {
        let key = TextFormatStockKey {
            family_name: family_name.into(),
            size: size.into(),
            weight,
        };

        match self.formats.entry(key) {
            std::collections::hash_map::Entry::Occupied(e) => Ok(e.get().clone()),
            std::collections::hash_map::Entry::Vacant(e) => {
                let family_name_widechars = e
                    .key()
                    .family_name
                    .encode_utf16()
                    .chain(core::iter::once(0))
                    .collect::<Vec<_>>();
                let format = unsafe {
                    self.factory.CreateTextFormat(
                        PCWSTR(family_name_widechars.as_ptr()),
                        None,
                        weight,
                        DWRITE_FONT_STYLE_NORMAL,
                        DWRITE_FONT_STRETCH_NORMAL,
                        e.key().size.value(),
                        w!("ja-JP"),
                    )?
                };

                Ok(e.insert(format).clone())
            }
        }
    }
}

#[derive(Clone)]
pub struct TextSurface {
    pub surface: CompositionDrawingSurface,
    pub width: f32,
    pub height: f32,
}
impl TextSurface {
    #[inline]
    pub const fn visual_size(&self) -> Vector2 {
        Vector2 {
            X: self.width,
            Y: self.height,
        }
    }
}

pub struct TextSurfaceStock {
    dwrite_factory: IDWriteFactory,
    composition_graphics_device: CompositionGraphicsDevice,
    target_window_dpi: f32,
    surfaces: HashMap<(*mut core::ffi::c_void, Cow<'static, str>), TextSurface>,
}
impl TextSurfaceStock {
    pub fn new(
        dwrite_factory: &IDWriteFactory,
        composition_graphics_device: &CompositionGraphicsDevice,
        current_window_dpi: f32,
    ) -> Self {
        Self {
            dwrite_factory: dwrite_factory.clone(),
            composition_graphics_device: composition_graphics_device.clone(),
            target_window_dpi: current_window_dpi,
            surfaces: HashMap::new(),
        }
    }

    pub fn get(
        &mut self,
        fmt: &IDWriteTextFormat,
        text: impl Into<Cow<'static, str>>,
    ) -> windows::core::Result<TextSurface> {
        match self.surfaces.entry((fmt.as_raw(), text.into())) {
            std::collections::hash_map::Entry::Occupied(e) => Ok(e.get().clone()),
            std::collections::hash_map::Entry::Vacant(e) => {
                let text_layout = unsafe {
                    self.dwrite_factory.CreateTextLayout(
                        &e.key().1.encode_utf16().collect::<Vec<_>>(),
                        fmt,
                        core::f32::MAX,
                        core::f32::MAX,
                    )?
                };
                let mut text_metrics = core::mem::MaybeUninit::<DWRITE_TEXT_METRICS>::uninit();
                unsafe { text_layout.GetMetrics(text_metrics.as_mut_ptr())? };
                let text_metrics = unsafe { text_metrics.assume_init() };
                let size = Size {
                    Width: text_metrics.width * self.target_window_dpi / 96.0,
                    Height: text_metrics.height * self.target_window_dpi / 96.0,
                };
                let surface = self.composition_graphics_device.CreateDrawingSurface(
                    size,
                    DirectXPixelFormat::B8G8R8A8UIntNormalized,
                    DirectXAlphaMode::Premultiplied,
                )?;

                let surface_interop = surface.cast::<ICompositionDrawingSurfaceInterop>()?;
                let mut offset = core::mem::MaybeUninit::<POINT>::uninit();
                let dc: ID2D1DeviceContext =
                    unsafe { surface_interop.BeginDraw(None, offset.as_mut_ptr())? };
                let offset = unsafe { offset.assume_init() };
                let res = 'drawing_block: {
                    unsafe {
                        dc.SetDpi(self.target_window_dpi, self.target_window_dpi);

                        let clear_color = D2D1_COLOR_F {
                            a: 0.0,
                            r: 0.0,
                            g: 0.0,
                            b: 0.0,
                        };
                        let text_color = D2D1_COLOR_F {
                            a: 1.0,
                            r: 1.0,
                            g: 1.0,
                            b: 1.0,
                        };

                        let brush = match dc.CreateSolidColorBrush(&text_color, None) {
                            Ok(b) => b,
                            Err(e) => break 'drawing_block Err(e),
                        };

                        dc.Clear(Some(&clear_color));
                        dc.DrawTextLayout(
                            D2D_POINT_2F {
                                x: offset.x as f32 * 96.0 / self.target_window_dpi,
                                y: offset.y as f32 * 96.0 / self.target_window_dpi,
                            },
                            &text_layout,
                            &brush,
                            D2D1_DRAW_TEXT_OPTIONS_NONE,
                        );

                        Ok(())
                    }
                };
                unsafe { surface_interop.EndDraw()? };
                res?;

                Ok(e.insert(TextSurface {
                    surface,
                    width: text_metrics.width,
                    height: text_metrics.height,
                })
                .clone())
            }
        }
    }
}
