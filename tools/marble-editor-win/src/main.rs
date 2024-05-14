use std::{
    borrow::Cow,
    cell::RefCell,
    collections::HashMap,
    rc::{Rc, Weak},
};

use features::{DockingPanePreview, PaneSplitterView, SplitDirection};
use object_cache::{TextFormatStock, TextSurfaceStock};
use uikit::{CursorStyle, InputContext, InputEventHandler, ViewContext};
use utils::{rect_slice_bottom, rect_slice_left, rect_slice_right, rect_slice_top, RectExtensions};
use winapi_extras::{
    timespan_ms, KeyFrameAnimationExtension, KeyFrameAnimationPropertySetterExtension,
    VisualExtensions,
};
use windows::{
    core::*,
    Foundation::{
        Numerics::{Vector2, Vector3},
        Rect,
    },
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
                LoadCursorA, LoadIconA, PostQuitMessage, SetCursor, SetWindowLongPtrA, ShowWindow,
                TranslateMessage, HTCLIENT, IDC_ARROW, IDC_SIZENS, IDC_SIZEWE, IDI_APPLICATION,
                MSG, SW_SHOWNORMAL, WINDOW_LONG_PTR_INDEX, WM_DESTROY, WM_LBUTTONDOWN,
                WM_LBUTTONUP, WM_MOUSEMOVE, WM_SETCURSOR, WM_WINDOWPOSCHANGED, WNDCLASSEXA,
                WNDCLASS_STYLES,
            },
        },
    },
    UI::{
        Color,
        Composition::{
            CompositionRoundedRectangleGeometry, CompositionSurfaceBrush, Compositor,
            ContainerVisual, LayerVisual, ScalarKeyFrameAnimation, ShapeVisual, SpriteVisual,
            VisualCollection,
        },
    },
};

use crate::{
    uikit::{UICommonObjects, ViewContext1},
    winapi_extras::{register_window_class, VectorScalarConstructor, WindowBuilder},
};

mod bindgen;
mod features;
mod object_cache;
mod uikit;
mod utils;
mod winapi_extras;

// Note: Rcは特別なtraitがついてるようで、newtypeで包むことはできない（やるとSharedMut<T>をSharedMut<dyn Trait>にできなくなる）
type SharedMut<T> = Rc<RefCell<T>>;
type WeakMut<T> = Weak<RefCell<T>>;
#[inline]
fn new_shared_mut<T>(value: T) -> SharedMut<T> {
    Rc::new(RefCell::new(value))
}
#[inline]
fn new_cyclic_shared_mut<T>(ctor: impl FnOnce(&WeakMut<T>) -> T) -> SharedMut<T> {
    Rc::new_cyclic(|w| RefCell::new(ctor(w)))
}
#[inline]
const fn empty_weak_mut<T>() -> WeakMut<T> {
    Weak::new()
}

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

const PANE_SPLITTER_GAP: f32 = 5.0;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum RedockingMode {
    Pane,
    Tab,
}

pub enum PaneDockingRecommendation {
    Left(SharedMut<PaneDockLayer>),
    Right(SharedMut<PaneDockLayer>),
    Top(SharedMut<PaneDockLayer>),
    Bottom(SharedMut<PaneDockLayer>),
    MergeGroup(SharedMut<TabGroupPaneView>),
    Free,
}
impl core::fmt::Debug for PaneDockingRecommendation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Left(_) => f.write_str("Left"),
            Self::Right(_) => f.write_str("Right"),
            Self::Top(_) => f.write_str("Top"),
            Self::Bottom(_) => f.write_str("Bottom"),
            Self::MergeGroup(_) => f.write_str("MergeGroup"),
            Self::Free => f.write_str("Free"),
        }
    }
}
impl PaneDockingRecommendation {
    pub fn dock_rect(&self, preview_rect: &Rect) -> Option<Rect> {
        match self {
            Self::Left(d) => Some(Rect {
                X: d.borrow().controlling_rect_left(),
                Y: d.borrow().controlling_rect_top(),
                Width: preview_rect
                    .Width
                    .min(d.borrow().controlling_rect_width() * 0.9),
                Height: d.borrow().controlling_rect_height(),
            }),
            Self::Right(d) => Some({
                let w = preview_rect
                    .Width
                    .min(d.borrow().controlling_rect_width() * 0.9);

                Rect {
                    X: d.borrow().controlling_rect_right() - w,
                    Y: d.borrow().controlling_rect_top(),
                    Width: w,
                    Height: d.borrow().controlling_rect_height(),
                }
            }),
            Self::Top(d) => Some(Rect {
                X: d.borrow().controlling_rect_left(),
                Y: d.borrow().controlling_rect_top(),
                Width: d.borrow().controlling_rect_width(),
                Height: preview_rect
                    .Height
                    .min(d.borrow().controlling_rect_height() * 0.9),
            }),
            Self::Bottom(d) => Some({
                let h = preview_rect
                    .Height
                    .min(d.borrow().controlling_rect_height() * 0.9);

                Rect {
                    X: d.borrow().controlling_rect_left(),
                    Y: d.borrow().controlling_rect_bottom() - h,
                    Width: d.borrow().controlling_rect_width(),
                    Height: h,
                }
            }),
            Self::MergeGroup(view) => Some(view.borrow().view_rect.clone()),
            Self::Free => None,
        }
    }
}

pub enum PaneDockLayer {
    EmptyRoot(Option<SharedMut<PaneDockLayer>>, Rect),
    Left {
        docked: SharedMut<PaneDockLayer>,
        splitter: SharedMut<PaneSplitterView>,
        container_region: Rect,
        rest: SharedMut<PaneDockLayer>,
        parent: WeakMut<PaneDockLayer>,
    },
    Right {
        docked: SharedMut<PaneDockLayer>,
        splitter: SharedMut<PaneSplitterView>,
        container_region: Rect,
        rest: SharedMut<PaneDockLayer>,
        parent: WeakMut<PaneDockLayer>,
    },
    Top {
        docked: SharedMut<PaneDockLayer>,
        splitter: SharedMut<PaneSplitterView>,
        container_region: Rect,
        rest: SharedMut<PaneDockLayer>,
        parent: WeakMut<PaneDockLayer>,
    },
    Bottom {
        docked: SharedMut<PaneDockLayer>,
        splitter: SharedMut<PaneSplitterView>,
        container_region: Rect,
        rest: SharedMut<PaneDockLayer>,
        parent: WeakMut<PaneDockLayer>,
    },
    Fill {
        inner_view: SharedMut<TabGroupPaneView>,
        parent: WeakMut<PaneDockLayer>,
    },
}
impl PaneDockLayer {
    fn new_root(
        content: impl FnOnce(&WeakMut<Self>) -> Option<SharedMut<Self>>,
    ) -> SharedMut<Self> {
        new_cyclic_shared_mut(|wthis| {
            Self::EmptyRoot(
                content(wthis),
                Rect {
                    X: 0.0,
                    Y: 0.0,
                    Width: 0.0,
                    Height: 0.0,
                },
            )
        })
    }

    fn new_on_left<VC: ViewContext + ?Sized>(
        ctx: &mut VC,
        parent: &WeakMut<Self>,
        docked: impl FnOnce(&WeakMut<Self>, &mut VC) -> SharedMut<Self>,
        rest: impl FnOnce(&WeakMut<Self>, &mut VC) -> SharedMut<Self>,
    ) -> windows::core::Result<SharedMut<Self>> {
        let splitter = PaneSplitterView::new(ctx, SplitDirection::Vertical)?;

        Ok(new_cyclic_shared_mut(|wthis| {
            splitter.borrow_mut().bind_dock_layer(wthis);

            Self::Left {
                docked: docked(wthis, ctx),
                splitter,
                container_region: Rect {
                    X: 0.0,
                    Y: 0.0,
                    Width: 0.0,
                    Height: 0.0,
                },
                rest: rest(wthis, ctx),
                parent: parent.clone(),
            }
        }))
    }
    fn new_on_right<VC: ViewContext + ?Sized>(
        ctx: &mut VC,
        parent: &WeakMut<Self>,
        docked: impl FnOnce(&WeakMut<Self>, &mut VC) -> SharedMut<Self>,
        rest: impl FnOnce(&WeakMut<Self>, &mut VC) -> SharedMut<Self>,
    ) -> windows::core::Result<SharedMut<Self>> {
        let splitter = PaneSplitterView::new(ctx, SplitDirection::Vertical)?;

        Ok(new_cyclic_shared_mut(|wthis| {
            splitter.borrow_mut().bind_dock_layer(wthis);

            Self::Right {
                docked: docked(wthis, ctx),
                splitter,
                container_region: Rect {
                    X: 0.0,
                    Y: 0.0,
                    Width: 0.0,
                    Height: 0.0,
                },
                rest: rest(wthis, ctx),
                parent: parent.clone(),
            }
        }))
    }
    fn new_on_top<VC: ViewContext + ?Sized>(
        ctx: &mut VC,
        parent: &WeakMut<Self>,
        docked: impl FnOnce(&WeakMut<Self>, &mut VC) -> SharedMut<Self>,
        rest: impl FnOnce(&WeakMut<Self>, &mut VC) -> SharedMut<Self>,
    ) -> windows::core::Result<SharedMut<Self>> {
        let splitter = PaneSplitterView::new(ctx, SplitDirection::Horizontal)?;

        Ok(new_cyclic_shared_mut(|wthis| {
            splitter.borrow_mut().bind_dock_layer(wthis);

            Self::Top {
                docked: docked(wthis, ctx),
                splitter,
                container_region: Rect {
                    X: 0.0,
                    Y: 0.0,
                    Width: 0.0,
                    Height: 0.0,
                },
                rest: rest(wthis, ctx),
                parent: parent.clone(),
            }
        }))
    }
    fn new_on_bottom<VC: ViewContext + ?Sized>(
        ctx: &mut VC,
        parent: &WeakMut<Self>,
        docked: impl FnOnce(&WeakMut<Self>, &mut VC) -> SharedMut<Self>,
        rest: impl FnOnce(&WeakMut<Self>, &mut VC) -> SharedMut<Self>,
    ) -> windows::core::Result<SharedMut<Self>> {
        let splitter = PaneSplitterView::new(ctx, SplitDirection::Horizontal)?;

        Ok(new_cyclic_shared_mut(|wthis| {
            splitter.borrow_mut().bind_dock_layer(wthis);

            Self::Bottom {
                docked: docked(wthis, ctx),
                splitter,
                container_region: Rect {
                    X: 0.0,
                    Y: 0.0,
                    Width: 0.0,
                    Height: 0.0,
                },
                rest: rest(wthis, ctx),
                parent: parent.clone(),
            }
        }))
    }
    fn new_filled(
        inner_view: &SharedMut<TabGroupPaneView>,
        parent: &WeakMut<Self>,
    ) -> SharedMut<Self> {
        new_cyclic_shared_mut(|wthis| {
            inner_view.borrow_mut().bind_dock_layer(wthis);

            Self::Fill {
                inner_view: inner_view.clone(),
                parent: parent.clone(),
            }
        })
    }

    #[inline(always)]
    pub const fn is_empty_root(&self) -> bool {
        matches!(self, Self::EmptyRoot(_, _))
    }

    #[inline]
    pub fn parent(&self) -> Option<&WeakMut<Self>> {
        match self {
            Self::Fill { parent, .. }
            | Self::Left { parent, .. }
            | Self::Right { parent, .. }
            | Self::Top { parent, .. }
            | Self::Bottom { parent, .. } => Some(parent),
            Self::EmptyRoot(_, _) => None,
        }
    }

    pub fn controlling_rect(&self) -> Rect {
        match self {
            Self::EmptyRoot(_, container_region)
            | Self::Left {
                container_region, ..
            }
            | Self::Right {
                container_region, ..
            }
            | Self::Top {
                container_region, ..
            }
            | Self::Bottom {
                container_region, ..
            } => container_region.clone(),
            Self::Fill { inner_view, .. } => inner_view.borrow().view_rect.clone(),
        }
    }
    pub fn controlling_rect_left(&self) -> f32 {
        match self {
            Self::EmptyRoot(_, container_region)
            | Self::Left {
                container_region, ..
            }
            | Self::Right {
                container_region, ..
            }
            | Self::Top {
                container_region, ..
            }
            | Self::Bottom {
                container_region, ..
            } => container_region.X,
            Self::Fill { inner_view, .. } => inner_view.borrow().view_rect.X,
        }
    }
    pub fn controlling_rect_right(&self) -> f32 {
        match self {
            Self::EmptyRoot(_, container_region)
            | Self::Left {
                container_region, ..
            }
            | Self::Right {
                container_region, ..
            }
            | Self::Top {
                container_region, ..
            }
            | Self::Bottom {
                container_region, ..
            } => container_region.X + container_region.Width,
            Self::Fill { inner_view, .. } => {
                inner_view.borrow().view_rect.X + inner_view.borrow().view_rect.Width
            }
        }
    }
    pub fn controlling_rect_top(&self) -> f32 {
        match self {
            Self::EmptyRoot(_, container_region)
            | Self::Left {
                container_region, ..
            }
            | Self::Right {
                container_region, ..
            }
            | Self::Top {
                container_region, ..
            }
            | Self::Bottom {
                container_region, ..
            } => container_region.Y,
            Self::Fill { inner_view, .. } => inner_view.borrow().view_rect.Y,
        }
    }
    pub fn controlling_rect_bottom(&self) -> f32 {
        match self {
            Self::EmptyRoot(_, container_region)
            | Self::Left {
                container_region, ..
            }
            | Self::Right {
                container_region, ..
            }
            | Self::Top {
                container_region, ..
            }
            | Self::Bottom {
                container_region, ..
            } => container_region.Y + container_region.Height,
            Self::Fill { inner_view, .. } => {
                inner_view.borrow().view_rect.Y + inner_view.borrow().view_rect.Height
            }
        }
    }
    pub fn controlling_rect_width(&self) -> f32 {
        match self {
            Self::EmptyRoot(_, container_region)
            | Self::Left {
                container_region, ..
            }
            | Self::Right {
                container_region, ..
            }
            | Self::Top {
                container_region, ..
            }
            | Self::Bottom {
                container_region, ..
            } => container_region.Width,
            Self::Fill { inner_view, .. } => inner_view.borrow().view_rect.Width,
        }
    }
    pub fn controlling_rect_height(&self) -> f32 {
        match self {
            Self::EmptyRoot(_, container_region)
            | Self::Left {
                container_region, ..
            }
            | Self::Right {
                container_region, ..
            }
            | Self::Top {
                container_region, ..
            }
            | Self::Bottom {
                container_region, ..
            } => container_region.Height,
            Self::Fill { inner_view, .. } => inner_view.borrow().view_rect.Height,
        }
    }

    #[inline]
    fn split_left(region: Rect, docked_size: f32) -> (Rect, Rect, Rect) {
        let (docked, a) = rect_slice_left(region, docked_size.max(1.0));
        let (splitter, rest) = rect_slice_left(a, PANE_SPLITTER_GAP);

        (docked, splitter, rest)
    }
    #[inline]
    fn split_right(region: Rect, docked_size: f32) -> (Rect, Rect, Rect) {
        let (docked, a) = rect_slice_right(region, docked_size.max(1.0));
        let (splitter, rest) = rect_slice_right(a, PANE_SPLITTER_GAP);

        (docked, splitter, rest)
    }
    #[inline]
    fn split_top(region: Rect, docked_size: f32) -> (Rect, Rect, Rect) {
        let (docked, a) = rect_slice_top(region, docked_size.max(1.0));
        let (splitter, rest) = rect_slice_top(a, PANE_SPLITTER_GAP);

        (docked, splitter, rest)
    }
    #[inline]
    fn split_bottom(region: Rect, docked_size: f32) -> (Rect, Rect, Rect) {
        let (docked, a) = rect_slice_bottom(region, docked_size.max(1.0));
        let (splitter, rest) = rect_slice_bottom(a, PANE_SPLITTER_GAP);

        (docked, splitter, rest)
    }

    pub fn dock_size(&self) -> f32 {
        match self {
            Self::EmptyRoot(_, _) => 0.0,
            Self::Left { docked, .. } | Self::Right { docked, .. } => {
                docked.borrow().controlling_rect_width()
            }
            Self::Top { docked, .. } | Self::Bottom { docked, .. } => {
                docked.borrow().controlling_rect_height()
            }
            Self::Fill { .. } => 0.0,
        }
    }
    /// returns new split bar position
    pub fn set_dock_size(&mut self, size: f32) -> windows::core::Result<(f32, f32)> {
        match self {
            Self::EmptyRoot(_, _) => Ok((0.0, 0.0)),
            Self::Left {
                docked,
                container_region,
                rest,
                ..
            } => {
                let (docked_rect, splitter_rect, rest_rect) =
                    Self::split_left(container_region.clone(), size);
                docked.borrow_mut().layout(docked_rect)?;
                rest.borrow_mut().layout(rest_rect)?;

                Ok((splitter_rect.X, splitter_rect.Y))
            }
            Self::Right {
                docked,
                container_region,
                rest,
                ..
            } => {
                let (docked_rect, splitter_rect, rest_rect) =
                    Self::split_right(container_region.clone(), size);
                docked.borrow_mut().layout(docked_rect)?;
                rest.borrow_mut().layout(rest_rect)?;

                Ok((splitter_rect.X, splitter_rect.Y))
            }
            Self::Top {
                docked,
                container_region,
                rest,
                ..
            } => {
                let (docked_rect, splitter_rect, rest_rect) =
                    Self::split_top(container_region.clone(), size);
                docked.borrow_mut().layout(docked_rect)?;
                rest.borrow_mut().layout(rest_rect)?;

                Ok((splitter_rect.X, splitter_rect.Y))
            }
            Self::Bottom {
                docked,
                container_region,
                rest,
                ..
            } => {
                let (docked_rect, splitter_rect, rest_rect) =
                    Self::split_bottom(container_region.clone(), size);
                docked.borrow_mut().layout(docked_rect)?;
                rest.borrow_mut().layout(rest_rect)?;

                Ok((splitter_rect.X, splitter_rect.Y))
            }
            Self::Fill { .. } => {
                // nop for filling container
                Ok((0.0, 0.0))
            }
        }
    }

    fn reparent(&mut self, new_parent: &Weak<RefCell<Self>>) {
        match self {
            Self::EmptyRoot(_, _) => (),
            Self::Left { parent, .. } => *parent = new_parent.clone(),
            Self::Right { parent, .. } => *parent = new_parent.clone(),
            Self::Top { parent, .. } => *parent = new_parent.clone(),
            Self::Bottom { parent, .. } => *parent = new_parent.clone(),
            Self::Fill { parent, .. } => *parent = new_parent.clone(),
        }
    }

    fn replace_child(&mut self, old_child_ref: &SharedMut<Self>, new_child: &SharedMut<Self>) {
        match self {
            Self::EmptyRoot(r, _) => *r = Some(new_child.clone()),
            Self::Left { docked, .. } if Rc::ptr_eq(docked, old_child_ref) => {
                *docked = new_child.clone();
            }
            Self::Left { rest, .. } if Rc::ptr_eq(rest, old_child_ref) => {
                *rest = new_child.clone();
            }
            Self::Right { docked, .. } if Rc::ptr_eq(docked, old_child_ref) => {
                *docked = new_child.clone();
            }
            Self::Right { rest, .. } if Rc::ptr_eq(rest, old_child_ref) => {
                *rest = new_child.clone();
            }
            Self::Top { docked, .. } if Rc::ptr_eq(docked, old_child_ref) => {
                *docked = new_child.clone();
            }
            Self::Top { rest, .. } if Rc::ptr_eq(rest, old_child_ref) => {
                *rest = new_child.clone();
            }
            Self::Bottom { docked, .. } if Rc::ptr_eq(docked, old_child_ref) => {
                *docked = new_child.clone();
            }
            Self::Bottom { rest, .. } if Rc::ptr_eq(rest, old_child_ref) => {
                *rest = new_child.clone();
            }
            _ => unreachable!("invalid tree"),
        }
    }

    fn mount_recursive(
        &self,
        onto: &VisualCollection,
        onto_ht: &SharedMut<HitTestTree>,
    ) -> windows::core::Result<()> {
        match self {
            // no child
            Self::EmptyRoot(None, _) => Ok(()),
            Self::EmptyRoot(Some(r), _) => r.borrow().mount_recursive(onto, onto_ht),
            Self::Left {
                docked,
                splitter,
                rest,
                ..
            }
            | Self::Right {
                docked,
                splitter,
                rest,
                ..
            }
            | Self::Top {
                docked,
                splitter,
                rest,
                ..
            }
            | Self::Bottom {
                docked,
                splitter,
                rest,
                ..
            } => {
                docked.borrow().mount_recursive(onto, onto_ht)?;
                splitter.borrow().mount(onto, onto_ht)?;
                rest.borrow().mount_recursive(onto, onto_ht)
            }
            Self::Fill { inner_view, .. } => inner_view.borrow().mount(onto, onto_ht),
        }
    }

    fn relayout(&mut self) -> windows::core::Result<()> {
        let region = self.controlling_rect();
        match self {
            // no child
            Self::EmptyRoot(None, r) => {
                *r = region;
                Ok(())
            }
            Self::EmptyRoot(Some(r), rect) => {
                *rect = region.clone();
                r.borrow_mut().layout(region)
            }
            Self::Left {
                docked,
                splitter,
                container_region,
                rest,
                ..
            } => {
                *container_region = region.clone();
                let w = docked.borrow().controlling_rect_width();
                let (docked_rect, splitter_rect, rest_rect) = Self::split_left(region, w);
                docked.borrow_mut().layout(docked_rect)?;
                splitter.borrow().set_rect(splitter_rect)?;
                rest.borrow_mut().layout(rest_rect)
            }
            Self::Right {
                docked,
                splitter,
                container_region,
                rest,
                ..
            } => {
                *container_region = region.clone();
                let w = docked.borrow().controlling_rect_width();
                let (docked_rect, splitter_rect, rest_rect) = Self::split_right(region, w);
                docked.borrow_mut().layout(docked_rect)?;
                splitter.borrow().set_rect(splitter_rect)?;
                rest.borrow_mut().layout(rest_rect)
            }
            Self::Top {
                docked,
                splitter,
                container_region,
                rest,
                ..
            } => {
                *container_region = region;
                let h = docked.borrow().controlling_rect_height();
                let (docked_rect, splitter_rect, rest_rect) = Self::split_top(region, h);
                docked.borrow_mut().layout(docked_rect)?;
                splitter.borrow().set_rect(splitter_rect)?;
                rest.borrow_mut().layout(rest_rect)
            }
            Self::Bottom {
                docked,
                splitter,
                container_region,
                rest,
                ..
            } => {
                *container_region = region;
                let h = docked.borrow().controlling_rect_height();
                let (docked_rect, splitter_rect, rest_rect) = Self::split_bottom(region, h);
                docked.borrow_mut().layout(docked_rect)?;
                splitter.borrow().set_rect(splitter_rect)?;
                rest.borrow_mut().layout(rest_rect)
            }
            Self::Fill { inner_view, .. } => inner_view.borrow_mut().set_rect(region),
        }
    }

    fn layout(&mut self, region: Rect) -> windows::core::Result<()> {
        match self {
            // no child
            Self::EmptyRoot(None, r) => {
                *r = region;
                Ok(())
            }
            Self::EmptyRoot(Some(r), rect) => {
                *rect = region.clone();
                r.borrow_mut().layout(region)
            }
            Self::Left {
                docked,
                splitter,
                container_region,
                rest,
                ..
            } => {
                if *container_region == region {
                    return Ok(());
                }

                *container_region = region.clone();
                let w = docked.borrow().controlling_rect_width();
                let (docked_rect, splitter_rect, rest_rect) = Self::split_left(region, w);
                docked.borrow_mut().layout(docked_rect)?;
                splitter.borrow().set_rect(splitter_rect)?;
                rest.borrow_mut().layout(rest_rect)
            }
            Self::Right {
                docked,
                splitter,
                container_region,
                rest,
                ..
            } => {
                if *container_region == region {
                    return Ok(());
                }

                *container_region = region.clone();
                let w = docked.borrow().controlling_rect_width();
                let (docked_rect, splitter_rect, rest_rect) = Self::split_right(region, w);
                docked.borrow_mut().layout(docked_rect)?;
                splitter.borrow().set_rect(splitter_rect)?;
                rest.borrow_mut().layout(rest_rect)
            }
            Self::Top {
                docked,
                splitter,
                container_region,
                rest,
                ..
            } => {
                if *container_region == region {
                    return Ok(());
                }

                *container_region = region;
                let h = docked.borrow().controlling_rect_height();
                let (docked_rect, splitter_rect, rest_rect) = Self::split_top(region, h);
                docked.borrow_mut().layout(docked_rect)?;
                splitter.borrow().set_rect(splitter_rect)?;
                rest.borrow_mut().layout(rest_rect)
            }
            Self::Bottom {
                docked,
                splitter,
                container_region,
                rest,
                ..
            } => {
                if *container_region == region {
                    return Ok(());
                }

                *container_region = region;
                let h = docked.borrow().controlling_rect_height();
                let (docked_rect, splitter_rect, rest_rect) = Self::split_bottom(region, h);
                docked.borrow_mut().layout(docked_rect)?;
                splitter.borrow().set_rect(splitter_rect)?;
                rest.borrow_mut().layout(rest_rect)
            }
            Self::Fill { inner_view, .. } => inner_view.borrow_mut().set_rect(region),
        }
    }

    /// returns: relayout root
    pub fn undock(this: &SharedMut<Self>) -> SharedMut<Self> {
        let parent = this
            .borrow()
            .parent()
            .expect("illegal binding with dock layer")
            .upgrade()
            .expect("Parent has gone?");

        let relayout_root = match &mut *parent.borrow_mut() {
            Self::EmptyRoot(rest, _) => {
                *rest = None;
                parent.clone()
            }
            Self::Left {
                docked,
                rest,
                parent: parent1,
                splitter,
                ..
            } => {
                let new_child = if Rc::ptr_eq(docked, this) {
                    rest
                } else {
                    docked
                };

                new_child.borrow_mut().reparent(parent1);
                parent1
                    .upgrade()
                    .unwrap()
                    .borrow_mut()
                    .replace_child(&parent, new_child);
                splitter
                    .borrow()
                    .unmount()
                    .expect("Failed to unmounting splitter");
                parent1.upgrade().unwrap()
            }
            Self::Right {
                docked,
                rest,
                parent: parent1,
                splitter,
                ..
            } => {
                let new_child = if Rc::ptr_eq(docked, this) {
                    rest
                } else {
                    docked
                };

                new_child.borrow_mut().reparent(parent1);
                parent1
                    .upgrade()
                    .unwrap()
                    .borrow_mut()
                    .replace_child(&parent, new_child);
                splitter
                    .borrow()
                    .unmount()
                    .expect("Failed to unmounting splitter");
                parent1.upgrade().unwrap()
            }
            Self::Top {
                docked,
                rest,
                parent: parent1,
                splitter,
                ..
            } => {
                let new_child = if Rc::ptr_eq(docked, this) {
                    rest
                } else {
                    docked
                };

                new_child.borrow_mut().reparent(parent1);
                parent1
                    .upgrade()
                    .unwrap()
                    .borrow_mut()
                    .replace_child(&parent, new_child);
                splitter
                    .borrow()
                    .unmount()
                    .expect("Failed to unmounting splitter");
                parent1.upgrade().unwrap()
            }
            Self::Bottom {
                docked,
                rest,
                parent: parent1,
                splitter,
                ..
            } => {
                let new_child = if Rc::ptr_eq(docked, this) {
                    rest
                } else {
                    docked
                };

                new_child.borrow_mut().reparent(parent1);
                parent1
                    .upgrade()
                    .unwrap()
                    .borrow_mut()
                    .replace_child(&parent, new_child);
                splitter
                    .borrow()
                    .unmount()
                    .expect("Failed to unmounting splitter");
                parent1.upgrade().unwrap()
            }
            Self::Fill { .. } => unreachable!("invalid structure"),
        };

        relayout_root
    }

    pub fn compute_recommended_docking_destination(
        this: &SharedMut<Self>,
        mode: RedockingMode,
        local_x: f32,
        local_y: f32,
    ) -> PaneDockingRecommendation {
        match &*this.borrow() {
            Self::EmptyRoot(None, _) => PaneDockingRecommendation::Free,
            Self::EmptyRoot(Some(r), _) => {
                Self::compute_recommended_docking_destination(r, mode, local_x, local_y)
            }
            Self::Left { docked, rest, .. } => {
                let left_thres = docked.borrow().controlling_rect_width() + PANE_SPLITTER_GAP * 0.5;
                if local_x < left_thres {
                    Self::compute_recommended_docking_destination(docked, mode, local_x, local_y)
                } else {
                    Self::compute_recommended_docking_destination(
                        rest,
                        mode,
                        local_x - docked.borrow().controlling_rect_width() - PANE_SPLITTER_GAP,
                        local_y,
                    )
                }
            }
            Self::Right { docked, rest, .. } => {
                let right_thres = rest.borrow().controlling_rect_width() + PANE_SPLITTER_GAP * 0.5;
                if right_thres < local_x {
                    Self::compute_recommended_docking_destination(
                        docked,
                        mode,
                        local_x - rest.borrow().controlling_rect_width() - PANE_SPLITTER_GAP,
                        local_y,
                    )
                } else {
                    Self::compute_recommended_docking_destination(rest, mode, local_x, local_y)
                }
            }
            Self::Top { docked, rest, .. } => {
                let top_thres = docked.borrow().controlling_rect_height() + PANE_SPLITTER_GAP * 0.5;
                if local_y < top_thres {
                    Self::compute_recommended_docking_destination(docked, mode, local_x, local_y)
                } else {
                    Self::compute_recommended_docking_destination(
                        rest,
                        mode,
                        local_x,
                        local_y - docked.borrow().controlling_rect_height() - PANE_SPLITTER_GAP,
                    )
                }
            }
            Self::Bottom { docked, rest, .. } => {
                let bottom_thres =
                    rest.borrow().controlling_rect_height() + PANE_SPLITTER_GAP * 0.5;
                if bottom_thres < local_y {
                    Self::compute_recommended_docking_destination(
                        docked,
                        mode,
                        local_x,
                        local_y - rest.borrow().controlling_rect_height() - PANE_SPLITTER_GAP,
                    )
                } else {
                    Self::compute_recommended_docking_destination(rest, mode, local_x, local_y)
                }
            }
            Self::Fill { inner_view, parent } => {
                let rect = inner_view.borrow().relative_rect();
                if !rect.contains_point(local_x, local_y) {
                    // overflow
                    return PaneDockingRecommendation::Free;
                }

                if mode == RedockingMode::Tab && local_y <= inner_view.borrow().tab_height {
                    // タブ領域にいる
                    return PaneDockingRecommendation::MergeGroup(inner_view.clone());
                }

                if local_x.min(rect.Width - local_x) < local_y.min(rect.Height - local_y) {
                    // xのほうがエッジに近い
                    if let Some(parent) = parent.upgrade().filter(|x| !x.borrow().is_empty_root()) {
                        // 一つ上のレベルでドックできるかも
                        if local_x < 8.0 {
                            return PaneDockingRecommendation::Left(parent);
                        }
                        if local_x > rect.Width - 8.0 {
                            return PaneDockingRecommendation::Right(parent);
                        }
                    }

                    match local_x / rect.Width {
                        r if r <= 0.3 => PaneDockingRecommendation::Left(this.clone()),
                        r if 0.7 <= r => PaneDockingRecommendation::Right(this.clone()),
                        _ => PaneDockingRecommendation::Free,
                    }
                } else {
                    // yのほうがエッジに近い
                    if let Some(parent) = parent.upgrade().filter(|x| !x.borrow().is_empty_root()) {
                        // 一つ上のレベルでドックできるかも
                        if local_y < 8.0 {
                            return PaneDockingRecommendation::Top(parent);
                        }
                        if local_y > rect.Width - 8.0 {
                            return PaneDockingRecommendation::Bottom(parent);
                        }
                    }

                    match local_y / rect.Height {
                        r if r <= 0.3 => PaneDockingRecommendation::Top(this.clone()),
                        r if 0.7 <= r => PaneDockingRecommendation::Bottom(this.clone()),
                        _ => PaneDockingRecommendation::Free,
                    }
                }
            }
        }
    }
}

pub struct PaneGroupDockingManager {
    docks: SharedMut<PaneDockLayer>,
    placement_visual: ContainerVisual,
    ht_placement_root: SharedMut<HitTestTree>,
    floating_preview: DockingPanePreview,
}
impl PaneGroupDockingManager {
    fn new(
        ctx: &mut impl ViewContext,
        ht_root: &SharedMut<HitTestTree>,
    ) -> windows::core::Result<Self> {
        Ok(Self {
            docks: PaneDockLayer::new_root(|_| None),
            placement_visual: ctx.compositor().CreateContainerVisual()?,
            ht_placement_root: ht_root.clone(),
            floating_preview: DockingPanePreview::new(ctx)?,
        })
    }

    fn set_layout(&mut self, layout: SharedMut<PaneDockLayer>) -> windows::core::Result<()> {
        let children = self.placement_visual.Children()?;
        children.RemoveAll()?;
        // TODO: HitTestTreeのほうもきれいにする
        layout
            .borrow()
            .mount_recursive(&children, &self.ht_placement_root)?;

        self.docks = layout;
        Ok(())
    }
    fn resize_root(&mut self, width: f32, height: f32) -> windows::core::Result<()> {
        self.docks
            .borrow_mut()
            .layout(Rect::from_size(width, height))?;

        Ok(())
    }
    fn mount_splitter_only(&self, layout: &PaneDockLayer) -> windows::core::Result<()> {
        match layout {
            PaneDockLayer::EmptyRoot(_, _) | PaneDockLayer::Fill { .. } => Ok(()),
            PaneDockLayer::Left { splitter, .. }
            | PaneDockLayer::Right { splitter, .. }
            | PaneDockLayer::Top { splitter, .. }
            | PaneDockLayer::Bottom { splitter, .. } => splitter
                .borrow()
                .mount(&self.placement_visual.Children()?, &self.ht_placement_root),
        }
    }
    fn mount_filled(&self, layout: &PaneDockLayer) -> windows::core::Result<()> {
        let PaneDockLayer::Fill { inner_view, .. } = layout else {
            return Ok(());
        };

        inner_view
            .borrow()
            .mount(&self.placement_visual.Children()?, &self.ht_placement_root)
    }

    fn show_preview_at(&self, rect: Rect) -> windows::core::Result<()> {
        self.floating_preview.show()?;
        self.set_preview_rect(rect)
    }
    fn hide_preview(&self) -> windows::core::Result<()> {
        self.floating_preview.hide()
    }
    fn set_preview_rect(&self, rect: Rect) -> windows::core::Result<()> {
        self.floating_preview.set_rect(rect)
    }

    fn compute_recommended_docking_destination(
        &self,
        mode: RedockingMode,
        x: f32,
        y: f32,
    ) -> PaneDockingRecommendation {
        PaneDockLayer::compute_recommended_docking_destination(&self.docks, mode, x, y)
    }
}

pub struct TabGroupPaneView {
    docking_manager: WeakMut<PaneGroupDockingManager>,
    bound_dock_layer: WeakMut<PaneDockLayer>,
    root: ContainerVisual,
    content_area: ContainerVisual,
    content_area_base: SpriteVisual,
    ht_ref: SharedMut<HitTestTree>,
    ht_ref_content: SharedMut<HitTestTree>,
    current_active: usize,
    tab_height: f32,
    view_rect: Rect,
    tabs: Vec<(
        SharedMut<PaneTabHeaderView>,
        SharedMut<dyn PaneTabContentPresenter>,
    )>,
    drag_base_point: Option<(f32, f32, f32, f32)>,
    preview_rect: Rect,
}
impl TabGroupPaneView {
    const CONTENT_AREA_BASE_COLOR: Color = Color {
        A: 255,
        R: 64,
        G: 64,
        B: 72,
    };

    pub fn new(
        docking_manager: &SharedMut<PaneGroupDockingManager>,
        ctx: &mut (impl ViewContext + ?Sized),
    ) -> windows::core::Result<SharedMut<Self>> {
        let root = ctx.compositor().CreateContainerVisual()?;
        root.SetSize(Vector2::scalar(128.0))?;

        let content_area = ctx.compositor().CreateContainerVisual()?;
        content_area.SetRelativeSizeAdjustment(Vector2::one())?;
        root.Children()?.InsertAtBottom(&content_area)?;

        let content_area_base = ctx.compositor().CreateSpriteVisual()?;
        content_area_base.SetBrush(
            &ctx.compositor()
                .CreateColorBrushWithColor(Self::CONTENT_AREA_BASE_COLOR)?,
        )?;
        content_area_base.SetRelativeOffsetAdjustment(Vector3::zero())?;
        content_area_base.SetRelativeSizeAdjustment(Vector2::one())?;
        root.Children()?.InsertAtBottom(&content_area_base)?;
        root.SetClip(
            &ctx.compositor()
                .CreateInsetClipWithInsets(0.0, 0.0, 0.0, 0.0)?,
        )?;

        Ok(new_cyclic_shared_mut(|wthis| {
            let ht = HitTestTree::new(
                &Rc::new(wthis.clone()),
                ctx.hittest_context_mut().new_id(),
                Rect::from_size(128.0, 128.0),
            );
            let ht_content = HitTestTree::new(
                &Rc::new(wthis.clone()),
                ctx.hittest_context_mut().new_id(),
                Rect::from_size(128.0, 128.0),
            );
            HitTestTree::add_child(&ht, ht_content.clone());

            Self {
                docking_manager: Rc::downgrade(docking_manager),
                bound_dock_layer: empty_weak_mut(),
                root,
                content_area,
                content_area_base,
                ht_ref: ht,
                ht_ref_content: ht_content,
                current_active: 0,
                tab_height: 0.0,
                view_rect: Rect {
                    X: 0.0,
                    Y: 0.0,
                    Width: 128.0,
                    Height: 128.0,
                },
                tabs: Vec::new(),
                drag_base_point: None,
                preview_rect: Rect {
                    X: 0.0,
                    Y: 0.0,
                    Width: 0.0,
                    Height: 0.0,
                },
            }
        }))
    }
    pub fn bind_dock_layer(&mut self, layer: &WeakMut<PaneDockLayer>) {
        self.bound_dock_layer = layer.clone();
    }

    pub fn move_tab_into(
        &mut self,
        tab: &SharedMut<PaneTabHeaderView>,
        target: &SharedMut<Self>,
        mut view_ctx: &mut (impl ViewContext + ?Sized),
    ) -> windows::core::Result<()> {
        let index = tab.borrow().index_in_group;

        if tab.borrow().is_active && self.tabs.len() > 1 {
            // アクティブを付け替える（0個になる場合はどのみち消されるのでなにもしない）
            let new_active = if index == 0 { 1 } else { index - 1 };
            self.switch_active(new_active, &mut view_ctx)?;
        }
        tab.borrow().unmount()?;
        let (tab, content) = self.tabs.remove(index);

        let new_tab_index = Self::add_tab_raw(target, &tab, content.clone())?;
        // activate this tab
        target.borrow_mut().switch_active(new_tab_index, view_ctx)?;

        Ok(())
    }

    pub fn split_tab(
        &mut self,
        tab: &SharedMut<PaneTabHeaderView>,
        mut view_ctx: &mut (impl ViewContext + ?Sized),
    ) -> windows::core::Result<Option<SharedMut<Self>>> {
        let Some(index) = self.tabs.iter().position(|(h, _)| Rc::ptr_eq(h, tab)) else {
            // 対応するタブがない
            return Ok(None);
        };

        if tab.borrow().is_active && self.tabs.len() > 1 {
            // アクティブを付け替える（0個になる場合はどのみち消されるので何もしない）
            let new_active = if index == 0 { 1 } else { index - 1 };
            self.switch_active(new_active, &mut view_ctx)?;
        }
        tab.borrow().unmount()?;
        let (tab, content) = self.tabs.remove(index);

        let new_group = Self::new(
            &self
                .docking_manager
                .upgrade()
                .expect("Docking Manager has dead"),
            &mut view_ctx,
        )?;
        Self::add_tab_raw(&new_group, &tab, content.clone())?;

        // this is the first tab
        content.borrow_mut().build_content_view(
            &new_group.borrow().content_area,
            &self.ht_ref_content,
            &mut view_ctx,
        )?;
        tab.borrow_mut().set_active_imm(true, view_ctx)?;

        Ok(Some(new_group))
    }

    pub fn add_tab<T: PaneTabPresenter + 'static>(
        this: &SharedMut<Self>,
        ctx: &mut impl ViewContext,
    ) -> windows::core::Result<SharedMut<T>> {
        let header_view =
            PaneTabHeaderView::new(T::INIT_TAB_NAME, this.borrow().tabs.is_empty(), ctx)?;
        let content_presenter = new_shared_mut(T::new(&header_view));
        Self::add_tab_raw(this, &header_view, content_presenter.clone())?;

        let thisref = this.borrow();
        if thisref.tabs.len() == 1 {
            // first tab
            content_presenter.borrow_mut().build_content_view(
                &thisref.content_area,
                &thisref.ht_ref_content,
                ctx,
            )?;
        }

        Ok(content_presenter)
    }

    fn add_tab_raw(
        this: &SharedMut<Self>,
        header: &SharedMut<PaneTabHeaderView>,
        content: SharedMut<dyn PaneTabContentPresenter>,
    ) -> windows::core::Result<usize> {
        let new_index = this.borrow().tabs.len();
        header.borrow_mut().bind_group_view(this, new_index);
        let mut thisref = this.borrow_mut();
        thisref.tabs.push((header.clone(), content));
        header
            .borrow()
            .mount(&thisref.root.Children()?, &thisref.ht_ref)?;

        Ok(new_index)
    }

    fn readjust_content_area(&mut self) -> windows::core::Result<()> {
        let content_area = Rect {
            X: 0.0,
            Y: self.tab_height,
            Width: self.view_rect.Width,
            Height: (self.view_rect.Height - self.tab_height).max(0.0),
        };

        self.content_area.set_properties().rect(&content_area)?;
        self.content_area_base
            .set_properties()
            .rect(&content_area)?;
        self.ht_ref_content.borrow_mut().set_rect(
            content_area.X,
            content_area.Y,
            content_area.Width,
            content_area.Height,
        );

        Ok(())
    }

    pub fn rearrange(&mut self) {
        let mut offset = 0.0;
        self.tab_height = 0.0f32;
        for (n, v) in self.tabs.iter().enumerate() {
            v.0.borrow()
                .set_offset(offset, 0.0)
                .expect("Failed to set tab offset");
            offset += v.0.borrow().width;
            self.tab_height = self.tab_height.max(v.0.borrow().height);
            v.0.borrow_mut().index_in_group = n;
        }

        self.readjust_content_area()
            .expect("Failed to readjust content area");
    }

    #[inline]
    pub const fn relative_rect(&self) -> Rect {
        Rect {
            X: 0.0,
            Y: 0.0,
            Width: self.view_rect.Width,
            Height: self.view_rect.Height,
        }
    }

    pub fn set_width(&mut self, width: f32) -> windows::core::Result<()> {
        self.root.SetSize(Vector2 {
            X: width,
            Y: self.view_rect.Height,
        })?;
        self.ht_ref
            .borrow_mut()
            .set_size(width, self.view_rect.Height);
        self.view_rect.Width = width;

        self.readjust_content_area()?;
        Ok(())
    }
    pub fn set_height(&mut self, height: f32) -> windows::core::Result<()> {
        self.root.SetSize(Vector2 {
            X: self.view_rect.Width,
            Y: height,
        })?;
        self.ht_ref
            .borrow_mut()
            .set_size(self.view_rect.Width, height);
        self.view_rect.Height = height;

        self.readjust_content_area()?;
        Ok(())
    }
    pub fn resize(&mut self, width: f32, height: f32) -> windows::core::Result<()> {
        self.root.SetSize(Vector2 {
            X: width,
            Y: height,
        })?;
        self.ht_ref.borrow_mut().set_size(width, height);
        self.view_rect.Width = width;
        self.view_rect.Height = height;

        self.readjust_content_area()?;
        Ok(())
    }
    pub fn set_rect(&mut self, rect: Rect) -> windows::core::Result<()> {
        self.root.set_properties().rect(&rect)?;
        self.ht_ref
            .borrow_mut()
            .set_rect(rect.X, rect.Y, rect.Width, rect.Height);
        self.view_rect = rect;

        self.readjust_content_area()?;
        Ok(())
    }

    pub fn switch_active(
        &mut self,
        new_active: usize,
        mut view_ctx: impl ViewContext,
    ) -> windows::core::Result<()> {
        let new_active = new_active.min(self.tabs.len());
        if self.current_active == new_active {
            // 変わってないのでなにもしない
            return Ok(());
        }

        self.tabs[self.current_active]
            .1
            .borrow_mut()
            .on_hide_content_view(&mut view_ctx)?;
        self.tabs[self.current_active]
            .0
            .borrow_mut()
            .set_active(false, &mut view_ctx)?;
        self.content_area.Children()?.RemoveAll()?;
        self.current_active = new_active;
        self.tabs[self.current_active]
            .1
            .borrow_mut()
            .build_content_view(&self.content_area, &self.ht_ref_content, &mut view_ctx)?;
        self.tabs[self.current_active]
            .0
            .borrow_mut()
            .set_active(true, &mut view_ctx)?;

        Ok(())
    }

    pub fn mount(
        &self,
        onto: &VisualCollection,
        onto_ht: &SharedMut<HitTestTree>,
    ) -> windows::core::Result<()> {
        onto.InsertAtTop(&self.root)?;
        HitTestTree::add_child(onto_ht, self.ht_ref.clone());

        Ok(())
    }
    pub fn unmount(&self) -> windows::core::Result<()> {
        self.root.Parent()?.Children()?.Remove(&self.root)?;
        let ht_ref = self.ht_ref.borrow();
        if let Some(parent_ht) = ht_ref.parent.upgrade() {
            drop(ht_ref);
            parent_ht.borrow_mut().remove_child(&self.ht_ref);
        }

        Ok(())
    }
}
impl InputEventHandler for WeakMut<TabGroupPaneView> {
    fn on_begin_drag(&self, x: f32, y: f32, window: HWND, ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };
        let Some(docking_manager) = this.borrow().docking_manager.upgrade() else {
            return;
        };

        let mut thisref = this.borrow_mut();
        let HitTestTree { rect, .. } = *thisref.ht_ref.borrow();

        let app_window = AppWindow::wrap(window);

        thisref.drag_base_point = Some((x, y, rect.X, rect.Y));
        thisref.preview_rect = rect.clone();
        docking_manager
            .borrow()
            .show_preview_at(app_window.dip_rect_to_desktop_pixels_rect(&rect))
            .expect("Failed to show floating preview");

        ctx.capture_mouse();
    }
    fn on_drag_move(&self, x: f32, y: f32, window: HWND, _ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };
        let Some((bx, by, ox, oy)) = this.borrow().drag_base_point else {
            return;
        };
        let Some(docking_manager) = this.borrow().docking_manager.upgrade() else {
            return;
        };

        let window = AppWindow::wrap(window);
        let recommended_dest = docking_manager
            .borrow()
            .compute_recommended_docking_destination(
                RedockingMode::Pane,
                window.pixels_to_dip(x),
                window.pixels_to_dip(y),
            );
        let preview_rect = this.borrow().preview_rect.clone();
        let new_rect = recommended_dest
            .dock_rect(&preview_rect)
            .unwrap_or_else(|| Rect {
                X: ox + window.pixels_to_dip(x - bx),
                Y: oy + window.pixels_to_dip(y - by),
                ..preview_rect.clone()
            });
        docking_manager
            .borrow()
            .set_preview_rect(window.dip_rect_to_desktop_pixels_rect(&new_rect))
            .expect("Failed to update preview rect");
    }
    fn on_end_drag(&self, x: f32, y: f32, window: HWND, ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };
        let Some(docking_manager) = this.borrow().docking_manager.upgrade() else {
            return;
        };

        let window = AppWindow::wrap(window);
        let recommended_dest = docking_manager
            .borrow()
            .compute_recommended_docking_destination(
                RedockingMode::Pane,
                window.pixels_to_dip(x),
                window.pixels_to_dip(y),
            );
        match recommended_dest {
            PaneDockingRecommendation::Left(d) => {
                let bound_dock_layer = this.borrow().bound_dock_layer.upgrade().unwrap();
                if !Rc::ptr_eq(&bound_dock_layer, &d) {
                    let relayout_root = PaneDockLayer::undock(&bound_dock_layer);

                    let dest_parent = d
                        .borrow()
                        .parent()
                        .expect("Docking on root?")
                        .upgrade()
                        .expect("Parent has gone?");
                    let new_layer = PaneDockLayer::new_on_left(
                        &mut *ctx,
                        &Rc::downgrade(&dest_parent),
                        |parent, _ctx| PaneDockLayer::new_filled(&this, parent),
                        |parent, _ctx| {
                            d.borrow_mut().reparent(parent);
                            d.clone()
                        },
                    )
                    .expect("Failed to create new dock layer");
                    docking_manager
                        .borrow()
                        .mount_splitter_only(&new_layer.borrow())
                        .expect("Failed to mount new splitter");
                    dest_parent.borrow_mut().replace_child(&d, &new_layer);
                    dest_parent
                        .borrow_mut()
                        .relayout()
                        .expect("Failed to relayout from new parent");
                    relayout_root
                        .borrow_mut()
                        .relayout()
                        .expect("Failed to relayout from old parent");
                }
            }
            PaneDockingRecommendation::Right(d) => {
                let bound_dock_layer = this.borrow().bound_dock_layer.upgrade().unwrap();
                if !Rc::ptr_eq(&bound_dock_layer, &d) {
                    let relayout_root = PaneDockLayer::undock(&bound_dock_layer);

                    let dest_parent = d
                        .borrow()
                        .parent()
                        .expect("Docking on root?")
                        .upgrade()
                        .expect("Parent has gone?");
                    let new_layer = PaneDockLayer::new_on_right(
                        &mut *ctx,
                        &Rc::downgrade(&dest_parent),
                        |parent, _ctx| PaneDockLayer::new_filled(&this, parent),
                        |parent, _ctx| {
                            d.borrow_mut().reparent(parent);
                            d.clone()
                        },
                    )
                    .expect("Failed to create new dock layer");
                    this.borrow()
                        .docking_manager
                        .upgrade()
                        .unwrap()
                        .borrow()
                        .mount_splitter_only(&new_layer.borrow())
                        .expect("Failed to mount new splitter");
                    dest_parent.borrow_mut().replace_child(&d, &new_layer);
                    dest_parent
                        .borrow_mut()
                        .relayout()
                        .expect("Failed to relayout from new parent");
                    relayout_root
                        .borrow_mut()
                        .relayout()
                        .expect("Failed to relayout from old parent");
                }
            }
            PaneDockingRecommendation::Top(d) => {
                let bound_dock_layer = this.borrow().bound_dock_layer.upgrade().unwrap();
                if !Rc::ptr_eq(&bound_dock_layer, &d) {
                    let relayout_root = PaneDockLayer::undock(&bound_dock_layer);

                    let dest_parent = d
                        .borrow()
                        .parent()
                        .expect("Docking on root?")
                        .upgrade()
                        .expect("Parent has gone?");
                    let new_layer = PaneDockLayer::new_on_top(
                        &mut *ctx,
                        &Rc::downgrade(&dest_parent),
                        |parent, _ctx| PaneDockLayer::new_filled(&this, parent),
                        |parent, _ctx| {
                            d.borrow_mut().reparent(parent);
                            d.clone()
                        },
                    )
                    .expect("Failed to create new dock layer");
                    this.borrow()
                        .docking_manager
                        .upgrade()
                        .unwrap()
                        .borrow()
                        .mount_splitter_only(&new_layer.borrow())
                        .expect("Failed to mount new splitter");
                    dest_parent.borrow_mut().replace_child(&d, &new_layer);
                    dest_parent
                        .borrow_mut()
                        .relayout()
                        .expect("Failed to relayout from new parent");
                    relayout_root
                        .borrow_mut()
                        .relayout()
                        .expect("Failed to relayout from old parent");
                }
            }
            PaneDockingRecommendation::Bottom(d) => {
                let bound_dock_layer = this.borrow().bound_dock_layer.upgrade().unwrap();
                if !Rc::ptr_eq(&bound_dock_layer, &d) {
                    let relayout_root = PaneDockLayer::undock(&bound_dock_layer);

                    let dest_parent = d
                        .borrow()
                        .parent()
                        .expect("Docking on root?")
                        .upgrade()
                        .expect("Parent has gone?");
                    let new_layer = PaneDockLayer::new_on_bottom(
                        &mut *ctx,
                        &Rc::downgrade(&dest_parent),
                        |parent, _ctx| PaneDockLayer::new_filled(&this, parent),
                        |parent, _ctx| {
                            d.borrow_mut().reparent(parent);
                            d.clone()
                        },
                    )
                    .expect("Failed to create new dock layer");
                    this.borrow()
                        .docking_manager
                        .upgrade()
                        .unwrap()
                        .borrow()
                        .mount_splitter_only(&new_layer.borrow())
                        .expect("Failed to mount new splitter");
                    dest_parent.borrow_mut().replace_child(&d, &new_layer);
                    dest_parent
                        .borrow_mut()
                        .relayout()
                        .expect("Failed to relayout from new parent");
                    relayout_root
                        .borrow_mut()
                        .relayout()
                        .expect("Failed to relayout from old parent");
                }
            }
            PaneDockingRecommendation::Free => {
                let bound_dock_layer = this.borrow().bound_dock_layer.upgrade().unwrap();
                let relayout_root = PaneDockLayer::undock(&bound_dock_layer);

                this.borrow()
                    .unmount()
                    .expect("Failed to unmount group view");
                relayout_root
                    .borrow_mut()
                    .relayout()
                    .expect("Failed to relayout docks");

                println!("TODO: floating");
            }
            PaneDockingRecommendation::MergeGroup(_) => unreachable!(),
        }

        docking_manager
            .borrow()
            .hide_preview()
            .expect("Failed to show floating preview");
        ctx.release_mouse_capture();
    }
}

pub struct PaneTabHeaderView {
    group_view: WeakMut<TabGroupPaneView>,
    index_in_group: usize,
    label: Cow<'static, str>,
    visual: LayerVisual,
    bg_visual: ShapeVisual,
    active_overlay_visual: ShapeVisual,
    label_content_brush: CompositionSurfaceBrush,
    bg_hover_animation: ScalarKeyFrameAnimation,
    bg_hover_end_animation: ScalarKeyFrameAnimation,
    active_overlay_enter_animation: ScalarKeyFrameAnimation,
    active_overlay_leave_animation: ScalarKeyFrameAnimation,
    hittest_tree_self: SharedMut<HitTestTree>,
    bg_active: bool,
    is_active: bool,
    width: f32,
    height: f32,
    drag_base_point: Option<(f32, f32, f32, f32)>,
    preview_rect: Rect,
}
impl PaneTabHeaderView {
    fn create_geometry(
        text_width: f32,
        text_height: f32,
        ctx: &mut impl ViewContext,
    ) -> windows::core::Result<CompositionRoundedRectangleGeometry> {
        let g = ctx.compositor().CreateRoundedRectangleGeometry()?;
        g.SetCornerRadius(Vector2 {
            X: TAB_RADIUS,
            Y: TAB_RADIUS,
        })
        .expect("Failed to set corner radius");
        g.SetSize(Vector2 {
            X: text_width + TAB_MARGIN_X * 2.0,
            // 高さ2倍にして下半分を見切れさせる（丸角にしない）
            Y: (text_height + TAB_MARGIN_Y * 2.0) * 2.0,
        })?;

        Ok(g)
    }

    pub fn new(
        title: impl Into<Cow<'static, str>>,
        init_active: bool,
        ctx: &mut impl ViewContext,
    ) -> windows::core::Result<SharedMut<Self>> {
        let base = ctx.compositor().CreateLayerVisual()?;
        let title = title.into();
        let font = if init_active {
            ctx.common().tab_active_title_font.clone()
        } else {
            ctx.common().tab_title_font.clone()
        };
        let title_text = ctx.text_surface_stock_mut().get(&font, title.clone())?;
        let view_size = Vector2 {
            X: title_text.width + TAB_MARGIN_X * 2.0,
            Y: title_text.height + TAB_MARGIN_Y * 2.0,
        };
        let label_content_brush = ctx
            .compositor()
            .CreateSurfaceBrushWithSurface(&title_text.surface)?;
        base.Children()
            .expect("Failed to get children collection")
            .InsertAtTop(&{
                let v = ctx.compositor().CreateSpriteVisual()?;
                v.SetBrush(&label_content_brush)?;
                v.SetSize(title_text.visual_size())?;
                v.SetAnchorPoint(Vector2::scalar(0.5))?;
                v.SetOffset(Vector3 {
                    X: title_text.width * 0.5 + TAB_MARGIN_X,
                    Y: title_text.height * 0.5 + TAB_MARGIN_Y,
                    Z: 0.0,
                })?;

                v
            })
            .expect("Failed to insert visual");

        let geometry = Self::create_geometry(title_text.width, title_text.height, ctx)?;
        let bg = {
            let shape = ctx.compositor().CreateSpriteShapeWithGeometry(&geometry)?;
            shape.SetFillBrush(&ctx.common().tab_base_brush)?;

            let v = ctx.compositor().CreateShapeVisual()?;
            v.Shapes()?.Append(&shape)?;
            v.SetSize(view_size.clone())?;
            v
        };
        let active_overlay = {
            let shape = ctx.compositor().CreateSpriteShapeWithGeometry(&geometry)?;
            shape.SetFillBrush(&ctx.common().tab_active_overlay_brush)?;

            let v = ctx.compositor().CreateShapeVisual()?;
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

        Ok(new_cyclic_shared_mut(|wthis| {
            let ht_self = HitTestTree::new(
                &Rc::new(wthis.clone()),
                ctx.hittest_context_mut().new_id(),
                Rect::from_size(view_size.X, view_size.Y),
            );

            Self {
                group_view: empty_weak_mut(),
                index_in_group: 0,
                label: title,
                visual: base,
                bg_visual: bg,
                active_overlay_visual: active_overlay,
                label_content_brush,
                bg_hover_animation: ctx.common().tab_hover_animation.clone(),
                bg_hover_end_animation: ctx.common().tab_hover_end_animation.clone(),
                active_overlay_enter_animation: ctx
                    .common()
                    .tab_active_overlay_enter_animation
                    .clone(),
                active_overlay_leave_animation: ctx
                    .common()
                    .tab_active_overlay_leave_animation
                    .clone(),
                hittest_tree_self: ht_self,
                bg_active: init_active,
                is_active: init_active,
                width: view_size.X,
                height: view_size.Y,
                drag_base_point: None,
                preview_rect: Rect {
                    X: 0.0,
                    Y: 0.0,
                    Width: view_size.X,
                    Height: view_size.Y,
                },
            }
        }))
    }
    pub fn bind_group_view(
        &mut self,
        group_view: &SharedMut<TabGroupPaneView>,
        index_in_group: usize,
    ) {
        self.group_view = Rc::downgrade(group_view);
        self.index_in_group = index_in_group;
    }

    fn mount(
        &self,
        onto: &VisualCollection,
        onto_ht: &SharedMut<HitTestTree>,
    ) -> windows::core::Result<()> {
        onto.InsertAtTop(&self.visual)?;
        HitTestTree::add_child(onto_ht, self.hittest_tree_self.clone());

        Ok(())
    }
    fn unmount(&self) -> windows::core::Result<()> {
        self.visual.Parent()?.Children()?.Remove(&self.visual)?;
        let ht_ref = self.hittest_tree_self.borrow();
        if let Some(ht_parent) = ht_ref.parent.upgrade() {
            drop(ht_ref);
            ht_parent.borrow_mut().remove_child(&self.hittest_tree_self);
        }

        Ok(())
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
    fn activate_bg_imm(&mut self) -> windows::core::Result<()> {
        if self.bg_active {
            return Ok(());
        }

        self.bg_visual.SetOpacity(1.0)?;
        self.bg_active = true;
        Ok(())
    }
    fn deactivate_bg_imm(&mut self) -> windows::core::Result<()> {
        if !self.bg_active {
            return Ok(());
        }

        if self.is_active {
            // アクティブ状態のときは非アクティブにできない
            return Ok(());
        }

        self.bg_visual.SetOpacity(0.0)?;
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
        view_ctx: &mut impl ViewContext,
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
            let font = if is_active {
                view_ctx.common().tab_active_title_font.clone()
            } else {
                view_ctx.common().tab_title_font.clone()
            };
            let new_label_surface = view_ctx
                .text_surface_stock_mut()
                .get(&font, self.label.clone())?;
            self.label_content_brush
                .SetSurface(&new_label_surface.surface)?;
        }

        Ok(())
    }

    pub fn set_active_imm(
        &mut self,
        is_active: bool,
        view_ctx: &mut (impl ViewContext + ?Sized),
    ) -> windows::core::Result<()> {
        let requires_transition = self.is_active != is_active;
        self.is_active = is_active;

        if self.is_active {
            self.activate_bg_imm()?;
        } else {
            self.deactivate_bg_imm()?;
        }

        if requires_transition {
            self.active_overlay_visual
                .SetOpacity(if is_active { 1.0 } else { 0.0 })?;
            let font = if is_active {
                view_ctx.common().tab_active_title_font.clone()
            } else {
                view_ctx.common().tab_title_font.clone()
            };
            let new_label_surface = view_ctx
                .text_surface_stock_mut()
                .get(&font, self.label.clone())?;
            self.label_content_brush
                .SetSurface(&new_label_surface.surface)?;
        }

        Ok(())
    }
}
impl InputEventHandler for WeakMut<PaneTabHeaderView> {
    fn on_pointer_enter(&self, _ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };

        this.borrow_mut()
            .activate_bg()
            .expect("Failed to activate bg");
    }
    fn on_pointer_leave(&self, _ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };

        this.borrow_mut()
            .deactivate_bg()
            .expect("Failed to deactivate bg");
    }
    fn on_click(&self, ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };

        // Note: selfを借りっぱなしにしないためにいったん切り出す
        let Some(g) = this.borrow().group_view.upgrade() else {
            return;
        };
        let index = this.borrow().index_in_group;

        g.borrow_mut()
            .switch_active(index, ctx)
            .expect("Failed to transition");
    }

    fn on_begin_drag(&self, x: f32, y: f32, window: HWND, ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };
        let Some(group_view) = this.borrow().group_view.upgrade() else {
            return;
        };
        let Some(docking_manager) = group_view.borrow().docking_manager.upgrade() else {
            return;
        };

        let HitTestTree { rect, .. } = *group_view.borrow_mut().ht_ref.borrow();

        let app_window = AppWindow::wrap(window);

        let mut thisref = this.borrow_mut();
        thisref.drag_base_point = Some((x, y, rect.X, rect.Y));
        thisref.preview_rect = rect.clone();
        docking_manager
            .borrow()
            .show_preview_at(app_window.dip_rect_to_desktop_pixels_rect(&rect))
            .expect("Failed to show floating preview");

        ctx.capture_mouse();
    }
    fn on_drag_move(&self, x: f32, y: f32, window: HWND, _ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };
        let Some((bx, by, ox, oy)) = this.borrow().drag_base_point else {
            return;
        };
        let Some(group_view) = this.borrow().group_view.upgrade() else {
            return;
        };
        let Some(docking_manager) = group_view.borrow().docking_manager.upgrade() else {
            return;
        };

        let window = AppWindow::wrap(window);
        let recommended_dest = docking_manager
            .borrow()
            .compute_recommended_docking_destination(
                RedockingMode::Tab,
                window.pixels_to_dip(x),
                window.pixels_to_dip(y),
            );
        let preview_rect = this.borrow().preview_rect.clone();
        let new_rect = recommended_dest
            .dock_rect(&preview_rect)
            .unwrap_or_else(|| Rect {
                X: ox + window.pixels_to_dip(x - bx),
                Y: oy + window.pixels_to_dip(y - by),
                ..preview_rect.clone()
            });
        docking_manager
            .borrow()
            .set_preview_rect(window.dip_rect_to_desktop_pixels_rect(&new_rect))
            .expect("Failed to update preview rect");
    }
    fn on_end_drag(&self, x: f32, y: f32, window: HWND, ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };
        let Some(group_view) = this.borrow().group_view.upgrade() else {
            return;
        };
        let Some(docking_manager) = group_view.borrow().docking_manager.upgrade() else {
            return;
        };

        let window = AppWindow::wrap(window);
        // TODO: tab group changing
        let recommended_dest = docking_manager
            .borrow()
            .compute_recommended_docking_destination(
                RedockingMode::Tab,
                window.pixels_to_dip(x),
                window.pixels_to_dip(y),
            );
        match recommended_dest {
            PaneDockingRecommendation::Left(d) => {
                let bound_dock_layer = group_view.borrow().bound_dock_layer.upgrade().unwrap();
                if !Rc::ptr_eq(&bound_dock_layer, &d) || group_view.borrow().tabs.len() != 1 {
                    let new_group_view = group_view
                        .borrow_mut()
                        .split_tab(&this, ctx)
                        .expect("Failed to split group view")
                        .expect("corrupted relationship");
                    new_group_view.borrow_mut().rearrange();

                    if group_view.borrow().tabs.is_empty() {
                        // destroy group view
                        let relayout_root = PaneDockLayer::undock(&bound_dock_layer);
                        group_view
                            .borrow()
                            .unmount()
                            .expect("Failed to unmount group view");
                        relayout_root
                            .borrow_mut()
                            .relayout()
                            .expect("Failed to relayout docks");
                    } else {
                        group_view.borrow_mut().rearrange();
                    }

                    let dest_parent = d
                        .borrow()
                        .parent()
                        .expect("Docking on root?")
                        .upgrade()
                        .expect("Parent has gone?");
                    let new_layer = PaneDockLayer::new_on_left(
                        &mut *ctx,
                        &Rc::downgrade(&dest_parent),
                        |parent, _ctx| {
                            let new_filled = PaneDockLayer::new_filled(&new_group_view, parent);
                            docking_manager
                                .borrow()
                                .mount_filled(&new_filled.borrow())
                                .expect("Failed to mount new dock state");
                            new_filled
                        },
                        |parent, _ctx| {
                            d.borrow_mut().reparent(parent);
                            d.clone()
                        },
                    )
                    .expect("Failed to create new dock layer");
                    docking_manager
                        .borrow()
                        .mount_splitter_only(&new_layer.borrow())
                        .expect("Failed to mount new splitter");
                    dest_parent.borrow_mut().replace_child(&d, &new_layer);
                    dest_parent
                        .borrow_mut()
                        .relayout()
                        .expect("Failed to relayout from new parent");
                }
            }
            PaneDockingRecommendation::Right(d) => {
                let bound_dock_layer = group_view.borrow().bound_dock_layer.upgrade().unwrap();
                if !Rc::ptr_eq(&bound_dock_layer, &d) || group_view.borrow().tabs.len() != 1 {
                    let new_group_view = group_view
                        .borrow_mut()
                        .split_tab(&this, ctx)
                        .expect("Failed to split group view")
                        .expect("corrupted relationship");
                    new_group_view.borrow_mut().rearrange();

                    if group_view.borrow().tabs.is_empty() {
                        // destroy group view
                        let relayout_root = PaneDockLayer::undock(&bound_dock_layer);
                        group_view
                            .borrow()
                            .unmount()
                            .expect("Failed to unmount group view");
                        relayout_root
                            .borrow_mut()
                            .relayout()
                            .expect("Failed to relayout docks");
                    } else {
                        group_view.borrow_mut().rearrange();
                    }

                    let dest_parent = d
                        .borrow()
                        .parent()
                        .expect("Docking on root?")
                        .upgrade()
                        .expect("Parent has gone?");
                    let new_layer = PaneDockLayer::new_on_right(
                        &mut *ctx,
                        &Rc::downgrade(&dest_parent),
                        |parent, _ctx| {
                            let new_filled = PaneDockLayer::new_filled(&new_group_view, parent);
                            docking_manager
                                .borrow()
                                .mount_filled(&new_filled.borrow())
                                .expect("Failed to mount new dock state");
                            new_filled
                        },
                        |parent, _ctx| {
                            d.borrow_mut().reparent(parent);
                            d.clone()
                        },
                    )
                    .expect("Failed to create new dock layer");
                    docking_manager
                        .borrow()
                        .mount_splitter_only(&new_layer.borrow())
                        .expect("Failed to mount new splitter");
                    dest_parent.borrow_mut().replace_child(&d, &new_layer);
                    dest_parent
                        .borrow_mut()
                        .relayout()
                        .expect("Failed to relayout from new parent");
                }
            }
            PaneDockingRecommendation::Top(d) => {
                let bound_dock_layer = group_view.borrow().bound_dock_layer.upgrade().unwrap();
                if !Rc::ptr_eq(&bound_dock_layer, &d) || group_view.borrow().tabs.len() != 1 {
                    let new_group_view = group_view
                        .borrow_mut()
                        .split_tab(&this, ctx)
                        .expect("Failed to split group view")
                        .expect("corrupted relationship");
                    new_group_view.borrow_mut().rearrange();

                    if group_view.borrow().tabs.is_empty() {
                        // destroy group view
                        let relayout_root = PaneDockLayer::undock(&bound_dock_layer);
                        group_view
                            .borrow()
                            .unmount()
                            .expect("Failed to unmount group view");
                        relayout_root
                            .borrow_mut()
                            .relayout()
                            .expect("Failed to relayout docks");
                    } else {
                        group_view.borrow_mut().rearrange();
                    }

                    let dest_parent = d
                        .borrow()
                        .parent()
                        .expect("Docking on root?")
                        .upgrade()
                        .expect("Parent has gone?");
                    let new_layer = PaneDockLayer::new_on_top(
                        &mut *ctx,
                        &Rc::downgrade(&dest_parent),
                        |parent, _ctx| {
                            let new_filled = PaneDockLayer::new_filled(&new_group_view, parent);
                            docking_manager
                                .borrow()
                                .mount_filled(&new_filled.borrow())
                                .expect("Failed to mount new dock state");
                            new_filled
                        },
                        |parent, _ctx| {
                            d.borrow_mut().reparent(parent);
                            d.clone()
                        },
                    )
                    .expect("Failed to create new dock layer");
                    docking_manager
                        .borrow()
                        .mount_splitter_only(&new_layer.borrow())
                        .expect("Failed to mount new splitter");
                    dest_parent.borrow_mut().replace_child(&d, &new_layer);
                    dest_parent
                        .borrow_mut()
                        .relayout()
                        .expect("Failed to relayout from new parent");
                }
            }
            PaneDockingRecommendation::Bottom(d) => {
                let bound_dock_layer = group_view.borrow().bound_dock_layer.upgrade().unwrap();
                if !Rc::ptr_eq(&bound_dock_layer, &d) || group_view.borrow().tabs.len() != 1 {
                    let new_group_view = group_view
                        .borrow_mut()
                        .split_tab(&this, ctx)
                        .expect("Failed to split group view")
                        .expect("corrupted relationship");
                    new_group_view.borrow_mut().rearrange();

                    if group_view.borrow().tabs.is_empty() {
                        // destroy group view
                        let relayout_root = PaneDockLayer::undock(&bound_dock_layer);
                        group_view
                            .borrow()
                            .unmount()
                            .expect("Failed to unmount group view");
                        relayout_root
                            .borrow_mut()
                            .relayout()
                            .expect("Failed to relayout docks");
                    } else {
                        group_view.borrow_mut().rearrange();
                    }

                    let dest_parent = d
                        .borrow()
                        .parent()
                        .expect("Docking on root?")
                        .upgrade()
                        .expect("Parent has gone?");
                    let new_layer = PaneDockLayer::new_on_bottom(
                        &mut *ctx,
                        &Rc::downgrade(&dest_parent),
                        |parent, _ctx| {
                            let new_filled = PaneDockLayer::new_filled(&new_group_view, parent);
                            docking_manager
                                .borrow()
                                .mount_filled(&new_filled.borrow())
                                .expect("Failed to mount new dock state");
                            new_filled
                        },
                        |parent, _ctx| {
                            d.borrow_mut().reparent(parent);
                            d.clone()
                        },
                    )
                    .expect("Failed to create new dock layer");
                    docking_manager
                        .borrow()
                        .mount_splitter_only(&new_layer.borrow())
                        .expect("Failed to mount new splitter");
                    dest_parent.borrow_mut().replace_child(&d, &new_layer);
                    dest_parent
                        .borrow_mut()
                        .relayout()
                        .expect("Failed to relayout from new parent");
                }
            }
            PaneDockingRecommendation::Free => {
                let bound_dock_layer = group_view.borrow().bound_dock_layer.upgrade().unwrap();
                let relayout_root = PaneDockLayer::undock(&bound_dock_layer);

                group_view
                    .borrow()
                    .unmount()
                    .expect("Failed to unmount group view");
                let relayout_rect = relayout_root.borrow().controlling_rect();
                relayout_root
                    .borrow_mut()
                    .layout(relayout_rect)
                    .expect("Failed to relayout docks");

                println!("TODO: floating");
            }
            PaneDockingRecommendation::MergeGroup(target_group) => {
                let bound_dock_layer = group_view.borrow().bound_dock_layer.upgrade().unwrap();
                group_view
                    .borrow_mut()
                    .move_tab_into(&this, &target_group, ctx)
                    .expect("Failed to move tab");
                target_group.borrow_mut().rearrange();

                if group_view.borrow().tabs.is_empty() {
                    // destroy group view
                    let relayout_root = PaneDockLayer::undock(&bound_dock_layer);
                    group_view
                        .borrow()
                        .unmount()
                        .expect("Failed to unmount group view");
                    relayout_root
                        .borrow_mut()
                        .relayout()
                        .expect("Failed to relayout docks");
                } else {
                    group_view.borrow_mut().rearrange();
                }
            }
        }

        docking_manager
            .borrow()
            .hide_preview()
            .expect("Failed to show floating preview");
        ctx.release_mouse_capture();
    }
}

pub trait PaneTabContentPresenter {
    fn build_content_view(
        &mut self,
        onto: &ContainerVisual,
        onto_ht: &SharedMut<HitTestTree>,
        view_context: &mut dyn ViewContext,
    ) -> windows::core::Result<()>;
    fn on_hide_content_view(
        &mut self,
        view_context: &mut dyn ViewContext,
    ) -> windows::core::Result<()>;
}
pub trait PaneTabPresenter: PaneTabContentPresenter + Sized {
    const INIT_TAB_NAME: &'static str;
    fn new(_tab_header_view: &SharedMut<PaneTabHeaderView>) -> Self;
}

pub struct InspectorTabPresenter {}
impl PaneTabContentPresenter for InspectorTabPresenter {
    fn build_content_view(
        &mut self,
        onto: &ContainerVisual,
        _onto_ht: &SharedMut<HitTestTree>,
        view_context: &mut dyn ViewContext,
    ) -> windows::core::Result<()> {
        let ui_font = view_context.text_format_stock_mut().get(
            "system-ui",
            12.0,
            DWRITE_FONT_WEIGHT_NORMAL,
        )?;
        let label_surface = view_context
            .text_surface_stock_mut()
            .get(&ui_font, "Inspector Pane")?;
        let brush = view_context
            .compositor()
            .CreateSurfaceBrushWithSurface(&label_surface.surface)?;
        let label_visual = view_context.compositor().CreateSpriteVisual()?;
        label_visual.SetBrush(&brush)?;
        label_visual.SetSize(label_surface.visual_size())?;
        onto.Children()?.InsertAtTop(&label_visual)?;

        Ok(())
    }

    fn on_hide_content_view(
        &mut self,
        _view_context: &mut dyn ViewContext,
    ) -> windows::core::Result<()> {
        Ok(())
    }
}
impl PaneTabPresenter for InspectorTabPresenter {
    const INIT_TAB_NAME: &'static str = "Inspector";

    fn new(_tab_header_view: &SharedMut<PaneTabHeaderView>) -> Self {
        Self {}
    }
}

pub struct ProjectSettingsTabPresenter {}
impl PaneTabContentPresenter for ProjectSettingsTabPresenter {
    fn build_content_view(
        &mut self,
        _onto: &ContainerVisual,
        _onto_ht: &SharedMut<HitTestTree>,
        _view_context: &mut dyn ViewContext,
    ) -> windows::core::Result<()> {
        Ok(())
    }

    fn on_hide_content_view(
        &mut self,
        _view_context: &mut dyn ViewContext,
    ) -> windows::core::Result<()> {
        Ok(())
    }
}
impl PaneTabPresenter for ProjectSettingsTabPresenter {
    const INIT_TAB_NAME: &'static str = "Project Settings";

    fn new(_tab_header_view: &SharedMut<PaneTabHeaderView>) -> Self {
        Self {}
    }
}

pub struct TimelineTabPresenter {}
impl PaneTabContentPresenter for TimelineTabPresenter {
    fn build_content_view(
        &mut self,
        _onto: &ContainerVisual,
        _onto_ht: &SharedMut<HitTestTree>,
        _view_context: &mut dyn ViewContext,
    ) -> windows::core::Result<()> {
        Ok(())
    }

    fn on_hide_content_view(
        &mut self,
        _view_context: &mut dyn ViewContext,
    ) -> windows::core::Result<()> {
        Ok(())
    }
}
impl PaneTabPresenter for TimelineTabPresenter {
    const INIT_TAB_NAME: &'static str = "Timeline";

    fn new(_tab_header_view: &SharedMut<PaneTabHeaderView>) -> Self {
        Self {}
    }
}

pub struct StageTabPresenter {}
impl PaneTabContentPresenter for StageTabPresenter {
    fn build_content_view(
        &mut self,
        _onto: &ContainerVisual,
        _onto_ht: &SharedMut<HitTestTree>,
        _view_context: &mut dyn ViewContext,
    ) -> windows::core::Result<()> {
        Ok(())
    }

    fn on_hide_content_view(
        &mut self,
        _view_context: &mut dyn ViewContext,
    ) -> windows::core::Result<()> {
        Ok(())
    }
}
impl PaneTabPresenter for StageTabPresenter {
    const INIT_TAB_NAME: &'static str = "Stage";

    fn new(_tab_header_view: &SharedMut<PaneTabHeaderView>) -> Self {
        Self {}
    }
}

pub struct PreviewTabPresenter {}
impl PaneTabContentPresenter for PreviewTabPresenter {
    fn build_content_view(
        &mut self,
        _onto: &ContainerVisual,
        _onto_ht: &SharedMut<HitTestTree>,
        _view_context: &mut dyn ViewContext,
    ) -> windows::core::Result<()> {
        Ok(())
    }

    fn on_hide_content_view(
        &mut self,
        _view_context: &mut dyn ViewContext,
    ) -> windows::core::Result<()> {
        Ok(())
    }
}
impl PaneTabPresenter for PreviewTabPresenter {
    const INIT_TAB_NAME: &'static str = "Preview";

    fn new(_tab_header_view: &SharedMut<PaneTabHeaderView>) -> Self {
        Self {}
    }
}

pub enum InputAction {
    PointerLeave(Rc<dyn InputEventHandler>),
    PointerEnter(Rc<dyn InputEventHandler>),
    PointerDown(Rc<dyn InputEventHandler>),
    PointerUp(Rc<dyn InputEventHandler>),
    Click(Rc<dyn InputEventHandler>),
    BeginDrag(Rc<dyn InputEventHandler>),
    DragMove(Rc<dyn InputEventHandler>),
    EndDrag(Rc<dyn InputEventHandler>),
}
impl InputAction {
    #[inline]
    pub fn execute(self, x: f32, y: f32, mut ctx: &mut dyn InputContext, window: HWND) {
        match self {
            Self::PointerLeave(e) => e.on_pointer_leave(ctx),
            Self::PointerEnter(e) => e.on_pointer_enter(ctx),
            Self::PointerDown(e) => e.on_pointer_down(x, y, ctx),
            Self::PointerUp(e) => e.on_pointer_up(x, y, ctx),
            Self::Click(e) => e.on_click(&mut ctx),
            Self::BeginDrag(e) => e.on_begin_drag(x, y, window, ctx),
            Self::DragMove(e) => e.on_drag_move(x, y, window, ctx),
            Self::EndDrag(e) => e.on_end_drag(x, y, window, ctx),
        }
    }
}

const DRAG_THRESHOLD_DIST2: f32 = 5.0 * 5.0;
struct InputState {
    bound_window: HWND,
    ht_tree: SharedMut<HitTestTree>,
    mouse_capturing_element: Option<WeakMut<HitTestTree>>,
    mouse_current_enter_element: Option<WeakMut<HitTestTree>>,
    mouse_down_point: Option<(f32, f32, Option<WeakMut<HitTestTree>>)>,
    is_mouse_dragging: bool,
}
impl InputState {
    fn new(bound_window: HWND, ht_tree: &SharedMut<HitTestTree>) -> Self {
        Self {
            bound_window,
            ht_tree: ht_tree.clone(),
            mouse_capturing_element: None,
            mouse_current_enter_element: None,
            mouse_down_point: None,
            is_mouse_dragging: false,
        }
    }

    fn update_mouse_pos(&mut self, x: f32, y: f32, actions: &mut Vec<InputAction>) {
        let over_tree = HitTestTree::check(&self.ht_tree, x, y);
        let over_changes = over_tree.as_ref().map(|x| x.borrow().id)
            != self
                .mouse_current_enter_element
                .as_ref()
                .and_then(Weak::upgrade)
                .map(|x| x.borrow().id);
        if let Some(x) = self
            .mouse_current_enter_element
            .as_ref()
            .and_then(Weak::upgrade)
        {
            if Some(x.borrow().id) != over_tree.as_ref().map(|x| x.borrow().id) {
                // leave
                actions.push(InputAction::PointerLeave(x.borrow().eh.clone()));
            }
        }
        self.mouse_current_enter_element = over_tree.as_ref().map(Rc::downgrade);
        if over_changes {
            if let Some(x) = self
                .mouse_current_enter_element
                .as_ref()
                .and_then(Weak::upgrade)
            {
                actions.push(InputAction::PointerEnter(x.borrow().eh.clone()));
            }
        }
    }

    pub fn capture_mouse(&mut self) {
        if self.mouse_current_enter_element.is_none() {
            return;
        }

        self.mouse_capturing_element = self.mouse_current_enter_element.clone();
        unsafe {
            SetCapture(self.bound_window);
        }
    }

    pub fn release_mouse_capture(&mut self) {
        if self.mouse_capturing_element.is_none() {
            return;
        }

        unsafe {
            ReleaseCapture().expect("Failed to release captured mouse");
        }
        self.mouse_capturing_element = None;
    }

    fn on_mouse_move(&mut self, x: f32, y: f32) -> Vec<InputAction> {
        let mut actions = Vec::with_capacity(16);

        if let Some(e) = self
            .mouse_capturing_element
            .as_ref()
            .and_then(Weak::upgrade)
        {
            if let Some((dx, dy, _)) = self.mouse_down_point.as_ref() {
                if !self.is_mouse_dragging {
                    // 閾値を超えた後は永続的にドラッグ状態になる
                    let dist2 = (dx - x).powi(2) + (dy - y).powi(2);
                    if dist2 >= DRAG_THRESHOLD_DIST2 {
                        self.is_mouse_dragging = true;
                        actions.push(InputAction::BeginDrag(e.borrow().eh.clone()));
                    }
                }

                if self.is_mouse_dragging {
                    actions.push(InputAction::DragMove(e.borrow().eh.clone()));
                }
            }

            return actions;
        }

        self.update_mouse_pos(x, y, &mut actions);

        if let Some((dx, dy, down_element)) = self.mouse_down_point.as_ref() {
            if !self.is_mouse_dragging {
                // 閾値を超えた後は永続的にドラッグ状態になる
                let dist2 = (dx - x).powi(2) + (dy - y).powi(2);
                if dist2 >= DRAG_THRESHOLD_DIST2 {
                    self.is_mouse_dragging = true;
                    if let Some(e) = down_element.as_ref().and_then(Weak::upgrade) {
                        actions.push(InputAction::BeginDrag(e.borrow().eh.clone()));
                    }
                }
            }

            if self.is_mouse_dragging {
                if let Some(e) = down_element.as_ref().and_then(Weak::upgrade) {
                    actions.push(InputAction::DragMove(e.borrow().eh.clone()));
                }
            }
        }

        actions
    }

    fn on_mouse_down(&mut self, x: f32, y: f32) -> Vec<InputAction> {
        let mut actions = Vec::with_capacity(16);

        if let Some(e) = self
            .mouse_capturing_element
            .as_ref()
            .and_then(Weak::upgrade)
        {
            actions.push(InputAction::PointerDown(e.borrow().eh.clone()));
            return actions;
        }

        self.update_mouse_pos(x, y, &mut actions);
        self.mouse_down_point = Some((x, y, self.mouse_current_enter_element.clone()));
        self.is_mouse_dragging = false;
        if let Some(e) = self
            .mouse_current_enter_element
            .as_ref()
            .and_then(Weak::upgrade)
        {
            actions.push(InputAction::PointerDown(e.borrow().eh.clone()));
        }

        actions
    }

    fn on_mouse_up(&mut self, x: f32, y: f32) -> Vec<InputAction> {
        let mut actions = Vec::with_capacity(16);

        if let Some(e) = self
            .mouse_capturing_element
            .as_ref()
            .and_then(Weak::upgrade)
        {
            actions.push(InputAction::PointerUp(e.borrow().eh.clone()));
            if !self.is_mouse_dragging {
                actions.push(InputAction::Click(e.borrow().eh.clone()));
            } else {
                actions.push(InputAction::EndDrag(e.borrow().eh.clone()));
            }
            self.mouse_down_point = None;

            return actions;
        }

        self.update_mouse_pos(x, y, &mut actions);

        if !self.is_mouse_dragging {
            if let Some(x) = self
                .mouse_current_enter_element
                .as_ref()
                .and_then(Weak::upgrade)
            {
                actions.push(InputAction::Click(x.borrow().eh.clone()));
            }
        } else {
            if let Some(x) = self
                .mouse_down_point
                .as_ref()
                .and_then(|x| x.2.as_ref())
                .and_then(std::rc::Weak::upgrade)
            {
                actions.push(InputAction::EndDrag(x.borrow().eh.clone()));
            }
        }
        self.mouse_down_point = None;

        actions
    }

    fn set_cursor(&self) -> bool {
        if let Some(e) = self
            .mouse_capturing_element
            .as_ref()
            .and_then(Weak::upgrade)
        {
            // TODO: caching loaded cursors
            let c = match e.borrow().eh.hover_cursor() {
                CursorStyle::Arrow => unsafe {
                    LoadCursorA(None, core::mem::transmute::<_, PCSTR>(IDC_ARROW))
                },
                CursorStyle::SizeNS => unsafe {
                    LoadCursorA(None, core::mem::transmute::<_, PCSTR>(IDC_SIZENS))
                },
                CursorStyle::SizeEW => unsafe {
                    LoadCursorA(None, core::mem::transmute::<_, PCSTR>(IDC_SIZEWE))
                },
            };
            unsafe { SetCursor(c.expect("Failed to load cursor")) };

            return true;
        }

        if let Some(e) = self
            .mouse_current_enter_element
            .as_ref()
            .and_then(Weak::upgrade)
        {
            // TODO: caching loaded cursors
            let c = match e.borrow().eh.hover_cursor() {
                CursorStyle::Arrow => unsafe {
                    LoadCursorA(None, core::mem::transmute::<_, PCSTR>(IDC_ARROW))
                },
                CursorStyle::SizeNS => unsafe {
                    LoadCursorA(None, core::mem::transmute::<_, PCSTR>(IDC_SIZENS))
                },
                CursorStyle::SizeEW => unsafe {
                    LoadCursorA(None, core::mem::transmute::<_, PCSTR>(IDC_SIZEWE))
                },
            };
            unsafe { SetCursor(c.expect("Failed to load cursor")) };

            true
        } else {
            false
        }
    }
}

pub struct HitTestTree {
    eh: Rc<dyn InputEventHandler>,
    id: usize,
    rect: Rect,
    parent: WeakMut<HitTestTree>,
    children: HashMap<usize, SharedMut<HitTestTree>>,
}
impl HitTestTree {
    #[inline]
    pub fn new(
        eh: &Rc<impl InputEventHandler + 'static>,
        id: usize,
        rect: Rect,
    ) -> SharedMut<Self> {
        new_shared_mut(Self {
            eh: eh.clone(),
            id,
            rect,
            parent: empty_weak_mut(),
            children: HashMap::new(),
        })
    }
    #[inline]
    pub fn new_unsized(
        eh: &Rc<impl InputEventHandler + 'static>,
        id: usize,
        left: f32,
        top: f32,
    ) -> SharedMut<Self> {
        Self::new(
            eh,
            id,
            Rect {
                X: left,
                Y: top,
                Width: f32::MAX,
                Height: f32::MAX,
            },
        )
    }

    #[inline]
    pub fn add_child(this: &SharedMut<Self>, child: SharedMut<HitTestTree>) {
        child.borrow_mut().parent = Rc::downgrade(this);
        let cid = child.borrow().id;
        this.borrow_mut().children.insert(cid, child);
    }

    #[inline]
    pub fn remove_child(&mut self, child: &SharedMut<HitTestTree>) {
        let cb = child.borrow();
        self.children.remove(&cb.id);
        drop(cb);
        child.borrow_mut().parent = Weak::new();
    }

    #[inline]
    pub fn set_rect(&mut self, left: f32, top: f32, width: f32, height: f32) {
        self.rect = Rect {
            X: left,
            Y: top,
            Width: width,
            Height: height,
        };
    }
    #[inline]
    pub fn set_size(&mut self, width: f32, height: f32) {
        self.rect.Width = width;
        self.rect.Height = height;
    }
    #[inline]
    pub fn set_offset(&mut self, left: f32, top: f32) {
        self.rect.X = left;
        self.rect.Y = top;
    }

    pub fn check(this: &SharedMut<Self>, x: f32, y: f32) -> Option<SharedMut<Self>> {
        let this1 = this.borrow();
        if this1.rect.contains_point(x, y) {
            let child = this1
                .children
                .values()
                .find_map(|c| Self::check(c, x - this1.rect.X, y - this1.rect.Y));
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
            .field("left", &self.rect.X)
            .field("top", &self.rect.Y)
            .field("width", &self.rect.Width)
            .field("height", &self.rect.Height)
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

struct AppWindowState<'r> {
    input_state: InputState,
    compositor: Compositor,
    ui_common_objects: &'r UICommonObjects,
    text_format_stock: &'r mut TextFormatStock,
    text_surface_stock: &'r mut TextSurfaceStock,
    hittest_context: HitTestTreeContext,
    pane_group_docking_manager: SharedMut<PaneGroupDockingManager>,
}
impl ViewContext for AppWindowState<'_> {
    fn compositor(&self) -> &windows::UI::Composition::Compositor {
        &self.compositor
    }

    fn common(&self) -> &UICommonObjects {
        &self.ui_common_objects
    }

    fn text_format_stock_mut(&mut self) -> &mut TextFormatStock {
        self.text_format_stock
    }

    fn text_surface_stock_mut(&mut self) -> &mut TextSurfaceStock {
        self.text_surface_stock
    }

    fn hittest_context_mut(&mut self) -> &mut HitTestTreeContext {
        &mut self.hittest_context
    }
}
impl InputContext for AppWindowState<'_> {
    fn capture_mouse(&mut self) {
        self.input_state.capture_mouse();
    }

    fn release_mouse_capture(&mut self) {
        self.input_state.release_mouse_capture();
    }
}
struct AppWindow {
    handle: HWND,
    current_dpi: f32,
}
impl AppWindow {
    pub const WINDOW_EXTRA_SIZE: usize = core::mem::size_of::<usize>();
    const STATE_STORE_PTR_INDEX: WINDOW_LONG_PTR_INDEX = WINDOW_LONG_PTR_INDEX(0);

    #[inline]
    fn register_window_class() -> windows::core::Result<u16> {
        let cls = WNDCLASSEXA {
            cbSize: core::mem::size_of::<WNDCLASSEXA>() as _,
            cbClsExtra: 0,
            cbWndExtra: Self::WINDOW_EXTRA_SIZE as _,
            style: WNDCLASS_STYLES(0),
            lpfnWndProc: Some(window_proc),
            hInstance: unsafe { GetModuleHandleA(None)?.into() },
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

        register_window_class(&cls)
    }

    #[inline]
    pub fn wrap(handle: HWND) -> Self {
        Self {
            handle,
            current_dpi: unsafe { GetDpiForWindow(handle) as _ },
        }
    }

    #[inline]
    pub fn set_state_store(&mut self, state_ref: &mut AppWindowState) {
        unsafe {
            SetWindowLongPtrA(
                self.handle,
                Self::STATE_STORE_PTR_INDEX,
                state_ref as *mut _ as _,
            );
        }
    }

    #[inline]
    pub fn get_state_store(&self) -> Option<&mut AppWindowState> {
        unsafe {
            (GetWindowLongPtrA(self.handle, Self::STATE_STORE_PTR_INDEX) as *mut AppWindowState)
                .as_mut()
        }
    }

    #[inline]
    pub fn pixels_to_dip(&self, pixels: f32) -> f32 {
        pixels * 96.0 / self.current_dpi
    }
    #[inline]
    pub fn dip_to_pixels(&self, dip: f32) -> f32 {
        dip * self.current_dpi / 96.0
    }

    #[inline]
    pub fn map_points_to_desktop(&self, points: &mut [POINT]) {
        unsafe {
            MapWindowPoints(self.handle, None, points);
        }
    }

    pub fn dip_rect_to_desktop_pixels_rect(&self, dip_rect: &Rect) -> Rect {
        let mut loc = [POINT {
            x: self.dip_to_pixels(dip_rect.X) as _,
            y: self.dip_to_pixels(dip_rect.Y) as _,
        }];
        self.map_points_to_desktop(&mut loc);

        Rect {
            X: loc[0].x as _,
            Y: loc[0].y as _,
            Width: self.dip_to_pixels(dip_rect.Width),
            Height: self.dip_to_pixels(dip_rect.Height),
        }
    }

    #[inline]
    pub fn client_size_pixels(&self) -> windows::core::Result<(u32, u32)> {
        let mut sink = core::mem::MaybeUninit::<windows::Win32::Foundation::RECT>::uninit();
        unsafe { GetClientRect(self.handle, sink.as_mut_ptr())? };
        let rect = unsafe { sink.assume_init() };

        Ok((
            (rect.right - rect.left) as u32,
            (rect.bottom - rect.top) as u32,
        ))
    }

    #[inline]
    pub fn client_size(&self) -> windows::core::Result<(f32, f32)> {
        self.client_size_pixels()
            .map(|(w, h)| (self.pixels_to_dip(w as _), self.pixels_to_dip(h as _)))
    }

    #[inline]
    pub fn show(&self) {
        unsafe {
            let _ = ShowWindow(self.handle, SW_SHOWNORMAL);
        }
    }
}

fn main() {
    let instance_handle = unsafe { GetModuleHandleA(None).expect("Failed to get instance handle") };
    let window_handle = WindowBuilder::new(
        instance_handle.into(),
        AppWindow::register_window_class().expect("Failed to register window class"),
        s!("Peridot Marble Editor"),
    )
    .no_redirection_bitmap()
    .app_window()
    .overlapped_window()
    .create()
    .expect("Failed to create window");
    unsafe {
        // set dark mode preference
        let attr: BOOL = BOOL(1);
        DwmSetWindowAttribute(
            window_handle,
            DWMWINDOWATTRIBUTE(20),
            &attr as *const _ as _,
            core::mem::size_of::<BOOL>() as _,
        )
        .expect("Failed to set window attribute");
    }
    let mut window_handle = AppWindow::wrap(window_handle);

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

    let compositor = Compositor::new().expect("Failed to create ui compositor");
    let desktop_interop = compositor
        .cast::<ICompositorDesktopInterop>()
        .expect("This compositor does not support desktop interop");
    let desktop_window_target = unsafe {
        desktop_interop
            .CreateDesktopWindowTarget(window_handle.handle, false)
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
    let mut text_surface_stock = TextSurfaceStock::new(
        &dwrite_factory,
        &composition_graphics_device,
        window_handle.current_dpi,
    );

    let app_global_scale = window_handle.current_dpi as f64 / 96.0;
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

    let linear_easing_fn = compositor
        .CreateLinearEasingFunction()
        .expect("Failed to create easing function");
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
    };

    let hittest_tree_root = HitTestTree::new_unsized(&Rc::new(()), 0, 0.0, 0.0);
    let mut hittest_context = HitTestTreeContext::new();

    let mut view_context = ViewContext1 {
        compositor: &compositor,
        common: &common_objects,
        text_format_stock: &mut text_format_stock,
        text_surface_stock: &mut text_surface_stock,
        hittest_context: &mut hittest_context,
    };

    let pane_group_docking_manager = new_shared_mut(
        PaneGroupDockingManager::new(&mut view_context, &hittest_tree_root)
            .expect("Failed to initialize docking manager"),
    );

    let pane_group1 = TabGroupPaneView::new(&pane_group_docking_manager, &mut view_context)
        .expect("Failed to create TabGroupPaneView");
    TabGroupPaneView::add_tab::<TimelineTabPresenter>(&pane_group1, &mut view_context)
        .expect("Failed to create SceneViewPaneTabHeader");
    pane_group1.borrow_mut().rearrange();

    let pane_group2 = TabGroupPaneView::new(&pane_group_docking_manager, &mut view_context)
        .expect("Failed to create TabGroupPaneView");
    TabGroupPaneView::add_tab::<StageTabPresenter>(&pane_group2, &mut view_context)
        .expect("Failed to create StagePaneTab");
    TabGroupPaneView::add_tab::<PreviewTabPresenter>(&pane_group2, &mut view_context)
        .expect("Failed to create PreviewPaneTab");
    TabGroupPaneView::add_tab::<ProjectSettingsTabPresenter>(&pane_group2, &mut view_context)
        .expect("Failed to create ProjectSettingsPaneTabHeader");
    pane_group2.borrow_mut().rearrange();

    let pane_group3 = TabGroupPaneView::new(&pane_group_docking_manager, &mut view_context)
        .expect("Failed to create TabGroupPaneView");
    TabGroupPaneView::add_tab::<InspectorTabPresenter>(&pane_group3, &mut view_context)
        .expect("Failed to create InspectorPaneTabHeader");
    pane_group3.borrow_mut().rearrange();
    pane_group3
        .borrow_mut()
        .resize(256.0, 256.0)
        .expect("Failed to resize pane");

    let layout = PaneDockLayer::new_root(|parent| {
        Some(
            PaneDockLayer::new_on_right(
                &mut view_context,
                parent,
                |parent, _| PaneDockLayer::new_filled(&pane_group3, parent),
                |parent, ctx| {
                    PaneDockLayer::new_on_top(
                        ctx,
                        parent,
                        |parent, _| PaneDockLayer::new_filled(&pane_group1, parent),
                        |parent, _| PaneDockLayer::new_filled(&pane_group2, parent),
                    )
                    .expect("Failed to create pane dock state")
                },
            )
            .expect("Failed to create pane dock state"),
        )
    });
    pane_group_docking_manager
        .borrow_mut()
        .set_layout(layout)
        .expect("Failed to setup initial layout");

    composition_root
        .Children()
        .expect("Failed to get children collection")
        .InsertBelow(
            &pane_group_docking_manager.borrow().placement_visual,
            &overlay_layer,
        )
        .expect("Failed to insert placement visual");

    let (client_width, client_height) = window_handle
        .client_size()
        .expect("Failed to get initial client size");
    pane_group_docking_manager
        .borrow_mut()
        .resize_root(client_width, client_height)
        .expect("Failed to initial relayout");

    let mut ws = AppWindowState {
        input_state: InputState::new(window_handle.handle, &hittest_tree_root),
        compositor: compositor.clone(),
        ui_common_objects: &common_objects,
        text_format_stock: &mut text_format_stock,
        text_surface_stock: &mut text_surface_stock,
        hittest_context,
        pane_group_docking_manager,
    };
    window_handle.set_state_store(&mut ws);
    window_handle.show();

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
        let app_window = AppWindow::wrap(hwnd);
        let Some(state) = app_window.get_state_store() else {
            return LRESULT(0);
        };

        let (x, y) = ((lp.0 & 0xffff) as i16, ((lp.0 >> 16) & 0xffff) as i16);
        let actions = state.input_state.on_mouse_move(
            app_window.pixels_to_dip(x as _),
            app_window.pixels_to_dip(y as _),
        );
        for a in actions {
            a.execute(x as _, y as _, state, hwnd);
        }

        return LRESULT(0);
    }
    if msg == WM_LBUTTONDOWN {
        let app_window = AppWindow::wrap(hwnd);
        let Some(state) = app_window.get_state_store() else {
            return LRESULT(0);
        };

        let (x, y) = ((lp.0 & 0xffff) as i16, ((lp.0 >> 16) & 0xffff) as i16);
        let actions = state.input_state.on_mouse_down(
            app_window.pixels_to_dip(x as _),
            app_window.pixels_to_dip(y as _),
        );
        for a in actions {
            a.execute(x as _, y as _, state, hwnd);
        }

        return LRESULT(0);
    }
    if msg == WM_LBUTTONUP {
        let app_window = AppWindow::wrap(hwnd);
        let Some(state) = app_window.get_state_store() else {
            return LRESULT(0);
        };

        let (x, y) = ((lp.0 & 0xffff) as i16, ((lp.0 >> 16) & 0xffff) as i16);
        let actions = state.input_state.on_mouse_up(
            app_window.pixels_to_dip(x as _),
            app_window.pixels_to_dip(y as _),
        );
        for a in actions {
            a.execute(x as _, y as _, state, hwnd);
        }

        return LRESULT(0);
    }
    if msg == WM_WINDOWPOSCHANGED {
        let app_window = AppWindow::wrap(hwnd);
        let Some(state) = app_window.get_state_store() else {
            // not initialized
            return unsafe { DefWindowProcA(hwnd, msg, wp, lp) };
        };

        let (w, h) = app_window.client_size().expect("Failed to get client size");
        state
            .pane_group_docking_manager
            .borrow_mut()
            .resize_root(w, h)
            .expect("Failed to resize root");

        return LRESULT(0);
    }
    if msg == WM_SETCURSOR {
        if (lp.0 & 0xffff) as i16 as u32 != HTCLIENT {
            // non-client area
            return unsafe { DefWindowProcA(hwnd, msg, wp, lp) };
        }

        let app_window = AppWindow::wrap(hwnd);
        let Some(state) = app_window.get_state_store() else {
            // not initialized
            return unsafe { DefWindowProcA(hwnd, msg, wp, lp) };
        };

        if state.input_state.set_cursor() {
            return LRESULT(1);
        } else {
            return unsafe { DefWindowProcA(hwnd, msg, wp, lp) };
        }
    }

    unsafe { DefWindowProcA(hwnd, msg, wp, lp) }
}
