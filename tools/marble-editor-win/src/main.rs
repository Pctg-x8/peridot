use std::{
    borrow::Cow,
    cell::RefCell,
    ffi::c_void,
    rc::{Rc, Weak},
};

use bedrock as br;
use br::{
    CommandBuffer, CommandPool, DescriptorPool, Device, GraphicsPipelineBuilder,
    ImageSubresourceSlice, Instance, MemoryBound, PhysicalDevice, PipelineShaderStageProvider,
    Queue, RenderPass, SubmissionBatch, VulkanStructure,
};
use features::{AppTitleBarView, DockingPanePreview, PaneSplitterView, SplitDirection};
use miniengine::{ColoredVertex, Mat4, Vec4};
use object_cache::{TextFormatStock, TextSurfaceStock};
use peridot_math::{Camera, One, ProjectionMethod, Zero};
use uikit::{
    HitTestTree, HitTestTreeContext, InputContext, InputEventHandler, InputState, ViewContext,
};
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
        Foundation::{
            CloseHandle, BOOL, GENERIC_ALL, HANDLE, HWND, LPARAM, LRESULT, POINT, RECT,
            WAIT_OBJECT_0, WPARAM,
        },
        Graphics::{
            CompositionSwapchain::{
                CreatePresentationFactory, IPresentationBuffer, IPresentationFactory,
                IPresentationManager, IPresentationSurface,
            },
            Direct2D::{
                D2D1CreateFactory, ID2D1Factory1, D2D1_DEBUG_LEVEL_WARNING, D2D1_FACTORY_OPTIONS,
                D2D1_FACTORY_TYPE_SINGLE_THREADED,
            },
            Direct3D::{D3D_DRIVER_TYPE_HARDWARE, D3D_FEATURE_LEVEL, D3D_FEATURE_LEVEL_11_0},
            Direct3D11::{
                D3D11CreateDevice, ID3D11Device, ID3D11DeviceContext, ID3D11RenderTargetView,
                ID3D11Resource, ID3D11Texture2D, D3D11_BIND_RENDER_TARGET,
                D3D11_BIND_SHADER_RESOURCE, D3D11_CREATE_DEVICE_BGRA_SUPPORT,
                D3D11_RESOURCE_MISC_SHARED, D3D11_RESOURCE_MISC_SHARED_DISPLAYABLE,
                D3D11_RESOURCE_MISC_SHARED_KEYEDMUTEX, D3D11_RESOURCE_MISC_SHARED_NTHANDLE,
                D3D11_SDK_VERSION, D3D11_TEXTURE2D_DESC, D3D11_USAGE_DEFAULT,
            },
            Direct3D12::{
                D3D12CreateDevice, ID3D12CommandQueue, ID3D12Device,
                D3D12_COMMAND_LIST_TYPE_DIRECT, D3D12_COMMAND_QUEUE_DESC,
                D3D12_COMMAND_QUEUE_FLAGS,
            },
            DirectComposition::{
                DCompositionCreateSurfaceHandle, COMPOSITIONOBJECT_READ, COMPOSITIONOBJECT_WRITE,
            },
            DirectWrite::{
                DWriteCreateFactory, IDWriteFactory, DWRITE_FACTORY_TYPE_SHARED,
                DWRITE_FONT_WEIGHT_NORMAL, DWRITE_FONT_WEIGHT_SEMI_BOLD,
            },
            Dwm::{DwmExtendFrameIntoClientArea, DwmSetWindowAttribute, DWMWINDOWATTRIBUTE},
            Dxgi::{
                Common::{
                    DXGI_ALPHA_MODE_IGNORE, DXGI_COLOR_SPACE_RGB_FULL_G10_NONE_P709,
                    DXGI_FORMAT_R8G8B8A8_UNORM, DXGI_SAMPLE_DESC,
                },
                IDXGIDevice, IDXGIKeyedMutex, IDXGIResource, IDXGIResource1,
                DXGI_SHARED_RESOURCE_READ, DXGI_SHARED_RESOURCE_WRITE,
            },
            Gdi::{MapWindowPoints, HBRUSH},
        },
        Storage::Packaging::Appx::PACKAGE_VERSION,
        System::{
            LibraryLoader::{GetModuleHandleA, GetProcAddress, LoadLibraryA},
            Threading::{CreateEventA, ResetEvent, SetEvent, INFINITE},
            WinRT::{
                Composition::{ICompositorDesktopInterop, ICompositorInterop},
                CreateDispatcherQueueController, DispatcherQueueOptions, DQTAT_COM_ASTA,
                DQTYPE_THREAD_CURRENT,
            },
        },
        UI::{
            Controls::MARGINS,
            HiDpi::GetDpiForWindow,
            WindowsAndMessaging::{
                DefWindowProcA, DispatchMessageA, GetClientRect, GetSystemMetrics,
                GetWindowLongPtrA, GetWindowPlacement, GetWindowRect, LoadCursorA, LoadIconA,
                MsgWaitForMultipleObjects, PeekMessageA, PostQuitMessage, SetWindowLongPtrA,
                SetWindowPos, ShowWindow, TranslateMessage, HTCLIENT, HTTOP, IDC_ARROW,
                IDI_APPLICATION, MSG, NCCALCSIZE_PARAMS, PM_REMOVE, QS_ALLEVENTS, SM_CXSIZEFRAME,
                SM_CYSIZEFRAME, SWP_FRAMECHANGED, SW_MAXIMIZE, SW_SHOWNORMAL, WINDOWPLACEMENT,
                WINDOW_LONG_PTR_INDEX, WM_ACTIVATE, WM_CREATE, WM_DESTROY, WM_LBUTTONDOWN,
                WM_LBUTTONUP, WM_MOUSEMOVE, WM_NCCALCSIZE, WM_NCHITTEST, WM_NCMOUSEMOVE, WM_QUIT,
                WM_SETCURSOR, WM_WINDOWPOSCHANGED, WNDCLASSEXA, WNDCLASS_STYLES,
            },
        },
    },
    UI::{
        Color,
        Composition::{
            CompositionRoundedRectangleGeometry, CompositionSurfaceBrush, Compositor,
            ContainerVisual, Diagnostics::CompositionDebugSettings, LayerVisual,
            ScalarKeyFrameAnimation, ShapeVisual, SpriteVisual, VisualCollection,
        },
    },
};

use crate::{
    bindgen::UI::Composition::{
        ICompositionSupportsSystemBackdrop,
        SystemBackdrops::{
            MicaController, MicaKind, SystemBackdropConfiguration, SystemBackdropTheme,
        },
    },
    miniengine::MiniEngine,
    uikit::{UICommonObjects, ViewContext1},
    winapi_extras::{register_window_class, VectorScalarConstructor, WindowBuilder},
};

mod bindgen;
mod features;
mod miniengine;
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
    pub fn dock_rect(
        &self,
        preview_rect: &Rect,
        docking_root: &PaneGroupDockingManager,
    ) -> Option<Rect> {
        let (ox, oy) = docking_root.offset();

        match self {
            Self::Left(d) => Some(Rect {
                X: d.borrow().controlling_rect_left() + ox,
                Y: d.borrow().controlling_rect_top() + oy,
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
                    X: d.borrow().controlling_rect_right() + ox - w,
                    Y: d.borrow().controlling_rect_top() + oy,
                    Width: w,
                    Height: d.borrow().controlling_rect_height(),
                }
            }),
            Self::Top(d) => Some(Rect {
                X: d.borrow().controlling_rect_left() + ox,
                Y: d.borrow().controlling_rect_top() + oy,
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
                    X: d.borrow().controlling_rect_left() + ox,
                    Y: d.borrow().controlling_rect_bottom() + oy - h,
                    Width: d.borrow().controlling_rect_width(),
                    Height: h,
                }
            }),
            Self::MergeGroup(view) => Some(view.borrow().view_rect.clone().with_offset(ox, oy)),
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
        let ht_placement_root =
            HitTestTree::new_unsized(&Rc::new(()), ctx.hittest_context_mut().new_id(), 0.0, 0.0);
        HitTestTree::add_child(ht_root, ht_placement_root.clone());

        Ok(Self {
            docks: PaneDockLayer::new_root(|_| None),
            placement_visual: ctx.compositor().CreateContainerVisual()?,
            ht_placement_root,
            floating_preview: DockingPanePreview::new(ctx)?,
        })
    }

    fn set_layout(&mut self, layout: SharedMut<PaneDockLayer>) -> windows::core::Result<()> {
        let children = self.placement_visual.Children()?;
        children.RemoveAll()?;
        self.ht_placement_root.borrow_mut().remove_all_children();
        layout
            .borrow()
            .mount_recursive(&children, &self.ht_placement_root)?;

        self.docks = layout;
        Ok(())
    }
    fn set_offset(&self, left: f32, top: f32) -> windows::core::Result<()> {
        self.placement_visual.SetOffset(Vector3 {
            X: left,
            Y: top,
            Z: 0.0,
        })?;
        self.ht_placement_root.borrow_mut().set_offset(left, top);

        Ok(())
    }
    #[inline]
    fn offset(&self) -> (f32, f32) {
        (
            self.ht_placement_root.borrow().rect().X,
            self.ht_placement_root.borrow().rect().Y,
        )
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
        let (ox, oy) = self.offset();

        PaneDockLayer::compute_recommended_docking_destination(&self.docks, mode, x - ox, y - oy)
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
        let content_presenter = new_shared_mut(T::new(&header_view, ctx));
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
        self.ht_ref.borrow_mut().unmount();

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
        let rect = thisref.ht_ref.borrow().rect().clone();

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
            .dock_rect(&preview_rect, &docking_manager.borrow())
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
        self.hittest_tree_self.borrow_mut().unmount();

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

        let rect = group_view.borrow_mut().ht_ref.borrow().rect().clone();

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
            .dock_rect(&preview_rect, &docking_manager.borrow())
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
    fn new(
        _tab_header_view: &SharedMut<PaneTabHeaderView>,
        _view_ctx: &mut (impl ViewContext + ?Sized),
    ) -> Self;
}

pub struct InspectorTabPresenter {}
impl PaneTabContentPresenter for InspectorTabPresenter {
    fn build_content_view(
        &mut self,
        onto: &ContainerVisual,
        _onto_ht: &SharedMut<HitTestTree>,
        view_context: &mut dyn ViewContext,
    ) -> windows::core::Result<()> {
        let label = LabelView::new("Inspector Pane", view_context)?;
        label.mount(&onto.Children()?)?;

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

    fn new(
        _tab_header_view: &SharedMut<PaneTabHeaderView>,
        _view_ctx: &mut (impl ViewContext + ?Sized),
    ) -> Self {
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

    fn new(
        _tab_header_view: &SharedMut<PaneTabHeaderView>,
        _view_ctx: &mut (impl ViewContext + ?Sized),
    ) -> Self {
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

    fn new(
        _tab_header_view: &SharedMut<PaneTabHeaderView>,
        _view_ctx: &mut (impl ViewContext + ?Sized),
    ) -> Self {
        Self {}
    }
}

pub trait SignalEventReceiver {
    fn on_signal(&self, arg: usize);
}

pub enum SignalEventType {
    Receiver(Rc<dyn SignalEventReceiver>, usize),
    Message,
    Unknown,
}

pub struct PresentationAvailableEventEntry {
    pub event: EventHandle,
    pub buffer: IPresentationBuffer,
    pub buffer_res: ID3D11Texture2D,
}
pub struct AppGlobalSignals {
    pub entries: Vec<(Rc<dyn SignalEventReceiver>, usize)>,
    pub raw_events: Vec<HANDLE>,
}
impl AppGlobalSignals {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
            raw_events: Vec::new(),
        }
    }

    pub fn wait(&self) -> SignalEventType {
        let r = unsafe {
            MsgWaitForMultipleObjects(Some(&self.raw_events), false, INFINITE, QS_ALLEVENTS)
        };
        if WAIT_OBJECT_0.0 <= r.0 && r.0 < WAIT_OBJECT_0.0 + self.raw_events.len() as u32 {
            let index = (r.0 - WAIT_OBJECT_0.0) as usize;
            SignalEventType::Receiver(self.entries[index].0.clone(), self.entries[index].1)
        } else if WAIT_OBJECT_0.0 + self.raw_events.len() as u32 == r.0 {
            SignalEventType::Message
        } else {
            SignalEventType::Unknown
        }
    }

    pub fn register(
        &mut self,
        event: HANDLE,
        handler: &Rc<(impl SignalEventReceiver + 'static)>,
        arg: usize,
    ) {
        self.entries.push((handler.clone(), arg));
        self.raw_events.push(event);
    }
    pub fn unregister(&mut self, handler: &Rc<impl SignalEventReceiver + 'static>, arg: usize) {
        // Note: dynにするためにいったんcloneするしかない
        let handler: Rc<dyn SignalEventReceiver> = handler.clone();
        let Some(index) = self
            .entries
            .iter()
            .position(|(h, a)| Rc::ptr_eq(h, &handler) && *a == arg)
        else {
            // ない
            return;
        };

        self.raw_events.remove(index);
        self.entries.remove(index);
    }
}

pub struct StageTabContentRenderer {
    presentation_manager: IPresentationManager,
    presentation_surface: IPresentationSurface,
    graphics_queue: Rc<RefCell<br::QueueObject<StdVkDevice>>>,
    d3d11_device_context: ID3D11DeviceContext,
    back_buffers: Vec<(
        IPresentationBuffer,
        br::CommandBufferObject<StdVkDevice>,
        ID3D11Texture2D,
        ID3D11Texture2D,
        IDXGIKeyedMutex,
    )>,
}
impl SignalEventReceiver for StageTabContentRenderer {
    fn on_signal(&self, arg: usize) {
        let (pb, cb, fin, rt, km) = &self.back_buffers[arg];

        unsafe {
            km.AcquireSync(0, INFINITE)
                .expect("Failed to acquire keyed mutex");
        }
        self.graphics_queue
            .borrow_mut()
            .submit(
                &[br::EmptySubmissionBatch.with_command_buffers(&[cb])],
                None::<&mut br::FenceObject<StdVkDevice>>,
            )
            .expect("Failed to send command");
        self.graphics_queue
            .borrow_mut()
            .wait()
            .expect("Failed to wait work");
        unsafe {
            km.ReleaseSync(1).expect("Failed to release keyed mutex");
        }

        unsafe {
            km.AcquireSync(1, INFINITE)
                .expect("Failed to acquire keyed mutex");
        }
        unsafe {
            // Note: rtそのままでは表示できないらしい（Composition SwapchainでKeyedMutexいじれたらワンチャンありそうな気がする）
            self.d3d11_device_context.CopyResource(fin, rt);
        }
        unsafe {
            km.ReleaseSync(0).expect("Failed to release keyed mutex");
        }

        unsafe {
            self.presentation_surface
                .SetBuffer(pb)
                .expect("Failed to set new buffer");
        }
        unsafe {
            self.presentation_manager
                .Present()
                .expect("Failed to queue present");
        }
    }
}

pub struct StageTabPresenter {
    root: SpriteVisual,
    main_render_pass: br::RenderPassObject<StdVkDevice>,
    main_render_command_pool: br::CommandPoolObject<StdVkDevice>,
    _grid_pipeline_layout: br::PipelineLayoutObject<StdVkDevice>,
    _grid_pipeline: br::PipelineObject<StdVkDevice>,
    _grid_buffer: peridot_memory_manager::Buffer,
    camera_buffer: peridot_memory_manager::Buffer,
    _descriptor_set_layout_ub1: br::DescriptorSetLayoutObject<StdVkDevice>,
    _descriptor_pool: br::DescriptorPoolObject<StdVkDevice>,
    camera_descriptor_set: br::DescriptorSet,
    back_buffer_resources: Vec<(
        HANDLE,
        br::DeviceMemoryObject<StdVkDevice>,
        br::FramebufferObject<'static, StdVkDevice>,
    )>,
    renderer: Rc<StageTabContentRenderer>,
}
impl PaneTabContentPresenter for StageTabPresenter {
    fn build_content_view(
        &mut self,
        onto: &ContainerVisual,
        _onto_ht: &SharedMut<HitTestTree>,
        view_context: &mut dyn ViewContext,
    ) -> windows::core::Result<()> {
        onto.Children()?.InsertAtTop(&self.root)?;
        for (n, (e, _, _)) in self.back_buffer_resources.iter().enumerate() {
            view_context
                .app_global_signals()
                .borrow_mut()
                .register(*e, &self.renderer, n);
        }

        Ok(())
    }

    fn on_hide_content_view(
        &mut self,
        view_context: &mut dyn ViewContext,
    ) -> windows::core::Result<()> {
        for (n, _) in self.back_buffer_resources.iter().enumerate() {
            view_context
                .app_global_signals()
                .borrow_mut()
                .unregister(&self.renderer, n);
        }

        Ok(())
    }
}
impl PaneTabPresenter for StageTabPresenter {
    const INIT_TAB_NAME: &'static str = "Stage";

    fn new(
        _tab_header_view: &SharedMut<PaneTabHeaderView>,
        view_ctx: &mut (impl ViewContext + ?Sized),
    ) -> Self {
        let root = view_ctx
            .compositor()
            .CreateSpriteVisual()
            .expect("Failed to create root visual");

        let composition_surface_handle = unsafe {
            DCompositionCreateSurfaceHandle(
                (COMPOSITIONOBJECT_READ | COMPOSITIONOBJECT_WRITE) as _,
                None,
            )
            .expect("Failed to create composition surface handle")
        };
        let presentation_surface = unsafe {
            view_ctx
                .presentation_manager()
                .CreatePresentationSurface(composition_surface_handle)
                .expect("Failed to create presentation surface")
        };
        let surface = unsafe {
            view_ctx
                .compositor_interop()
                .CreateCompositionSurfaceForHandle(composition_surface_handle)
                .expect("Failed to create ui composition surface")
        };
        unsafe {
            presentation_surface
                .SetSourceRect(&RECT {
                    left: 0,
                    top: 0,
                    right: 128,
                    bottom: 128,
                })
                .expect("Failed to set source rect");
        }

        let brush = view_ctx
            .compositor()
            .CreateSurfaceBrushWithSurface(&surface)
            .expect("Failed to create surface brush");
        root.SetBrush(&brush).expect("Failed to set surface brush");
        root.SetSize(Vector2::scalar(128.0))
            .expect("Failed to resize visual");
        root.SetOffset(Vector3::zero())
            .expect("Failed to position visual");

        unsafe {
            presentation_surface
                .SetAlphaMode(DXGI_ALPHA_MODE_IGNORE)
                .expect("Failed to set alpha mode");
            presentation_surface
                .SetColorSpace(DXGI_COLOR_SPACE_RGB_FULL_G10_NONE_P709)
                .expect("Failed to set color space");
        }

        let main_render_pass = br::RenderPassBuilder2::new(
            &[
                br::AttachmentDescription2::new(br::vk::VK_FORMAT_R8G8B8A8_UNORM)
                    .layout_transition(br::ImageLayout::Undefined, br::ImageLayout::General)
                    .color_memory_op(br::LoadOp::Clear, br::StoreOp::Store),
                br::AttachmentDescription2::new(br::vk::VK_FORMAT_D24_UNORM_S8_UINT)
                    .layout_transition(
                        br::ImageLayout::Undefined,
                        br::ImageLayout::DepthStencilAttachmentOpt,
                    )
                    .color_memory_op(br::LoadOp::Clear, br::StoreOp::DontCare),
            ],
            &[br::SubpassDescription2::new()
                .colors(&[br::AttachmentReference2::color(
                    0,
                    br::ImageLayout::ColorAttachmentOpt,
                )])
                .depth_stencil(&br::AttachmentReference2::depth_stencil(
                    1,
                    br::ImageLayout::DepthStencilAttachmentOpt,
                ))],
            &[br::SubpassDependency2::new(
                br::SubpassIndex::Internal(0),
                br::SubpassIndex::External,
            )
            .of_memory(
                br::AccessFlags::COLOR_ATTACHMENT.write,
                br::AccessFlags::MEMORY.read,
            )],
        )
        .create(view_ctx.mini_engine().device().clone())
        .expect("Failed to create main render pass");
        let mut main_render_command_pool =
            br::CommandPoolBuilder::new(view_ctx.mini_engine().graphics_queue_family_index())
                .create(view_ctx.mini_engine().device().clone())
                .expect("Failed to create command pool");
        let main_render_commands = main_render_command_pool
            .alloc(3, true)
            .expect("Failed to allocate command buffers");

        let shared_depth_stencil_buffer = view_ctx
            .mini_engine_mut()
            .alloc_device_local_image(br::ImageDesc::new(
                br::vk::VkExtent2D::spread1(128),
                br::vk::VK_FORMAT_D24_UNORM_S8_UINT,
                br::ImageUsageFlags::DEPTH_STENCIL_ATTACHMENT,
                br::ImageLayout::Undefined,
            ))
            .expect("Failed to create shared depth stencil buffer");
        let shared_depth_stencil_buffer = Rc::new(
            shared_depth_stencil_buffer
                .subresource_range(br::AspectMask::DEPTH.stencil(), 0..1, 0..1)
                .view_builder()
                .create()
                .expect("Failed to create shared depth stencil buffer view"),
        );

        let descriptor_set_layout_ub1 = br::DescriptorSetLayoutBuilder::new()
            .bind(
                br::DescriptorType::UniformBuffer
                    .make_binding(1)
                    .only_for_vertex(),
            )
            .create(view_ctx.mini_engine().device().clone())
            .expect("Failed to create descriptor set layout");

        let grid_vsh = view_ctx
            .mini_engine_mut()
            .shader("shaders/simple_transformed_static_pos.vspv")
            .expect("Failed to load vertex shader");
        let grid_fsh = view_ctx
            .mini_engine_mut()
            .shader("shaders/vertex_color.fspv")
            .expect("Failed to load fragment shader");
        let (grid_vbinds, grid_vattrs) = ColoredVertex::single_binding(0, 1);
        let grid_pipeline_layout = br::PipelineLayoutBuilder::new(
            vec![&descriptor_set_layout_ub1],
            vec![(br::ShaderStage::VERTEX, 0..64)],
        )
        .create(view_ctx.mini_engine().device().clone())
        .expect("Failed to create grid pipeline layout");
        let mut grid_pipeline = br::NonDerivedGraphicsPipelineBuilder::new(
            &grid_pipeline_layout,
            (&main_render_pass, 0),
            br::VertexProcessingStages::new(
                br::VertexShaderStage::new(br::PipelineShader2::new(&grid_vsh, c"main".to_owned()))
                    .with_fragment_shader_stage(br::PipelineShader2::new(
                        &grid_fsh,
                        c"main".to_owned(),
                    )),
                &grid_vbinds,
                &grid_vattrs,
                br::vk::VK_PRIMITIVE_TOPOLOGY_LINE_LIST,
            ),
        );
        grid_pipeline
            .multisample_state(Some(br::MultisampleState::new()))
            .add_attachment_blend(br::AttachmentColorBlendState::noblend())
            .viewport_scissors(
                br::DynamicArrayState::Dynamic(1),
                br::DynamicArrayState::Dynamic(1),
            )
            .depth_test_settings(Some(br::CompareOp::LessOrEqual), true);
        let grid_pipeline = grid_pipeline
            .create(
                view_ctx.mini_engine().device().clone(),
                Some(view_ctx.mini_engine().pipeline_cache()),
            )
            .expect("Failed to create grid pipeline state");
        view_ctx.mini_engine().writeback_pipeline_cache();

        let grid_vertices = (-10..=10)
            .flat_map(|x| {
                [
                    ColoredVertex {
                        pos: Vec4::new(x as _, 0.0, -10.0, 1.0),
                        color: Vec4::new(0.5, 0.5, 0.5, 1.0),
                    },
                    ColoredVertex {
                        pos: Vec4::new(x as _, 0.0, 10.0, 1.0),
                        color: Vec4::new(0.5, 0.5, 0.5, 1.0),
                    },
                ]
            })
            .chain((-10..=10).flat_map(|z| {
                [
                    ColoredVertex {
                        pos: Vec4::new(-10.0, 0.0, z as _, 1.0),
                        color: Vec4::new(0.5, 0.5, 0.5, 1.0),
                    },
                    ColoredVertex {
                        pos: Vec4::new(10.0, 0.0, z as _, 1.0),
                        color: Vec4::new(0.5, 0.5, 0.5, 1.0),
                    },
                ]
            }))
            .chain([
                ColoredVertex {
                    pos: Vec4::new(0.0, 0.0, 0.0, 1.0),
                    color: Vec4::new(1.0, 0.0, 0.0, 1.0),
                },
                ColoredVertex {
                    pos: Vec4::new(1000.0, 0.0, 0.0, 1.0),
                    color: Vec4::new(1.0, 0.0, 0.0, 1.0),
                },
                ColoredVertex {
                    pos: Vec4::new(0.0, 0.0, 0.0, 1.0),
                    color: Vec4::new(0.0, 1.0, 0.0, 1.0),
                },
                ColoredVertex {
                    pos: Vec4::new(0.0, 1000.0, 0.0, 1.0),
                    color: Vec4::new(0.0, 1.0, 0.0, 1.0),
                },
                ColoredVertex {
                    pos: Vec4::new(0.0, 0.0, 0.0, 1.0),
                    color: Vec4::new(0.0, 0.0, 1.0, 1.0),
                },
                ColoredVertex {
                    pos: Vec4::new(0.0, 0.0, 1000.0, 1.0),
                    color: Vec4::new(0.0, 0.0, 1.0, 1.0),
                },
            ])
            .collect::<Vec<_>>();
        let mut default_camera = Camera {
            projection: Some(ProjectionMethod::Perspective {
                fov: 60.0f32.to_radians(),
            }),
            position: peridot_math::Vector3(0.0, 1.0, -5.0),
            rotation: peridot_math::Quaternion::ONE,
            depth_range: 0.1..100.0,
        };
        default_camera.look_at(peridot_math::Vector3::ZERO);

        let [grid_buffer, camera_buffer] = view_ctx
            .mini_engine_mut()
            .alloc_device_local_buffer_array([
                br::BufferDesc::new(
                    core::mem::size_of::<ColoredVertex>() * grid_vertices.len(),
                    br::BufferUsage::VERTEX_BUFFER.transfer_dest(),
                ),
                br::BufferDesc::new(
                    core::mem::size_of::<peridot_math::Matrix4F32>(),
                    br::BufferUsage::UNIFORM_BUFFER.transfer_dest(),
                ),
            ])
            .expect("Failed to allocate device local buffers");
        let [mut grid_buffer_stg, mut camera_buffer_stg] = view_ctx
            .mini_engine_mut()
            .alloc_upload_buffer_array([
                br::BufferDesc::new(
                    core::mem::size_of::<ColoredVertex>() * grid_vertices.len(),
                    br::BufferUsage::TRANSFER_SRC,
                ),
                br::BufferDesc::new(
                    core::mem::size_of::<peridot_math::Matrix4F32>(),
                    br::BufferUsage::TRANSFER_SRC,
                ),
            ])
            .expect("Failed to allocate upload buffers");
        grid_buffer_stg
            .clone_content_from_slice(&grid_vertices)
            .expect("Failed to write grid vbuffer content");
        camera_buffer_stg
            .write_content(default_camera.view_projection_matrix(1.0))
            .expect("Failed to write camera matrix");

        // initialize
        let mut cp =
            br::CommandPoolBuilder::new(view_ctx.mini_engine().graphics_queue_family_index())
                .transient()
                .create(view_ctx.mini_engine().device())
                .expect("Failed to create initialize command pool");
        let mut init_cb = cp
            .alloc(1, true)
            .expect("Failed to allocate init command buffer");
        unsafe {
            init_cb[0]
                .begin_once()
                .expect("Failed to begin init commands")
        }
        .pipeline_barrier_2(&br::DependencyInfo::new(
            &[br::MemoryBarrier2::new()
                .of_memory(
                    br::AccessFlags2::HOST.write,
                    br::AccessFlags2::TRANSFER.read,
                )
                .of_execution(br::PipelineStageFlags2::HOST, br::PipelineStageFlags2::COPY)],
            &[],
            &[],
        ))
        .copy_buffer(
            &grid_buffer_stg,
            &grid_buffer,
            &[br::vk::VkBufferCopy {
                srcOffset: 0,
                dstOffset: 0,
                size: grid_buffer.byte_length() as _,
            }],
        )
        .copy_buffer(
            &camera_buffer_stg,
            &camera_buffer,
            &[br::vk::VkBufferCopy {
                srcOffset: 0,
                dstOffset: 0,
                size: camera_buffer_stg.byte_length() as _,
            }],
        )
        .pipeline_barrier_2(&br::DependencyInfo::new(
            &[br::MemoryBarrier2::new()
                .of_memory(
                    br::AccessFlags2::TRANSFER.write,
                    br::AccessFlags2::VERTEX_ATTRIBUTE_READ | br::AccessFlags2::UNIFORM_READ,
                )
                .of_execution(
                    br::PipelineStageFlags2::COPY,
                    br::PipelineStageFlags2::VERTEX_INPUT | br::PipelineStageFlags2::VERTEX_SHADER,
                )],
            &[],
            &[],
        ))
        .end()
        .expect("Failed to finish init commands");
        view_ctx
            .mini_engine()
            .graphics_queue()
            .borrow_mut()
            .submit2(
                &[br::SubmitInfo2::new(
                    &[],
                    &[br::CommandBufferSubmitInfo::new(&init_cb[0])],
                    &[],
                )],
                None::<&mut br::FenceObject<StdVkDevice>>,
            )
            .expect("Failed to submit init commands");
        view_ctx
            .mini_engine()
            .graphics_queue()
            .borrow_mut()
            .wait()
            .expect("Failed to wait init commands");

        let mut dp = br::DescriptorPoolBuilder::new(1)
            .reserve(br::DescriptorType::UniformBuffer.with_count(1))
            .create(view_ctx.mini_engine().device().clone())
            .expect("Failed to create descriptor pool");
        let camera_descriptor_set = dp
            .alloc(&[&descriptor_set_layout_ub1])
            .expect("Failed to allocate camera descriptor set");
        view_ctx.mini_engine().device().update_descriptor_sets(
            &[
                br::DescriptorPointer::new(camera_descriptor_set[0].0, 0).write(
                    br::DescriptorContents::UniformBuffer(vec![br::DescriptorBufferRef::new(
                        &camera_buffer,
                        0..core::mem::size_of::<Mat4>() as u64,
                    )]),
                ),
            ],
            &[],
        );

        let mut back_buffer_resources = Vec::with_capacity(3);
        let mut back_buffer_render_resources = Vec::with_capacity(3);
        for (n, mut cb) in (0..3).zip(main_render_commands.into_iter()) {
            let texture_desc = D3D11_TEXTURE2D_DESC {
                Width: 128,
                Height: 128,
                MipLevels: 1,
                ArraySize: 1,
                Format: DXGI_FORMAT_R8G8B8A8_UNORM,
                SampleDesc: DXGI_SAMPLE_DESC {
                    Count: 1,
                    Quality: 0,
                },
                Usage: D3D11_USAGE_DEFAULT,
                BindFlags: (D3D11_BIND_SHADER_RESOURCE | D3D11_BIND_RENDER_TARGET).0 as _,
                CPUAccessFlags: 0,
                MiscFlags: (D3D11_RESOURCE_MISC_SHARED
                    | D3D11_RESOURCE_MISC_SHARED_NTHANDLE
                    | D3D11_RESOURCE_MISC_SHARED_DISPLAYABLE)
                    .0 as _,
            };
            let mut texture = core::mem::MaybeUninit::uninit();
            unsafe {
                view_ctx
                    .d3d11_device()
                    .CreateTexture2D(&texture_desc, None, Some(texture.as_mut_ptr()))
                    .expect("Failed to create back buffer texture")
            };
            let texture = unsafe { texture.assume_init().expect("texture not created") };
            let presentation_buffer = unsafe {
                view_ctx
                    .presentation_manager()
                    .AddBufferFromResource(&texture)
                    .expect("Failed to add texture as presentation buffer")
            };
            let eh = unsafe {
                presentation_buffer
                    .GetAvailableEvent()
                    .expect("Failed to get available event handle")
            };

            let rt_desc = D3D11_TEXTURE2D_DESC {
                BindFlags: D3D11_BIND_RENDER_TARGET.0 as _,
                MiscFlags: (D3D11_RESOURCE_MISC_SHARED_NTHANDLE
                    | D3D11_RESOURCE_MISC_SHARED_KEYEDMUTEX)
                    .0 as _,
                ..texture_desc
            };
            let mut rt = core::mem::MaybeUninit::uninit();
            unsafe {
                view_ctx
                    .d3d11_device()
                    .CreateTexture2D(&rt_desc, None, Some(rt.as_mut_ptr()))
                    .expect("Failed to create render target texture");
            }
            let rt = unsafe { rt.assume_init().expect("rt not created") };

            let share_name = widestring::WideCString::from_str(&format!("PMEStageBackbuffer{n}"))
                .expect("invalid sequence");

            let texture_res = rt
                .cast::<IDXGIResource1>()
                .expect("Failed to query underlying resource");
            let tex_handle = unsafe {
                texture_res
                    .CreateSharedHandle(
                        None,
                        GENERIC_ALL.0 | DXGI_SHARED_RESOURCE_READ | DXGI_SHARED_RESOURCE_WRITE,
                        PCWSTR(share_name.as_ptr()),
                    )
                    .expect("Failed to get shared handle")
            };
            let external_handle = br::ExternalMemoryHandleTypeWin32::D3D11Texture
                .with_handle(unsafe { core::mem::transmute(tex_handle.0) });
            let external_handle_image_memory_req = unsafe {
                external_handle
                    .properties(
                        &view_ctx.mini_engine().graphics_objects.device,
                        br::vk::VkMemoryWin32HandlePropertiesKHR::uninit_sink(),
                    )
                    .expect("Failed to query external handle memory properties")
            };
            let mut vk_image = br::ImageDesc::new(
                br::vk::VkExtent2D::spread1(128),
                br::vk::VK_FORMAT_R8G8B8A8_UNORM,
                br::ImageUsageFlags::COLOR_ATTACHMENT,
                br::ImageLayout::Undefined,
            )
            .exportable_as(br::ExternalMemoryHandleTypes::D3D11_TEXTURE)
            .create(view_ctx.mini_engine().graphics_objects.device.clone())
            .expect("Failed to create external backbuffer image");
            let vk_image_memory_req = vk_image.requirements();
            let vk_memory_index = view_ctx
                .mini_engine()
                .find_device_local_memory_index(
                    vk_image_memory_req.memoryTypeBits
                        & external_handle_image_memory_req.memoryTypeBits,
                )
                .expect("no suitable memory");
            let vk_image_memory =
                br::DeviceMemoryRequest::import(vk_memory_index, external_handle, &share_name)
                    .execute(view_ctx.mini_engine().graphics_objects.device.clone())
                    .expect("Failed to import d3d11 memory");
            vk_image
                .bind(&vk_image_memory, 0)
                .expect("Failed to bind image to memory");
            let vk_image = Rc::new(vk_image);

            let vk_framebuffer = br::FramebufferBuilder::new(&main_render_pass)
                .with_attachment(
                    vk_image
                        .clone()
                        .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                        .view_builder()
                        .create()
                        .expect("Failed to create image view"),
                )
                .with_attachment(shared_depth_stencil_buffer.clone())
                .create()
                .expect("Failed to create framebuffer");

            unsafe { cb.begin().expect("Failed to begin command recording") }
                .begin_render_pass(
                    &main_render_pass,
                    &vk_framebuffer,
                    br::vk::VkRect2D {
                        offset: br::vk::VkOffset2D::ZERO,
                        extent: br::vk::VkExtent2D {
                            width: 128,
                            height: 128,
                        },
                    },
                    &[
                        br::ClearValue::color_f32([0.0, 0.0, 0.0, 1.0]),
                        br::ClearValue::depth_stencil(1.0, 0),
                    ],
                    true,
                )
                .bind_graphics_pipeline_pair(&grid_pipeline, &grid_pipeline_layout)
                .set_viewport(
                    0,
                    &[br::vk::VkViewport {
                        x: 0.0,
                        y: 0.0,
                        width: 128.0,
                        height: 128.0,
                        minDepth: 0.0,
                        maxDepth: 1.0,
                    }],
                )
                .set_scissor(
                    0,
                    &[br::vk::VkRect2D {
                        offset: br::vk::VkOffset2D::ZERO,
                        extent: br::vk::VkExtent2D::spread1(128),
                    }],
                )
                .bind_graphics_descriptor_sets(0, &[camera_descriptor_set[0].0], &[])
                .bind_vertex_buffers(0, &[(&grid_buffer, 0)])
                .push_graphics_constant(br::ShaderStage::VERTEX, 0, &Mat4::IDENTITY)
                .draw(grid_vertices.len() as _, 1, 0, 0)
                .end_render_pass()
                .end()
                .expect("Failed to record commands");

            let rt_mutex = rt
                .cast::<IDXGIKeyedMutex>()
                .expect("Failed to get keyed mutex");
            back_buffer_render_resources.push((presentation_buffer, cb, texture, rt, rt_mutex));
            back_buffer_resources.push((eh, vk_image_memory, vk_framebuffer));
        }

        Self {
            root,
            main_render_pass,
            main_render_command_pool,
            _grid_pipeline_layout: grid_pipeline_layout,
            _grid_pipeline: grid_pipeline,
            _grid_buffer: grid_buffer,
            camera_buffer,
            _descriptor_set_layout_ub1: descriptor_set_layout_ub1,
            _descriptor_pool: dp,
            camera_descriptor_set: camera_descriptor_set[0],
            renderer: Rc::new(StageTabContentRenderer {
                back_buffers: back_buffer_render_resources,
                presentation_manager: view_ctx.presentation_manager().clone(),
                presentation_surface,
                graphics_queue: view_ctx.mini_engine().graphics_queue().clone(),
                d3d11_device_context: unsafe {
                    view_ctx
                        .d3d11_device()
                        .GetImmediateContext()
                        .expect("Failed to get d3d imm context")
                },
            }),
            back_buffer_resources,
        }
    }
}

pub struct EventHandle(HANDLE);
unsafe impl Sync for EventHandle {}
unsafe impl Send for EventHandle {}
impl EventHandle {
    #[inline(always)]
    pub fn new() -> windows::core::Result<Self> {
        unsafe { CreateEventA(None, false, false, None).map(Self) }
    }

    #[inline(always)]
    pub fn set(&self) -> windows::core::Result<()> {
        unsafe { SetEvent(self.0) }
    }

    #[inline(always)]
    pub fn reset(&self) -> windows::core::Result<()> {
        unsafe { ResetEvent(self.0) }
    }
}
impl Drop for EventHandle {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe { CloseHandle(self.0).expect("Failed to close event handle") }
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

    fn new(
        _tab_header_view: &SharedMut<PaneTabHeaderView>,
        _view_ctx: &mut (impl ViewContext + ?Sized),
    ) -> Self {
        Self {}
    }
}

pub struct ObjectTreeTabPresenter {}
impl PaneTabContentPresenter for ObjectTreeTabPresenter {
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
impl PaneTabPresenter for ObjectTreeTabPresenter {
    const INIT_TAB_NAME: &'static str = "Object Tree";

    fn new(
        _tab_header_view: &SharedMut<PaneTabHeaderView>,
        _view_ctx: &mut (impl ViewContext + ?Sized),
    ) -> Self {
        Self {}
    }
}

pub struct AssetExplorerTabPresenter {}
impl PaneTabContentPresenter for AssetExplorerTabPresenter {
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
impl PaneTabPresenter for AssetExplorerTabPresenter {
    const INIT_TAB_NAME: &'static str = "Asset Explorer";

    fn new(
        _tab_header_view: &SharedMut<PaneTabHeaderView>,
        _view_ctx: &mut (impl ViewContext + ?Sized),
    ) -> Self {
        Self {}
    }
}

pub struct LabelView {
    pub root: SpriteVisual,
}
impl LabelView {
    pub fn new(
        text: impl Into<Cow<'static, str>>,
        ctx: &mut (impl ViewContext + ?Sized),
    ) -> windows::core::Result<Self> {
        let root = ctx.compositor().CreateSpriteVisual()?;
        let text_format =
            ctx.text_format_stock_mut()
                .get("system-ui", 12.0, DWRITE_FONT_WEIGHT_NORMAL)?;
        let text_surface = ctx.text_surface_stock_mut().get(&text_format, text)?;
        let brush = ctx
            .compositor()
            .CreateSurfaceBrushWithSurface(&text_surface.surface)?;
        root.set_properties().brush(&brush)?.size(Vector2 {
            X: text_surface.width,
            Y: text_surface.height,
        })?;

        Ok(Self { root })
    }

    pub fn mount(&self, onto: &VisualCollection) -> windows::core::Result<()> {
        onto.InsertAtTop(&self.root)?;

        Ok(())
    }
    pub fn unmount(&self) -> windows::core::Result<()> {
        self.root.Parent()?.Children()?.Remove(&self.root)?;

        Ok(())
    }
}

type StdVkDevice = Rc<br::DeviceObject<Rc<br::InstanceObject>>>;

struct AppWindowState<'r> {
    input_state: InputState,
    compositor: Compositor,
    compositor_interop: ICompositorInterop,
    ui_common_objects: &'r UICommonObjects,
    d2d1_factory: ID2D1Factory1,
    dwrite_factory: IDWriteFactory,
    text_format_stock: &'r mut TextFormatStock,
    text_surface_stock: &'r mut TextSurfaceStock,
    hittest_context: HitTestTreeContext,
    pane_group_docking_manager: SharedMut<PaneGroupDockingManager>,
    app_title_bar_view: SharedMut<AppTitleBarView>,
    presentation_manager: IPresentationManager,
    d3d11_device: ID3D11Device,
    app_global_signals: SharedMut<AppGlobalSignals>,
    currently_maximized: bool,
    mini_engine: MiniEngine,
}
impl ViewContext for AppWindowState<'_> {
    fn compositor(&self) -> &windows::UI::Composition::Compositor {
        &self.compositor
    }

    fn compositor_interop(&self) -> &ICompositorInterop {
        &self.compositor_interop
    }

    fn common(&self) -> &UICommonObjects {
        &self.ui_common_objects
    }

    fn d2d1_factory(&self) -> &ID2D1Factory1 {
        &self.d2d1_factory
    }

    fn dwrite_factory(&self) -> &IDWriteFactory {
        &self.dwrite_factory
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

    fn presentation_manager(&self) -> &IPresentationManager {
        &self.presentation_manager
    }

    fn d3d11_device(&self) -> &ID3D11Device {
        &self.d3d11_device
    }

    fn app_global_signals(&self) -> &SharedMut<AppGlobalSignals> {
        &self.app_global_signals
    }

    fn mini_engine(&self) -> &MiniEngine {
        &self.mini_engine
    }

    fn mini_engine_mut(&mut self) -> &mut MiniEngine {
        &mut self.mini_engine
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
    pub fn clear_state_store(&mut self) {
        unsafe {
            SetWindowLongPtrA(self.handle, Self::STATE_STORE_PTR_INDEX, 0);
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
    #[inline]
    pub fn map_points_from_desktop(&self, points: &mut [POINT]) {
        unsafe {
            MapWindowPoints(None, self.handle, points);
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
    pub fn is_maximized(&self) -> windows::core::Result<bool> {
        let mut r = core::mem::MaybeUninit::<WINDOWPLACEMENT>::uninit();
        unsafe {
            (*r.as_mut_ptr()).length = core::mem::size_of::<WINDOWPLACEMENT>() as _;
            GetWindowPlacement(self.handle, r.as_mut_ptr())
                .map(|_| r.assume_init().showCmd == SW_MAXIMIZE.0 as _)
        }
    }

    #[inline]
    pub fn show(&self) {
        unsafe {
            let _ = ShowWindow(self.handle, SW_SHOWNORMAL);
        }
    }
}

// windows app sdk bootstrapping
type FPMddBootstrapInitialize2 = extern "system" fn(
    majorMinorVersion: u32,
    versionTag: PCWSTR,
    minVersion: PACKAGE_VERSION,
    options: MddBootstrapInitializeOptions,
) -> HRESULT;
type FPMddBootstrapShutdown = extern "system" fn();
#[repr(C)]
#[derive(Clone, Copy)]
enum MddBootstrapInitializeOptions {
    ShowUI = 0x08,
}
// copy from WindowsAppSDK-VersionInfo.h
const APP_SDK_VERSION_U64: u64 = 0;

fn main() {
    let app_runtime_lib = unsafe {
        LoadLibraryA(s!("Microsoft.WindowsAppRuntime.Bootstrap.dll"))
            .expect("Failed to load runtime bootstrap dll")
    };
    let initializer: FPMddBootstrapInitialize2 = unsafe {
        core::mem::transmute(
            GetProcAddress(app_runtime_lib, s!("MddBootstrapInitialize2"))
                .expect("Failed to load initialize fn"),
        )
    };
    let shutdown: FPMddBootstrapShutdown = unsafe {
        core::mem::transmute(
            GetProcAddress(app_runtime_lib, s!("MddBootstrapShutdown"))
                .expect("Failed to load shutdown fn"),
        )
    };

    unsafe {
        initializer(
            0x00010005,
            w!(""),
            core::mem::transmute(APP_SDK_VERSION_U64),
            MddBootstrapInitializeOptions::ShowUI,
        )
        .ok()
        .expect("Failed to initialize windows app runtime");
    }
    let r = app();
    shutdown();
    std::process::exit(r);
}

fn app() -> i32 {
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

    let mut miniengine = MiniEngine::new().expect("Failed to initialize mini engine");

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

    if !MicaController::IsSupported().expect("Failed to get mica support") {
        panic!("Mica is not supported");
    }

    let mica = MicaController::new().expect("Failed to get mica controller");
    let system_backdrop_configuration =
        SystemBackdropConfiguration::new().expect("Failed to create system backdrop configuration");
    system_backdrop_configuration
        .SetIsInputActive(true)
        .expect("Failed to set input active");
    system_backdrop_configuration
        .SetTheme(SystemBackdropTheme::Default)
        .expect("Failed to set theme");
    mica.SetKind(MicaKind::BaseAlt)
        .expect("Failed to set mica kind");
    mica.SetSystemBackdropConfiguration(&system_backdrop_configuration)
        .expect("Failed to set backdrop config");

    mica.AddSystemBackdropTarget(
        &desktop_window_target
            .cast::<ICompositionSupportsSystemBackdrop>()
            .expect("Failed to get system backdrop support"),
    )
    .expect("Failed to add backdrop target");

    let bg = compositor
        .CreateSpriteVisual()
        .expect("Failed to create bg");
    bg.SetBrush(
        &compositor
            .CreateColorBrushWithColor(Color {
                A: 0,
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

    let composition_debug =
        CompositionDebugSettings::TryGetSettings(&compositor).expect("Failed to get settings");

    let overlay_layer = compositor
        .CreateRedirectVisual()
        .expect("Failed to create overlay layer");
    overlay_layer
        .set_properties()
        .relative_offset_adjustment(Vector3::zero())
        .expect("Failed to set relative offset adjustment")
        .relative_size_adjustment(Vector2::one())
        .expect("Failed to set relative size adjustment");
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

    let app_global_signals = new_shared_mut(AppGlobalSignals::new());

    let mut view_context = ViewContext1 {
        compositor: &compositor,
        compositor_interop: &compositor_interop,
        common: &common_objects,
        d2d1_factory: &d2d1_factory,
        dwrite_factory: &dwrite_factory,
        text_format_stock: &mut text_format_stock,
        text_surface_stock: &mut text_surface_stock,
        hittest_context: &mut hittest_context,
        presentation_manager: &presentation_manager,
        d3d11_device: &d3d11_device,
        app_global_signals: &app_global_signals,
        mini_engine: &mut miniengine,
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

    let main_pane = TabGroupPaneView::new(&pane_group_docking_manager, &mut view_context)
        .expect("Failed to create TabGroupPaneView");
    TabGroupPaneView::add_tab::<StageTabPresenter>(&main_pane, &mut view_context)
        .expect("Failed to create StagePaneTab");
    TabGroupPaneView::add_tab::<PreviewTabPresenter>(&main_pane, &mut view_context)
        .expect("Failed to create PreviewPaneTab");
    TabGroupPaneView::add_tab::<ProjectSettingsTabPresenter>(&main_pane, &mut view_context)
        .expect("Failed to create ProjectSettingsPaneTabHeader");
    main_pane.borrow_mut().rearrange();

    let pane_group3 = TabGroupPaneView::new(&pane_group_docking_manager, &mut view_context)
        .expect("Failed to create TabGroupPaneView");
    TabGroupPaneView::add_tab::<InspectorTabPresenter>(&pane_group3, &mut view_context)
        .expect("Failed to create InspectorPaneTabHeader");
    pane_group3.borrow_mut().rearrange();
    pane_group3
        .borrow_mut()
        .resize(256.0, 256.0)
        .expect("Failed to resize pane");

    let explorers_pane = TabGroupPaneView::new(&pane_group_docking_manager, &mut view_context)
        .expect("Failed to create TabGroupPaneView");
    TabGroupPaneView::add_tab::<AssetExplorerTabPresenter>(&explorers_pane, &mut view_context)
        .expect("Failed to create AssetExplorerTab");
    explorers_pane.borrow_mut().rearrange();
    explorers_pane
        .borrow_mut()
        .resize(256.0, 256.0)
        .expect("Failed to resize pane");

    let scene_subinfo_pane = TabGroupPaneView::new(&pane_group_docking_manager, &mut view_context)
        .expect("Failed to create TabGroupPaneView");
    TabGroupPaneView::add_tab::<ObjectTreeTabPresenter>(&scene_subinfo_pane, &mut view_context)
        .expect("Failed to create ObjectTreeTab");
    scene_subinfo_pane.borrow_mut().rearrange();
    scene_subinfo_pane
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
                        |parent, ctx| {
                            PaneDockLayer::new_on_bottom(
                                ctx,
                                parent,
                                |parent, _| PaneDockLayer::new_filled(&explorers_pane, parent),
                                |parent, ctx| {
                                    PaneDockLayer::new_on_left(
                                        ctx,
                                        parent,
                                        |parent, _| {
                                            PaneDockLayer::new_filled(&scene_subinfo_pane, parent)
                                        },
                                        |parent, _| PaneDockLayer::new_filled(&main_pane, parent),
                                    )
                                    .expect("Failed to create pane dock layer")
                                },
                            )
                            .expect("Failed to create pane dock layer")
                        },
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
        .set_offset(0.0, AppTitleBarView::HEIGHT)
        .expect("Failed to set docking manager offset");
    pane_group_docking_manager
        .borrow_mut()
        .resize_root(client_width, client_height - AppTitleBarView::HEIGHT)
        .expect("Failed to initial relayout");

    let app_title = AppTitleBarView::new(&mut view_context, app_global_scale)
        .expect("Failed to initialize app title bar");
    app_title
        .borrow()
        .mount(
            &composition_root
                .Children()
                .expect("Failed to get children collection"),
            &hittest_tree_root,
        )
        .expect("Failed to mount app title bar");

    let mut ws = AppWindowState {
        input_state: InputState::new(window_handle.handle, &hittest_tree_root),
        compositor: compositor.clone(),
        compositor_interop,
        ui_common_objects: &common_objects,
        d2d1_factory,
        dwrite_factory,
        text_format_stock: &mut text_format_stock,
        text_surface_stock: &mut text_surface_stock,
        hittest_context,
        pane_group_docking_manager,
        app_title_bar_view: app_title,
        presentation_manager,
        d3d11_device,
        app_global_signals: app_global_signals.clone(),
        currently_maximized: window_handle
            .is_maximized()
            .expect("Failed to query maximized state"),
        mini_engine: miniengine,
    };
    window_handle.set_state_store(&mut ws);
    window_handle.show();

    composition_debug
        .HeatMaps()
        .expect("Failed to get heatmap object")
        .ShowMemoryUsage(&overlay_layer)
        .expect("Failed to set composition debug view");

    let mut msg = core::mem::MaybeUninit::<MSG>::uninit();
    'app: loop {
        let r = app_global_signals.borrow().wait();
        match r {
            SignalEventType::Message => {
                while unsafe { PeekMessageA(msg.as_mut_ptr(), None, 0, 0, PM_REMOVE).0 != 0 } {
                    if unsafe { msg.assume_init_ref().message == WM_QUIT } {
                        break 'app;
                    }

                    unsafe {
                        let _ = TranslateMessage(msg.as_ptr());
                        DispatchMessageA(msg.as_ptr());
                    }
                }
            }
            SignalEventType::Receiver(handler, arg) => handler.on_signal(arg),
            SignalEventType::Unknown => (),
        }
    }

    window_handle.clear_state_store();

    unsafe { msg.assume_init().wParam.0 as _ }
}

extern "system" fn window_proc(hwnd: HWND, msg: u32, wp: WPARAM, lp: LPARAM) -> LRESULT {
    if msg == WM_DESTROY {
        unsafe { PostQuitMessage(0) };
        return LRESULT(0);
    }
    if msg == WM_CREATE {
        let mut rc = core::mem::MaybeUninit::uninit();
        unsafe {
            GetWindowRect(hwnd, rc.as_mut_ptr()).expect("Failed to get window rect");
        }
        let rc = unsafe { rc.assume_init() };
        unsafe {
            SetWindowPos(
                hwnd,
                None,
                rc.left,
                rc.top,
                rc.right - rc.left,
                rc.bottom - rc.top,
                SWP_FRAMECHANGED,
            )
            .expect("Failed to reset window frame");
        }

        return LRESULT(0);
    }
    if msg == WM_NCCALCSIZE {
        if wp.0 == 1 {
            // remove non-client area

            let params = unsafe {
                core::mem::transmute::<_, *mut NCCALCSIZE_PARAMS>(lp.0)
                    .as_mut()
                    .unwrap()
            };
            let w = unsafe { GetSystemMetrics(SM_CXSIZEFRAME) };
            let h = unsafe { GetSystemMetrics(SM_CYSIZEFRAME) };
            params.rgrc[0].left += w;
            params.rgrc[0].right -= w;
            params.rgrc[0].bottom -= h;
            // topはいじらない（他アプリもそんな感じになってるのでtopは自前でNCHITTESTしてリサイズ判定する）

            return LRESULT(0);
        }
    }
    if msg == WM_ACTIVATE {
        unsafe {
            DwmExtendFrameIntoClientArea(
                hwnd,
                &MARGINS {
                    cxLeftWidth: 1,
                    cxRightWidth: 1,
                    cyTopHeight: 1,
                    cyBottomHeight: 1,
                },
            )
            .expect("Failed to extend dwm frame");
        }

        return LRESULT(0);
    }
    if msg == WM_NCHITTEST {
        let app_window = AppWindow::wrap(hwnd);
        let Some(state) = app_window.get_state_store() else {
            return LRESULT(HTCLIENT as _);
        };

        let resize_h = unsafe { GetSystemMetrics(SM_CYSIZEFRAME) };
        let client_size = app_window
            .client_size_pixels()
            .expect("Failed to get client size");

        let (x, y) = (
            (lp.0 & 0xffff) as i16 as i32,
            ((lp.0 >> 16) & 0xffff) as i16 as i32,
        );
        let mut p = [POINT { x, y }];
        app_window.map_points_from_desktop(&mut p);
        let POINT { x, y } = p[0];

        if 0 > x || x > client_size.0 as i32 || 0 > y || y > client_size.1 as i32 {
            // ウィンドウ範囲外はシステムにおまかせ
            return unsafe { DefWindowProcA(hwnd, msg, wp, lp) };
        }
        if y < resize_h {
            // global override
            return LRESULT(HTTOP as _);
        }

        let ht = state.input_state.nc_hittest(
            app_window.pixels_to_dip(x as _),
            app_window.pixels_to_dip(y as _),
        );
        return LRESULT(ht as _);
    }
    if msg == WM_MOUSEMOVE || msg == WM_NCMOUSEMOVE {
        let app_window = AppWindow::wrap(hwnd);
        let Some(state) = app_window.get_state_store() else {
            return LRESULT(0);
        };

        let (x, y) = ((lp.0 & 0xffff) as i16, ((lp.0 >> 16) & 0xffff) as i16);
        let (x, y) = if msg == WM_NCMOUSEMOVE {
            // NCのときは画面上での座標になる
            let mut p = [POINT {
                x: x as _,
                y: y as _,
            }];
            app_window.map_points_from_desktop(&mut p);
            (p[0].x, p[0].y)
        } else {
            (x as _, y as _)
        };
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
        state.app_title_bar_view.borrow().set_width(w);
        state
            .pane_group_docking_manager
            .borrow_mut()
            .resize_root(w, h - AppTitleBarView::HEIGHT)
            .expect("Failed to resize root");

        let maximized = app_window
            .is_maximized()
            .expect("Failed to query maximized state");
        if maximized != state.currently_maximized {
            // split borrowing
            let title_bar_view = state.app_title_bar_view.clone();

            title_bar_view
                .borrow()
                .change_maximize_restore_icon(maximized, state)
                .expect("Failed to change maxres icon");

            state.currently_maximized = maximized;
        }

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
