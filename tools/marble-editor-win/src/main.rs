use std::{
    borrow::Cow,
    cell::RefCell,
    collections::{BTreeSet, HashMap, HashSet},
    hash::Hash,
    rc::{Rc, Weak},
    sync::{Arc, Weak as AtomicWeak},
};

use app_global_signals::{AppGlobalSignals, SignalEventReceiver, SignalEventType};
use app_subsystem_instances::AppSubsystemInstances;
use bedrock::{self as br, ImageChild, VkObject};
use br::{
    CommandBuffer, CommandPool, DescriptorPool, Device, GraphicsPipelineBuilder,
    ImageSubresourceSlice, MemoryBound, PipelineShaderStageProvider, Queue, SubmissionBatch,
};
use components::{FloatSliderView, LabelView, RollableNumberView};
use features::{
    AppTitleBarView, ContextMenu, DockingPanePreview, MenuItem, PaneSplitterView, SplitDirection,
};
use miniengine::{
    ColoredVertex, GenericVertex, Mat4, SamplerDesc, StdVkDevice, TempRT, UtilityVertices, Vec4,
};
use observable::ObservationDisconnector;
use parking_lot::RwLock;
use peridot_math::{Camera, One, ProjectionMethod};
use uikit::{
    HitTestTree, InputContext, InputEventHandler, InputState, MountableView2, ResizeContext,
    ViewContext,
};
use utils::{
    rect_slice_bottom, rect_slice_left, rect_slice_right, rect_slice_top, EventHandle,
    RectExtensions,
};
use uuid::Uuid;
use winapi_extras::{
    timespan_ms, KeyFrameAnimationExtension, KeyFrameAnimationPropertySetterExtension,
    Vector2Extension, VisualExtensions,
};
use windows::{
    core::*,
    Foundation::{
        Numerics::{Vector2, Vector3},
        Rect, TimeSpan,
    },
    Win32::{
        Foundation::{BOOL, GENERIC_ALL, HANDLE, HWND, LPARAM, LRESULT, POINT, RECT, WPARAM},
        Graphics::{
            CompositionSwapchain::{
                IPresentationBuffer, IPresentationManager, IPresentationSurface,
            },
            Direct3D11::{
                ID3D11Device, ID3D11DeviceContext, ID3D11Texture2D, D3D11_BIND_RENDER_TARGET,
                D3D11_BIND_SHADER_RESOURCE, D3D11_RESOURCE_MISC_SHARED,
                D3D11_RESOURCE_MISC_SHARED_DISPLAYABLE, D3D11_RESOURCE_MISC_SHARED_KEYEDMUTEX,
                D3D11_RESOURCE_MISC_SHARED_NTHANDLE, D3D11_TEXTURE2D_DESC, D3D11_USAGE_DEFAULT,
            },
            DirectComposition::{
                DCompositionCreateSurfaceHandle, COMPOSITIONOBJECT_READ, COMPOSITIONOBJECT_WRITE,
            },
            DirectWrite::{IDWriteTextFormat, DWRITE_FONT_WEIGHT_NORMAL},
            Dwm::{DwmExtendFrameIntoClientArea, DwmSetWindowAttribute, DWMWINDOWATTRIBUTE},
            Dxgi::{
                Common::{
                    DXGI_ALPHA_MODE_IGNORE, DXGI_COLOR_SPACE_RGB_FULL_G22_NONE_P709,
                    DXGI_FORMAT_R8G8B8A8_UNORM, DXGI_SAMPLE_DESC,
                },
                IDXGIKeyedMutex, IDXGIResource1, DXGI_SHARED_RESOURCE_READ,
                DXGI_SHARED_RESOURCE_WRITE,
            },
            Gdi::{MapWindowPoints, HBRUSH},
        },
        Storage::Packaging::Appx::PACKAGE_VERSION,
        System::{
            LibraryLoader::{GetModuleHandleA, GetProcAddress, LoadLibraryA},
            Threading::INFINITE,
            WinRT::{
                Composition::ICompositorDesktopInterop, CreateDispatcherQueueController,
                DispatcherQueueOptions, DQTAT_COM_ASTA, DQTYPE_THREAD_CURRENT,
            },
        },
        UI::{
            Controls::{HOVER_DEFAULT, MARGINS, WM_MOUSELEAVE},
            HiDpi::GetDpiForWindow,
            Input::KeyboardAndMouse::{
                TrackMouseEvent, TME_LEAVE, TME_NONCLIENT, TRACKMOUSEEVENT, TRACKMOUSEEVENT_FLAGS,
            },
            WindowsAndMessaging::{
                DefWindowProcA, DispatchMessageA, GetClientRect, GetSystemMetrics,
                GetWindowLongPtrA, GetWindowPlacement, GetWindowRect, LoadCursorA, LoadIconA,
                PeekMessageA, PostQuitMessage, SetCursorPos, SetWindowLongPtrA, SetWindowPos,
                ShowCursor, ShowWindow, TranslateMessage, HTCLIENT, HTTOP, IDC_ARROW,
                IDI_APPLICATION, MSG, NCCALCSIZE_PARAMS, PM_REMOVE, SM_CXSIZEFRAME, SM_CYSIZEFRAME,
                SWP_FRAMECHANGED, SW_MAXIMIZE, SW_SHOWNORMAL, WHEEL_DELTA, WINDOWPLACEMENT,
                WINDOW_LONG_PTR_INDEX, WM_ACTIVATE, WM_CREATE, WM_DESTROY, WM_KILLFOCUS,
                WM_LBUTTONDOWN, WM_LBUTTONUP, WM_MBUTTONDOWN, WM_MBUTTONUP, WM_MOUSEMOVE,
                WM_MOUSEWHEEL, WM_NCCALCSIZE, WM_NCHITTEST, WM_NCMOUSELEAVE, WM_NCMOUSEMOVE,
                WM_QUIT, WM_RBUTTONUP, WM_SETCURSOR, WM_WINDOWPOSCHANGED, WNDCLASSEXA,
                WNDCLASS_STYLES,
            },
        },
    },
    UI::{
        Color,
        Composition::{
            CompositionRoundedRectangleGeometry, CompositionSurfaceBrush, ContainerVisual,
            Diagnostics::CompositionDebugSettings, LayerVisual, ScalarKeyFrameAnimation,
            ShapeVisual, SpriteVisual, VisualCollection,
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
    uikit::ViewContext1,
    winapi_extras::{register_window_class, VectorScalarConstructor, WindowBuilder},
};

mod app_global_signals;
mod app_subsystem_instances;
mod bindgen;
mod components;
mod features;
mod miniengine;
mod object_cache;
mod observable;
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

type MTSharedMut<T> = Arc<RwLock<T>>;
type MTWeakMut<T> = AtomicWeak<RwLock<T>>;
#[inline]
fn new_mt_shared_mut<T>(value: T) -> MTSharedMut<T> {
    Arc::new(RwLock::new(value))
}
#[inline]
fn new_cyclic_mt_shared_mut<T>(ctor: impl FnOnce(&MTWeakMut<T>) -> T) -> MTSharedMut<T> {
    Arc::new_cyclic(|w| RwLock::new(ctor(w)))
}
#[inline]
const fn empty_mt_weak_mut<T>() -> MTWeakMut<T> {
    AtomicWeak::new()
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

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum DockDirection {
    Left,
    Right,
    Top,
    Bottom,
}
pub enum PaneDockLayer {
    EmptyRoot(Option<SharedMut<PaneDockLayer>>, Rect),
    Docked {
        direction: DockDirection,
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
        new_cyclic_shared_mut(|wthis| Self::EmptyRoot(content(wthis), Rect::empty()))
    }

    fn new_on<VC: ViewContext + ?Sized>(
        direction: DockDirection,
        parent: &WeakMut<Self>,
        docked: impl FnOnce(&WeakMut<Self>, &VC) -> SharedMut<Self>,
        rest: impl FnOnce(&WeakMut<Self>, &VC) -> SharedMut<Self>,
        ctx: &VC,
    ) -> windows::core::Result<SharedMut<Self>> {
        let splitter = PaneSplitterView::new(match direction {
            DockDirection::Left | DockDirection::Right => SplitDirection::Vertical,
            DockDirection::Top | DockDirection::Bottom => SplitDirection::Horizontal,
        })?;

        Ok(new_cyclic_shared_mut(|wthis| {
            splitter.borrow_mut().bind_dock_layer(wthis);

            Self::Docked {
                direction,
                docked: docked(wthis, ctx),
                splitter,
                container_region: Rect::empty(),
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
    pub const fn parent(&self) -> Option<&WeakMut<Self>> {
        match self {
            Self::Fill { parent, .. } | Self::Docked { parent, .. } => Some(parent),
            Self::EmptyRoot(_, _) => None,
        }
    }

    pub fn controlling_rect(&self) -> Rect {
        match self {
            Self::EmptyRoot(_, container_region)
            | Self::Docked {
                container_region, ..
            } => container_region.clone(),
            Self::Fill { inner_view, .. } => inner_view.borrow().view_rect.clone(),
        }
    }
    pub fn controlling_rect_left(&self) -> f32 {
        match self {
            Self::EmptyRoot(_, container_region)
            | Self::Docked {
                container_region, ..
            } => container_region.X,
            Self::Fill { inner_view, .. } => inner_view.borrow().view_rect.X,
        }
    }
    pub fn controlling_rect_right(&self) -> f32 {
        match self {
            Self::EmptyRoot(_, container_region)
            | Self::Docked {
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
            | Self::Docked {
                container_region, ..
            } => container_region.Y,
            Self::Fill { inner_view, .. } => inner_view.borrow().view_rect.Y,
        }
    }
    pub fn controlling_rect_bottom(&self) -> f32 {
        match self {
            Self::EmptyRoot(_, container_region)
            | Self::Docked {
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
            | Self::Docked {
                container_region, ..
            } => container_region.Width,
            Self::Fill { inner_view, .. } => inner_view.borrow().view_rect.Width,
        }
    }
    pub fn controlling_rect_height(&self) -> f32 {
        match self {
            Self::EmptyRoot(_, container_region)
            | Self::Docked {
                container_region, ..
            } => container_region.Height,
            Self::Fill { inner_view, .. } => inner_view.borrow().view_rect.Height,
        }
    }

    #[inline]
    fn split_region(
        direction: DockDirection,
        region: Rect,
        docked_size: f32,
    ) -> (Rect, Rect, Rect) {
        match direction {
            DockDirection::Left => Self::split_left(region, docked_size),
            DockDirection::Right => Self::split_right(region, docked_size),
            DockDirection::Top => Self::split_top(region, docked_size),
            DockDirection::Bottom => Self::split_bottom(region, docked_size),
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
            Self::EmptyRoot(_, _) | Self::Fill { .. } => 0.0,
            Self::Docked {
                direction, docked, ..
            } => match direction {
                DockDirection::Left | DockDirection::Right => {
                    docked.borrow().controlling_rect_width()
                }
                DockDirection::Top | DockDirection::Bottom => {
                    docked.borrow().controlling_rect_height()
                }
            },
        }
    }

    /// returns new split bar position
    pub fn set_dock_size(
        &mut self,
        size: f32,
        resize_ctx: &ResizeContext,
    ) -> windows::core::Result<(f32, f32)> {
        match self {
            Self::EmptyRoot(_, _) | Self::Fill { .. } => {
                // nop for root/filling container
                Ok((0.0, 0.0))
            }
            Self::Docked {
                direction,
                docked,
                container_region,
                rest,
                ..
            } => {
                let (docked_rect, splitter_rect, rest_rect) =
                    Self::split_region(*direction, container_region.clone(), size);
                docked.borrow_mut().layout(docked_rect, resize_ctx)?;
                rest.borrow_mut().layout(rest_rect, resize_ctx)?;

                Ok((splitter_rect.X, splitter_rect.Y))
            }
        }
    }

    fn reparent(&mut self, new_parent: &Weak<RefCell<Self>>) {
        match self {
            Self::EmptyRoot(_, _) => (),
            Self::Docked { parent, .. } | Self::Fill { parent, .. } => *parent = new_parent.clone(),
        }
    }

    fn replace_child(&mut self, old_child_ref: &SharedMut<Self>, new_child: &SharedMut<Self>) {
        match self {
            Self::EmptyRoot(r, _) => *r = Some(new_child.clone()),
            Self::Docked { docked, .. } if Rc::ptr_eq(docked, old_child_ref) => {
                *docked = new_child.clone();
            }
            Self::Docked { rest, .. } if Rc::ptr_eq(rest, old_child_ref) => {
                *rest = new_child.clone();
            }
            _ => unreachable!("invalid tree or op"),
        }
    }

    fn mount_recursive(
        &self,
        onto: &VisualCollection,
        onto_ht: &HitTestTree,
    ) -> windows::core::Result<()> {
        match self {
            // no child
            Self::EmptyRoot(None, _) => Ok(()),
            Self::EmptyRoot(Some(r), _) => r.borrow().mount_recursive(onto, onto_ht),
            Self::Docked {
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

    fn relayout(&mut self, resize_ctx: &ResizeContext) -> windows::core::Result<()> {
        let region = self.controlling_rect();
        match self {
            // no child
            Self::EmptyRoot(None, r) => {
                *r = region;
                Ok(())
            }
            Self::EmptyRoot(Some(r), rect) => {
                *rect = region.clone();
                r.borrow_mut().layout(region, resize_ctx)
            }
            Self::Docked {
                direction,
                docked,
                splitter,
                container_region,
                rest,
                ..
            } => {
                *container_region = region.clone();
                let docked_size = match direction {
                    DockDirection::Left | DockDirection::Right => {
                        docked.borrow().controlling_rect_width()
                    }
                    DockDirection::Top | DockDirection::Bottom => {
                        docked.borrow().controlling_rect_height()
                    }
                };
                let (docked_rect, splitter_rect, rest_rect) =
                    Self::split_region(*direction, region, docked_size);
                docked.borrow_mut().layout(docked_rect, resize_ctx)?;
                splitter.borrow().set_rect(splitter_rect)?;
                rest.borrow_mut().layout(rest_rect, resize_ctx)
            }
            Self::Fill { inner_view, .. } => inner_view.borrow_mut().set_rect(region, resize_ctx),
        }
    }

    fn layout(&mut self, region: Rect, resize_ctx: &ResizeContext) -> windows::core::Result<()> {
        match self {
            Self::EmptyRoot(None, r) => {
                // no child
                *r = region;
                Ok(())
            }
            Self::EmptyRoot(Some(r), rect) => {
                *rect = region.clone();
                r.borrow_mut().layout(region, resize_ctx)
            }
            Self::Docked {
                direction,
                docked,
                splitter,
                container_region,
                rest,
                ..
            } => {
                if *container_region == region {
                    // same size
                    return Ok(());
                }

                *container_region = region.clone();
                let docked_size = match direction {
                    DockDirection::Left | DockDirection::Right => {
                        docked.borrow().controlling_rect_width()
                    }
                    DockDirection::Top | DockDirection::Bottom => {
                        docked.borrow().controlling_rect_height()
                    }
                };
                let (docked_rect, splitter_rect, rest_rect) =
                    Self::split_region(*direction, region, docked_size);
                docked.borrow_mut().layout(docked_rect, resize_ctx)?;
                splitter.borrow().set_rect(splitter_rect)?;
                rest.borrow_mut().layout(rest_rect, resize_ctx)
            }
            Self::Fill { inner_view, .. } => inner_view.borrow_mut().set_rect(region, resize_ctx),
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
            Self::Docked {
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
            Self::Docked {
                direction: DockDirection::Left,
                docked,
                rest,
                ..
            } => {
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
            Self::Docked {
                direction: DockDirection::Right,
                docked,
                rest,
                ..
            } => {
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
            Self::Docked {
                direction: DockDirection::Top,
                docked,
                rest,
                ..
            } => {
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
            Self::Docked {
                direction: DockDirection::Bottom,
                docked,
                rest,
                ..
            } => {
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
    ht_placement_root: HitTestTree,
    floating_preview: DockingPanePreview,
}
impl PaneGroupDockingManager {
    fn new(ht_root: &HitTestTree) -> windows::core::Result<Self> {
        let ht_placement_root = HitTestTree::new_unsized(Some(()), 0.0, 0.0);
        ht_root.add_child(&ht_placement_root);

        Ok(Self {
            docks: PaneDockLayer::new_root(|_| None),
            placement_visual: AppSubsystemInstances::get()
                .compositor
                .CreateContainerVisual()?,
            ht_placement_root,
            floating_preview: DockingPanePreview::new()?,
        })
    }

    fn set_layout(&mut self, layout: SharedMut<PaneDockLayer>) -> windows::core::Result<()> {
        let children = self.placement_visual.Children()?;
        children.RemoveAll()?;
        self.ht_placement_root.remove_all_children();
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
        self.ht_placement_root.set_offset(left, top);

        Ok(())
    }
    #[inline]
    fn offset(&self) -> (f32, f32) {
        (
            self.ht_placement_root.rect().X,
            self.ht_placement_root.rect().Y,
        )
    }
    fn resize_root(
        &mut self,
        width: f32,
        height: f32,
        resize_ctx: &ResizeContext,
    ) -> windows::core::Result<()> {
        self.docks
            .borrow_mut()
            .layout(Rect::from_size(width, height), resize_ctx)?;

        Ok(())
    }
    fn mount_splitter_only(&self, layout: &PaneDockLayer) -> windows::core::Result<()> {
        match layout {
            PaneDockLayer::EmptyRoot(_, _) | PaneDockLayer::Fill { .. } => Ok(()),
            PaneDockLayer::Docked { splitter, .. } => splitter
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
    ht_ref: HitTestTree,
    ht_ref_content: HitTestTree,
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
        A: 64,
        R: 64,
        G: 64,
        B: 72,
    };

    pub fn new(
        docking_manager: &SharedMut<PaneGroupDockingManager>,
    ) -> windows::core::Result<SharedMut<Self>> {
        let root = AppSubsystemInstances::get()
            .compositor
            .CreateContainerVisual()?;
        root.SetSize(Vector2::scalar(128.0))?;

        let content_area = AppSubsystemInstances::get()
            .compositor
            .CreateContainerVisual()?;
        root.Children()?.InsertAtBottom(&content_area)?;

        let content_area_base = AppSubsystemInstances::get()
            .compositor
            .CreateSpriteVisual()?;
        content_area_base.set_properties().brush(
            &AppSubsystemInstances::get()
                .compositor
                .CreateColorBrushWithColor(Self::CONTENT_AREA_BASE_COLOR)?,
        )?;
        root.Children()?.InsertAtBottom(&content_area_base)?;
        // root.SetClip(
        //     &ctx.app_subsystems()
        //         .borrow()
        //         .compositor
        //         .CreateInsetClipWithInsets(0.0, 0.0, 0.0, 0.0)?,
        // )?;

        Ok(new_cyclic_shared_mut(|wthis| {
            let ht = HitTestTree::new(
                Some(wthis.clone()),
                Rect::from_size(128.0, 128.0),
                Rect::empty(),
            );
            let ht_content = HitTestTree::new(
                Some(wthis.clone()),
                Rect::from_size(128.0, 128.0),
                Rect::empty(),
            );
            ht.add_child(&ht_content);

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
                view_rect: Rect::from_size(128.0, 128.0),
                tabs: Vec::new(),
                drag_base_point: None,
                preview_rect: Rect::empty(),
            }
        }))
    }
    pub fn bind_dock_layer(&mut self, layer: &WeakMut<PaneDockLayer>) {
        self.bound_dock_layer = layer.clone();
    }

    pub fn add_tab<T: PaneTabPresenter + 'static>(
        this: &SharedMut<Self>,
        ctx: &(impl ViewContext + ?Sized),
        app_state: &MTSharedMut<AppState>,
    ) -> windows::core::Result<SharedMut<T>> {
        // 最初のタブなら初手active
        let init_active = this.borrow().tabs.is_empty();
        let header_view = PaneTabHeaderView::new(T::INIT_TAB_NAME, init_active, ctx)?;
        let content_presenter = new_shared_mut(T::new(&header_view, ctx, app_state));
        Self::add_tab_raw(this, header_view, content_presenter.clone())?;

        if init_active {
            // first tab
            let thisref = this.borrow();
            content_presenter.borrow_mut().build_content_view(
                &thisref.content_area,
                &thisref.ht_ref_content,
                &ctx,
                app_state,
            )?;
            content_presenter
                .borrow_mut()
                .on_resize(thisref.ht_ref_content.size(), &ctx.resize_context())?;
        }

        Ok(content_presenter)
    }

    fn add_tab_raw(
        this: &SharedMut<Self>,
        header: SharedMut<PaneTabHeaderView>,
        content: SharedMut<dyn PaneTabContentPresenter>,
    ) -> windows::core::Result<usize> {
        let new_index = this.borrow().tabs.len();
        header.borrow_mut().bind_group_view(this, new_index);
        header
            .borrow()
            .mount(&this.borrow().root.Children()?, &this.borrow().ht_ref)?;

        this.borrow_mut().tabs.push((header, content));
        Ok(new_index)
    }

    fn remove_tab(
        &mut self,
        tab: &SharedMut<PaneTabHeaderView>,
        view_ctx: &(impl ViewContext + ?Sized),
        app_state: &MTSharedMut<AppState>,
    ) -> windows::core::Result<
        Option<(
            SharedMut<PaneTabHeaderView>,
            SharedMut<dyn PaneTabContentPresenter>,
        )>,
    > {
        let Some(index) = self.tabs.iter().position(|(h, _)| Rc::ptr_eq(h, tab)) else {
            // 対応するタブがない
            return Ok(None);
        };

        if tab.borrow().is_active {
            // アクティブを付け替える（0個になる場合はタブの非アクティブ化だけ）
            if self.tabs.len() > 1 {
                let new_active = if index == 0 { 1 } else { index - 1 };
                self.switch_active(new_active, view_ctx, app_state)?;
            } else {
                self.inactive_current(view_ctx, app_state, PaneTabTransitionMode::Normal)?;
            }
        }
        tab.borrow().unmount()?;

        Ok(Some(self.tabs.remove(index)))
    }

    pub fn move_tab_into(
        &mut self,
        tab: &SharedMut<PaneTabHeaderView>,
        target: &SharedMut<Self>,
        view_ctx: &(impl ViewContext + ?Sized),
        app_state: &MTSharedMut<AppState>,
    ) -> windows::core::Result<()> {
        let Some((tab, content)) = self.remove_tab(tab, view_ctx, app_state)? else {
            // 対応するタブがない
            return Ok(());
        };

        let new_tab_index = Self::add_tab_raw(target, tab, content)?;
        // activate this tab
        target
            .borrow_mut()
            .switch_active(new_tab_index, view_ctx, app_state)?;

        Ok(())
    }

    pub fn split_tab(
        &mut self,
        tab: &SharedMut<PaneTabHeaderView>,
        view_ctx: &(impl ViewContext + ?Sized),
        app_state: &MTSharedMut<AppState>,
    ) -> windows::core::Result<Option<SharedMut<Self>>> {
        let Some((tab, content)) = self.remove_tab(tab, view_ctx, app_state)? else {
            // 対応するタブがない
            return Ok(None);
        };

        let new_group = Self::new(
            &self
                .docking_manager
                .upgrade()
                .expect("Docking Manager has dead"),
        )?;
        Self::add_tab_raw(&new_group, tab.clone(), content.clone())?;

        // activate first
        new_group.borrow_mut().current_active = 0;
        new_group.borrow_mut().active_current(
            &view_ctx,
            app_state,
            PaneTabTransitionMode::Immediate,
        )?;

        Ok(Some(new_group))
    }

    fn readjust_content_area(&mut self, resize_ctx: &ResizeContext) -> windows::core::Result<()> {
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
        self.ht_ref_content.set_rect(
            content_area.X,
            content_area.Y,
            content_area.Width,
            content_area.Height,
        );

        self.current_content()
            .borrow_mut()
            .on_resize(content_area.size(), resize_ctx)?;

        Ok(())
    }

    pub fn rearrange(&mut self, resize_ctx: &ResizeContext) {
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

        self.readjust_content_area(resize_ctx)
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

    pub fn set_width(
        &mut self,
        width: f32,
        resize_ctx: &ResizeContext,
    ) -> windows::core::Result<()> {
        self.root.SetSize(Vector2 {
            X: width,
            Y: self.view_rect.Height,
        })?;
        self.ht_ref.set_size(width, self.view_rect.Height);
        self.view_rect.Width = width;

        self.readjust_content_area(resize_ctx)?;
        Ok(())
    }
    pub fn set_height(
        &mut self,
        height: f32,
        resize_ctx: &ResizeContext,
    ) -> windows::core::Result<()> {
        self.root.SetSize(Vector2 {
            X: self.view_rect.Width,
            Y: height,
        })?;
        self.ht_ref.set_size(self.view_rect.Width, height);
        self.view_rect.Height = height;

        self.readjust_content_area(resize_ctx)?;
        Ok(())
    }
    pub fn resize(
        &mut self,
        width: f32,
        height: f32,
        resize_ctx: &ResizeContext,
    ) -> windows::core::Result<()> {
        self.root.SetSize(Vector2 {
            X: width,
            Y: height,
        })?;
        self.ht_ref.set_size(width, height);
        self.view_rect.Width = width;
        self.view_rect.Height = height;

        self.readjust_content_area(resize_ctx)?;
        Ok(())
    }
    pub fn set_rect(
        &mut self,
        rect: Rect,
        resize_ctx: &ResizeContext,
    ) -> windows::core::Result<()> {
        self.root.set_properties().rect(&rect)?;
        self.ht_ref
            .set_rect(rect.X, rect.Y, rect.Width, rect.Height);
        self.view_rect = rect;

        self.readjust_content_area(resize_ctx)?;
        Ok(())
    }

    fn inactive_current(
        &mut self,
        view_ctx: &(impl ViewContext + ?Sized),
        app_state: &MTSharedMut<AppState>,
        tab_transition_mode: PaneTabTransitionMode,
    ) -> windows::core::Result<()> {
        let (tab, content) = &self.tabs[self.current_active];

        content
            .borrow_mut()
            .on_hide_content_view(&view_ctx, app_state)?;
        tab.borrow_mut().set_active(false, tab_transition_mode)?;
        self.content_area.Children()?.RemoveAll()?;

        Ok(())
    }

    fn active_current(
        &mut self,
        view_ctx: &(impl ViewContext + ?Sized),
        app_state: &MTSharedMut<AppState>,
        tab_transition_mode: PaneTabTransitionMode,
    ) -> windows::core::Result<()> {
        let (tab, content) = &self.tabs[self.current_active];

        content.borrow_mut().build_content_view(
            &self.content_area,
            &self.ht_ref_content,
            &view_ctx,
            app_state,
        )?;
        content
            .borrow_mut()
            .on_resize(self.ht_ref_content.size(), &view_ctx.resize_context())?;
        tab.borrow_mut().set_active(true, tab_transition_mode)?;

        Ok(())
    }

    pub fn switch_active(
        &mut self,
        new_active: usize,
        view_ctx: &(impl ViewContext + ?Sized),
        app_state: &MTSharedMut<AppState>,
    ) -> windows::core::Result<()> {
        let new_active = new_active.min(self.tabs.len());
        if self.current_active == new_active {
            // 変わってないのでなにもしない
            return Ok(());
        }

        self.inactive_current(view_ctx, app_state, PaneTabTransitionMode::Normal)?;
        self.current_active = new_active;
        self.active_current(view_ctx, app_state, PaneTabTransitionMode::Normal)?;

        Ok(())
    }

    pub fn current_content(&self) -> &SharedMut<dyn PaneTabContentPresenter> {
        &self.tabs[self.current_active].1
    }

    pub fn mount(
        &self,
        onto: &VisualCollection,
        onto_ht: &HitTestTree,
    ) -> windows::core::Result<()> {
        onto.InsertAtTop(&self.root)?;
        onto_ht.add_child(&self.ht_ref);

        Ok(())
    }
    pub fn unmount(&self) -> windows::core::Result<()> {
        self.root.Parent()?.Children()?.Remove(&self.root)?;
        self.ht_ref.unmount();

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
        let rect = thisref.ht_ref.rect().clone();

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
                    let new_layer = PaneDockLayer::new_on(
                        DockDirection::Left,
                        &Rc::downgrade(&dest_parent),
                        |parent, _ctx| PaneDockLayer::new_filled(&this, parent),
                        |parent, _ctx| {
                            d.borrow_mut().reparent(parent);
                            d.clone()
                        },
                        &mut *ctx,
                    )
                    .expect("Failed to create new dock layer");
                    docking_manager
                        .borrow()
                        .mount_splitter_only(&new_layer.borrow())
                        .expect("Failed to mount new splitter");
                    dest_parent.borrow_mut().replace_child(&d, &new_layer);
                    dest_parent
                        .borrow_mut()
                        .relayout(&ctx.make_resize_context())
                        .expect("Failed to relayout from new parent");
                    relayout_root
                        .borrow_mut()
                        .relayout(&ctx.make_resize_context())
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
                    let new_layer = PaneDockLayer::new_on(
                        DockDirection::Right,
                        &Rc::downgrade(&dest_parent),
                        |parent, _ctx| PaneDockLayer::new_filled(&this, parent),
                        |parent, _ctx| {
                            d.borrow_mut().reparent(parent);
                            d.clone()
                        },
                        &mut *ctx,
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
                        .relayout(&ctx.make_resize_context())
                        .expect("Failed to relayout from new parent");
                    relayout_root
                        .borrow_mut()
                        .relayout(&ctx.make_resize_context())
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
                    let new_layer = PaneDockLayer::new_on(
                        DockDirection::Top,
                        &Rc::downgrade(&dest_parent),
                        |parent, _ctx| PaneDockLayer::new_filled(&this, parent),
                        |parent, _ctx| {
                            d.borrow_mut().reparent(parent);
                            d.clone()
                        },
                        &mut *ctx,
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
                        .relayout(&ctx.make_resize_context())
                        .expect("Failed to relayout from new parent");
                    relayout_root
                        .borrow_mut()
                        .relayout(&ctx.make_resize_context())
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
                    let new_layer = PaneDockLayer::new_on(
                        DockDirection::Bottom,
                        &Rc::downgrade(&dest_parent),
                        |parent, _ctx| PaneDockLayer::new_filled(&this, parent),
                        |parent, _ctx| {
                            d.borrow_mut().reparent(parent);
                            d.clone()
                        },
                        &mut *ctx,
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
                        .relayout(&ctx.make_resize_context())
                        .expect("Failed to relayout from new parent");
                    relayout_root
                        .borrow_mut()
                        .relayout(&ctx.make_resize_context())
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
                    .relayout(&ctx.make_resize_context())
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

    fn on_sub_pointer_up(&self, x: f32, y: f32, window: HWND, ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };

        let mut p = [POINT {
            x: x as _,
            y: y as _,
        }];
        unsafe {
            MapWindowPoints(window, None, &mut p);
        }
        this.borrow()
            .current_content()
            .borrow_mut()
            .on_context_menu(p[0].x as _, p[0].y as _, ctx);
    }
}

#[derive(Clone, Copy)]
pub enum PaneTabTransitionMode {
    Normal,
    Immediate,
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
    hittest_tree_self: HitTestTree,
    rendered_dpi: f32,
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
    ) -> windows::core::Result<CompositionRoundedRectangleGeometry> {
        let g = AppSubsystemInstances::get()
            .compositor
            .CreateRoundedRectangleGeometry()?;
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

    #[inline]
    fn font(active: bool) -> IDWriteTextFormat {
        if active {
            AppSubsystemInstances::get()
                .ui_common_objects
                .tab_active_title_font
                .clone()
        } else {
            AppSubsystemInstances::get()
                .ui_common_objects
                .tab_title_font
                .clone()
        }
    }

    pub fn new(
        title: impl Into<Cow<'static, str>>,
        init_active: bool,
        ctx: &(impl ViewContext + ?Sized),
    ) -> windows::core::Result<SharedMut<Self>> {
        let base = AppSubsystemInstances::get()
            .compositor
            .CreateLayerVisual()?;

        let title = title.into();
        let font = Self::font(init_active);
        let title_text = AppSubsystemInstances::get()
            .text_surface_stock
            .borrow_mut()
            .get(&font, ctx.current_dpi(), title.clone())?;
        let view_size = Vector2 {
            X: title_text.width + TAB_MARGIN_X * 2.0,
            Y: title_text.height + TAB_MARGIN_Y * 2.0,
        };
        let label_content_brush = AppSubsystemInstances::get()
            .compositor
            .CreateSurfaceBrushWithSurface(&title_text.surface)?;
        base.Children()?.InsertAtTop(&{
            let v = AppSubsystemInstances::get()
                .compositor
                .CreateSpriteVisual()?;
            v.set_properties()
                .brush(&label_content_brush)?
                .size(title_text.visual_size())?
                .anchor_point(Vector2::scalar(0.5))?
                .offset(Vector3 {
                    X: title_text.width * 0.5 + TAB_MARGIN_X,
                    Y: title_text.height * 0.5 + TAB_MARGIN_Y,
                    Z: 0.0,
                })?;

            v
        })?;

        let geometry = Self::create_geometry(title_text.width, title_text.height)?;
        let bg = {
            let shape = AppSubsystemInstances::get()
                .compositor
                .CreateSpriteShapeWithGeometry(&geometry)?;
            shape.SetFillBrush(
                &AppSubsystemInstances::get()
                    .ui_common_objects
                    .tab_base_brush,
            )?;

            let v = AppSubsystemInstances::get()
                .compositor
                .CreateShapeVisual()?;
            v.Shapes()?.Append(&shape)?;
            v.SetSize(view_size.clone())?;
            v
        };
        let active_overlay = {
            let shape = AppSubsystemInstances::get()
                .compositor
                .CreateSpriteShapeWithGeometry(&geometry)?;
            shape.SetFillBrush(
                &AppSubsystemInstances::get()
                    .ui_common_objects
                    .tab_active_overlay_brush,
            )?;

            let v = AppSubsystemInstances::get()
                .compositor
                .CreateShapeVisual()?;
            v.Shapes()?.Append(&shape)?;
            v.SetSize(view_size.clone())?;
            v
        };

        let init_opacity = if init_active { 1.0 } else { 0.0 };
        bg.SetOpacity(init_opacity)?;
        active_overlay.SetOpacity(init_opacity)?;

        let children = base.Children()?;
        children.InsertAtTop(&bg)?;
        children.InsertAtTop(&active_overlay)?;

        Ok(new_cyclic_shared_mut(|wthis| {
            let ht_self = HitTestTree::new(
                Some(wthis.clone()),
                Rect::from_size(view_size.X, view_size.Y),
                Rect::empty(),
            );

            Self {
                group_view: empty_weak_mut(),
                index_in_group: 0,
                label: title,
                visual: base,
                bg_visual: bg,
                active_overlay_visual: active_overlay,
                label_content_brush,
                bg_hover_animation: AppSubsystemInstances::get()
                    .ui_common_objects
                    .tab_hover_animation
                    .clone(),
                bg_hover_end_animation: AppSubsystemInstances::get()
                    .ui_common_objects
                    .tab_hover_end_animation
                    .clone(),
                active_overlay_enter_animation: AppSubsystemInstances::get()
                    .ui_common_objects
                    .tab_active_overlay_enter_animation
                    .clone(),
                active_overlay_leave_animation: AppSubsystemInstances::get()
                    .ui_common_objects
                    .tab_active_overlay_leave_animation
                    .clone(),
                hittest_tree_self: ht_self,
                rendered_dpi: ctx.current_dpi(),
                bg_active: init_active,
                is_active: init_active,
                width: view_size.X,
                height: view_size.Y,
                drag_base_point: None,
                preview_rect: Rect::from_size(view_size.X, view_size.Y),
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

    pub fn set_offset(&self, left: f32, top: f32) -> windows::core::Result<()> {
        self.visual.SetOffset(Vector3 {
            X: left,
            Y: top,
            Z: 0.0,
        })?;
        self.hittest_tree_self.set_offset(left, top);

        Ok(())
    }

    fn activate_bg(&mut self, mode: PaneTabTransitionMode) -> windows::core::Result<()> {
        if self.bg_active {
            // すでにアクティブ
            return Ok(());
        }

        match mode {
            PaneTabTransitionMode::Normal => self
                .bg_visual
                .StartAnimation(h!("Opacity"), &self.bg_hover_animation)?,
            PaneTabTransitionMode::Immediate => self.bg_visual.SetOpacity(1.0)?,
        }

        self.bg_active = true;
        Ok(())
    }
    fn deactivate_bg(&mut self, mode: PaneTabTransitionMode) -> windows::core::Result<()> {
        if !self.bg_active {
            // すでに非アクティブ
            return Ok(());
        }

        if self.is_active {
            // アクティブ状態のときは背景は非アクティブにできない
            return Ok(());
        }

        match mode {
            PaneTabTransitionMode::Normal => self
                .bg_visual
                .StartAnimation(h!("Opacity"), &self.bg_hover_end_animation)?,
            PaneTabTransitionMode::Immediate => self.bg_visual.SetOpacity(0.0)?,
        }

        self.bg_active = false;
        Ok(())
    }

    pub fn set_active(
        &mut self,
        is_active: bool,
        mode: PaneTabTransitionMode,
    ) -> windows::core::Result<()> {
        let requires_transition = self.is_active != is_active;
        self.is_active = is_active;

        if is_active {
            self.activate_bg(mode)?;
        } else {
            self.deactivate_bg(mode)?;
        }

        if requires_transition {
            match mode {
                PaneTabTransitionMode::Normal => self.active_overlay_visual.StartAnimation(
                    h!("Opacity"),
                    if is_active {
                        &self.active_overlay_enter_animation
                    } else {
                        &self.active_overlay_leave_animation
                    },
                )?,
                PaneTabTransitionMode::Immediate => self
                    .active_overlay_visual
                    .SetOpacity(if is_active { 1.0 } else { 0.0 })?,
            }

            let font = Self::font(is_active);
            let new_label_surface = AppSubsystemInstances::get()
                .text_surface_stock
                .borrow_mut()
                .get(&font, self.rendered_dpi, self.label.clone())?;
            self.label_content_brush
                .SetSurface(&new_label_surface.surface)?;
        }

        Ok(())
    }
}
impl MountableView2 for PaneTabHeaderView {
    fn mount(&self, onto: &VisualCollection, onto_ht: &HitTestTree) -> windows::core::Result<()> {
        onto.InsertAtTop(&self.visual)?;
        onto_ht.add_child(&self.hittest_tree_self);

        Ok(())
    }

    fn unmount(&self) -> windows::core::Result<()> {
        self.visual.Parent()?.Children()?.Remove(&self.visual)?;
        self.hittest_tree_self.unmount();

        Ok(())
    }
}
impl InputEventHandler for WeakMut<PaneTabHeaderView> {
    fn on_pointer_enter(&self, _ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };

        this.borrow_mut()
            .activate_bg(PaneTabTransitionMode::Normal)
            .expect("Failed to activate bg");
    }
    fn on_pointer_leave(&self, _ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };

        this.borrow_mut()
            .deactivate_bg(PaneTabTransitionMode::Normal)
            .expect("Failed to deactivate bg");
    }
    fn on_click(&self, window: HWND, ctx: &mut dyn InputContext) {
        let app_window = AppWindow::wrap(window);

        let Some(this) = self.upgrade() else {
            return;
        };
        let Some(state) = app_window.get_state_store() else {
            return;
        };

        // Note: selfを借りっぱなしにしないためにいったん切り出す
        let Some(g) = this.borrow().group_view.upgrade() else {
            return;
        };
        let index = this.borrow().index_in_group;

        g.borrow_mut()
            .switch_active(index, ctx, &state.app_state)
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

        let rect = group_view.borrow_mut().ht_ref.rect().clone();

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
                        .split_tab(
                            &this,
                            ctx,
                            &window.get_state_store().expect("no state store?").app_state,
                        )
                        .expect("Failed to split group view")
                        .expect("corrupted relationship");
                    new_group_view
                        .borrow_mut()
                        .rearrange(&ctx.make_resize_context());

                    if group_view.borrow().tabs.is_empty() {
                        // destroy group view
                        let relayout_root = PaneDockLayer::undock(&bound_dock_layer);
                        group_view
                            .borrow()
                            .unmount()
                            .expect("Failed to unmount group view");
                        relayout_root
                            .borrow_mut()
                            .relayout(&ctx.make_resize_context())
                            .expect("Failed to relayout docks");
                    } else {
                        group_view
                            .borrow_mut()
                            .rearrange(&ctx.make_resize_context());
                    }

                    let dest_parent = d
                        .borrow()
                        .parent()
                        .expect("Docking on root?")
                        .upgrade()
                        .expect("Parent has gone?");
                    let new_layer = PaneDockLayer::new_on(
                        DockDirection::Left,
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
                        &mut *ctx,
                    )
                    .expect("Failed to create new dock layer");
                    docking_manager
                        .borrow()
                        .mount_splitter_only(&new_layer.borrow())
                        .expect("Failed to mount new splitter");
                    dest_parent.borrow_mut().replace_child(&d, &new_layer);
                    dest_parent
                        .borrow_mut()
                        .relayout(&ctx.make_resize_context())
                        .expect("Failed to relayout from new parent");
                }
            }
            PaneDockingRecommendation::Right(d) => {
                let bound_dock_layer = group_view.borrow().bound_dock_layer.upgrade().unwrap();
                if !Rc::ptr_eq(&bound_dock_layer, &d) || group_view.borrow().tabs.len() != 1 {
                    let new_group_view = group_view
                        .borrow_mut()
                        .split_tab(
                            &this,
                            ctx,
                            &window.get_state_store().expect("no state store?").app_state,
                        )
                        .expect("Failed to split group view")
                        .expect("corrupted relationship");
                    new_group_view
                        .borrow_mut()
                        .rearrange(&ctx.make_resize_context());

                    if group_view.borrow().tabs.is_empty() {
                        // destroy group view
                        let relayout_root = PaneDockLayer::undock(&bound_dock_layer);
                        group_view
                            .borrow()
                            .unmount()
                            .expect("Failed to unmount group view");
                        relayout_root
                            .borrow_mut()
                            .relayout(&ctx.make_resize_context())
                            .expect("Failed to relayout docks");
                    } else {
                        group_view
                            .borrow_mut()
                            .rearrange(&ctx.make_resize_context());
                    }

                    let dest_parent = d
                        .borrow()
                        .parent()
                        .expect("Docking on root?")
                        .upgrade()
                        .expect("Parent has gone?");
                    let new_layer = PaneDockLayer::new_on(
                        DockDirection::Right,
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
                        &mut *ctx,
                    )
                    .expect("Failed to create new dock layer");
                    docking_manager
                        .borrow()
                        .mount_splitter_only(&new_layer.borrow())
                        .expect("Failed to mount new splitter");
                    dest_parent.borrow_mut().replace_child(&d, &new_layer);
                    dest_parent
                        .borrow_mut()
                        .relayout(&ctx.make_resize_context())
                        .expect("Failed to relayout from new parent");
                }
            }
            PaneDockingRecommendation::Top(d) => {
                let bound_dock_layer = group_view.borrow().bound_dock_layer.upgrade().unwrap();
                if !Rc::ptr_eq(&bound_dock_layer, &d) || group_view.borrow().tabs.len() != 1 {
                    let new_group_view = group_view
                        .borrow_mut()
                        .split_tab(
                            &this,
                            ctx,
                            &window.get_state_store().expect("no state store?").app_state,
                        )
                        .expect("Failed to split group view")
                        .expect("corrupted relationship");
                    new_group_view
                        .borrow_mut()
                        .rearrange(&ctx.make_resize_context());

                    if group_view.borrow().tabs.is_empty() {
                        // destroy group view
                        let relayout_root = PaneDockLayer::undock(&bound_dock_layer);
                        group_view
                            .borrow()
                            .unmount()
                            .expect("Failed to unmount group view");
                        relayout_root
                            .borrow_mut()
                            .relayout(&ctx.make_resize_context())
                            .expect("Failed to relayout docks");
                    } else {
                        group_view
                            .borrow_mut()
                            .rearrange(&ctx.make_resize_context());
                    }

                    let dest_parent = d
                        .borrow()
                        .parent()
                        .expect("Docking on root?")
                        .upgrade()
                        .expect("Parent has gone?");
                    let new_layer = PaneDockLayer::new_on(
                        DockDirection::Top,
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
                        &mut *ctx,
                    )
                    .expect("Failed to create new dock layer");
                    docking_manager
                        .borrow()
                        .mount_splitter_only(&new_layer.borrow())
                        .expect("Failed to mount new splitter");
                    dest_parent.borrow_mut().replace_child(&d, &new_layer);
                    dest_parent
                        .borrow_mut()
                        .relayout(&ctx.make_resize_context())
                        .expect("Failed to relayout from new parent");
                }
            }
            PaneDockingRecommendation::Bottom(d) => {
                let bound_dock_layer = group_view.borrow().bound_dock_layer.upgrade().unwrap();
                if !Rc::ptr_eq(&bound_dock_layer, &d) || group_view.borrow().tabs.len() != 1 {
                    let new_group_view = group_view
                        .borrow_mut()
                        .split_tab(
                            &this,
                            ctx,
                            &window.get_state_store().expect("no state store?").app_state,
                        )
                        .expect("Failed to split group view")
                        .expect("corrupted relationship");
                    new_group_view
                        .borrow_mut()
                        .rearrange(&ctx.make_resize_context());

                    if group_view.borrow().tabs.is_empty() {
                        // destroy group view
                        let relayout_root = PaneDockLayer::undock(&bound_dock_layer);
                        group_view
                            .borrow()
                            .unmount()
                            .expect("Failed to unmount group view");
                        relayout_root
                            .borrow_mut()
                            .relayout(&ctx.make_resize_context())
                            .expect("Failed to relayout docks");
                    } else {
                        group_view
                            .borrow_mut()
                            .rearrange(&ctx.make_resize_context());
                    }

                    let dest_parent = d
                        .borrow()
                        .parent()
                        .expect("Docking on root?")
                        .upgrade()
                        .expect("Parent has gone?");
                    let new_layer = PaneDockLayer::new_on(
                        DockDirection::Bottom,
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
                        &mut *ctx,
                    )
                    .expect("Failed to create new dock layer");
                    docking_manager
                        .borrow()
                        .mount_splitter_only(&new_layer.borrow())
                        .expect("Failed to mount new splitter");
                    dest_parent.borrow_mut().replace_child(&d, &new_layer);
                    dest_parent
                        .borrow_mut()
                        .relayout(&ctx.make_resize_context())
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
                    .layout(relayout_rect, &ctx.make_resize_context())
                    .expect("Failed to relayout docks");

                println!("TODO: floating");
            }
            PaneDockingRecommendation::MergeGroup(target_group) => {
                let bound_dock_layer = group_view.borrow().bound_dock_layer.upgrade().unwrap();
                group_view
                    .borrow_mut()
                    .move_tab_into(
                        &this,
                        &target_group,
                        ctx,
                        &window.get_state_store().expect("no state store?").app_state,
                    )
                    .expect("Failed to move tab");
                target_group
                    .borrow_mut()
                    .rearrange(&ctx.make_resize_context());

                if group_view.borrow().tabs.is_empty() {
                    // destroy group view
                    let relayout_root = PaneDockLayer::undock(&bound_dock_layer);
                    group_view
                        .borrow()
                        .unmount()
                        .expect("Failed to unmount group view");
                    relayout_root
                        .borrow_mut()
                        .relayout(&ctx.make_resize_context())
                        .expect("Failed to relayout docks");
                } else {
                    group_view
                        .borrow_mut()
                        .rearrange(&ctx.make_resize_context());
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
        onto_ht: &HitTestTree,
        view_context: &dyn ViewContext,
        app_state: &MTSharedMut<AppState>,
    ) -> windows::core::Result<()>;
    fn on_hide_content_view(
        &mut self,
        view_context: &dyn ViewContext,
        app_state: &MTSharedMut<AppState>,
    ) -> windows::core::Result<()>;

    #[allow(unused_variables)]
    fn on_resize(
        &mut self,
        new_size: Vector2,
        resize_ctx: &ResizeContext,
    ) -> windows::core::Result<()> {
        Ok(())
    }

    #[allow(unused_variables)]
    fn on_context_menu(
        &mut self,
        desktop_x_px: f32,
        desktop_y_px: f32,
        input_context: &dyn InputContext,
    ) {
    }
}
pub trait PaneTabPresenter: PaneTabContentPresenter + Sized {
    const INIT_TAB_NAME: &'static str;

    fn new(
        tab_header_view: &SharedMut<PaneTabHeaderView>,
        view_ctx: &(impl ViewContext + ?Sized),
        app_state: &MTSharedMut<AppState>,
    ) -> Self;
}

pub struct InspectorTabSelectionChangedEventHandler {
    content_root: ContainerVisual,
    root_ht: HitTestTree,
    current_mounted_views: RwLock<Vec<SharedMut<dyn MountableView2>>>,
    observation_disconnectors: RwLock<Vec<Box<dyn ObservationDisconnector>>>,
}
// TODO: これあとでなんとかする
unsafe impl Sync for InspectorTabSelectionChangedEventHandler {}
unsafe impl Send for InspectorTabSelectionChangedEventHandler {}
impl AppStateCurrentSelectionChangedHandler for InspectorTabSelectionChangedEventHandler {
    fn on_changed(&self, app_state: &MTSharedMut<AppState>, view_context: &dyn ViewContext) {
        // TODO: 本当は以前のViewを使いまわすとかしたほうがいいけどいったん見た目優先なのであとでやる
        for od in self.observation_disconnectors.write().drain(..) {
            od.disconnect();
        }
        for c in self.current_mounted_views.write().drain(..) {
            c.borrow().unmount().expect("Failed to unmount last views");
        }

        match app_state.read().current_selection_object_id.clone() {
            None => {
                let label = LabelView::new("None Selected", &view_context).unwrap();
                self.current_mounted_views
                    .write()
                    .push(new_shared_mut(label));
            }
            Some(id) => {
                if let Some(entity_ref) = app_state.read().current_scene.objects.get(&id) {
                    let id_label = LabelView::new(format!("Object: {id:?}"), view_context).unwrap();
                    self.current_mounted_views
                        .write()
                        .push(new_shared_mut(id_label));

                    let object_name_label =
                        LabelView::new(format!("Name: {:?}", entity_ref.name), view_context)
                            .unwrap();
                    object_name_label
                        .set_position(Vector3 {
                            X: 12.0,
                            Y: 20.0,
                            Z: 0.0,
                        })
                        .unwrap();
                    self.current_mounted_views
                        .write()
                        .push(new_shared_mut(object_name_label));

                    match entity_ref.details {
                        ObjectDetails::SunLight {
                            rotation,
                            intensity,
                            ..
                        } => {
                            let re = rotation.euler_angles();
                            let rotation_label = LabelView::new("Rotation", view_context).unwrap();
                            rotation_label
                                .set_position(Vector3 {
                                    X: 0.0,
                                    Y: 60.0,
                                    Z: 0.0,
                                })
                                .unwrap();
                            self.current_mounted_views
                                .write()
                                .push(new_shared_mut(rotation_label));

                            // TODO: これのUIレイアウト自動調整をしてくれるやつがほしい（伸縮はComposition APIがやってくれるんだけど、それの係数決めを自動化したい）
                            let rotation_x_control =
                                RollableNumberView::new(view_context, re.0.to_degrees()).unwrap();
                            rotation_x_control
                                .borrow()
                                .set_position(0.5, 0.0, 60.0)
                                .unwrap();
                            rotation_x_control
                                .borrow()
                                .set_relative_width(0.5 / 3.0, -2.0)
                                .unwrap();
                            self.current_mounted_views
                                .write()
                                .push(rotation_x_control.clone());

                            let rotation_y_control =
                                RollableNumberView::new(view_context, re.1.to_degrees()).unwrap();
                            rotation_y_control
                                .borrow()
                                .set_position(0.5 + 0.5 / 3.0, 1.0, 60.0)
                                .unwrap();
                            rotation_y_control
                                .borrow()
                                .set_relative_width(0.5 / 3.0, -2.0)
                                .unwrap();
                            self.current_mounted_views
                                .write()
                                .push(rotation_y_control.clone());

                            let rotation_z_control =
                                RollableNumberView::new(view_context, re.2.to_degrees()).unwrap();
                            rotation_z_control
                                .borrow()
                                .set_position(1.0 - 0.5 / 3.0, 2.0, 60.0)
                                .unwrap();
                            rotation_z_control
                                .borrow()
                                .set_relative_width(0.5 / 3.0, -2.0)
                                .unwrap();
                            self.current_mounted_views
                                .write()
                                .push(rotation_z_control.clone());

                            self.observation_disconnectors.write().push(Box::new(
                                rotation_x_control
                                    .borrow()
                                    .value_change_event_bus()
                                    .subscribe({
                                        let app_state = app_state.clone();
                                        let rotation_y_control = Rc::downgrade(&rotation_y_control);
                                        let rotation_z_control = Rc::downgrade(&rotation_z_control);
                                        let bound_object_id = id.clone();

                                        move |new_value| {
                                            let (Some(ry), Some(rz)) = (
                                                rotation_y_control.upgrade(),
                                                rotation_z_control.upgrade(),
                                            ) else {
                                                return;
                                            };

                                            app_state
                                                .write()
                                                .current_scene
                                                .objects
                                                .get_mut(&bound_object_id)
                                                .unwrap()
                                                .update_sunlight_rotation(
                                                    peridot_math::Quaternion::from_euler_angles(
                                                        peridot_math::Vector3(
                                                            new_value.to_radians(),
                                                            ry.borrow()
                                                                .current_value()
                                                                .to_radians(),
                                                            rz.borrow()
                                                                .current_value()
                                                                .to_radians(),
                                                        ),
                                                    ),
                                                );
                                        }
                                    }),
                            ));
                            self.observation_disconnectors.write().push(Box::new(
                                rotation_y_control
                                    .borrow()
                                    .value_change_event_bus()
                                    .subscribe({
                                        let app_state = app_state.clone();
                                        let rotation_x_control = Rc::downgrade(&rotation_x_control);
                                        let rotation_z_control = Rc::downgrade(&rotation_z_control);
                                        let bound_object_id = id.clone();

                                        move |new_value| {
                                            let (Some(rx), Some(rz)) = (
                                                rotation_x_control.upgrade(),
                                                rotation_z_control.upgrade(),
                                            ) else {
                                                return;
                                            };

                                            app_state
                                                .write()
                                                .current_scene
                                                .objects
                                                .get_mut(&bound_object_id)
                                                .unwrap()
                                                .update_sunlight_rotation(
                                                    peridot_math::Quaternion::from_euler_angles(
                                                        peridot_math::Vector3(
                                                            rx.borrow()
                                                                .current_value()
                                                                .to_radians(),
                                                            new_value.to_radians(),
                                                            rz.borrow()
                                                                .current_value()
                                                                .to_radians(),
                                                        ),
                                                    ),
                                                );
                                        }
                                    }),
                            ));
                            self.observation_disconnectors.write().push(Box::new(
                                rotation_z_control
                                    .borrow()
                                    .value_change_event_bus()
                                    .subscribe({
                                        let app_state = app_state.clone();
                                        let rotation_x_control = Rc::downgrade(&rotation_x_control);
                                        let rotation_y_control = Rc::downgrade(&rotation_y_control);
                                        let bound_object_id = id.clone();

                                        move |new_value| {
                                            let (Some(rx), Some(ry)) = (
                                                rotation_x_control.upgrade(),
                                                rotation_y_control.upgrade(),
                                            ) else {
                                                return;
                                            };

                                            app_state
                                                .write()
                                                .current_scene
                                                .objects
                                                .get_mut(&bound_object_id)
                                                .unwrap()
                                                .update_sunlight_rotation(
                                                    peridot_math::Quaternion::from_euler_angles(
                                                        peridot_math::Vector3(
                                                            rx.borrow()
                                                                .current_value()
                                                                .to_radians(),
                                                            ry.borrow()
                                                                .current_value()
                                                                .to_radians(),
                                                            new_value.to_radians(),
                                                        ),
                                                    ),
                                                );
                                        }
                                    }),
                            ));

                            let intensity_label =
                                LabelView::new("Intensity", view_context).unwrap();
                            intensity_label
                                .set_position(Vector3 {
                                    X: 0.0,
                                    Y: 80.0,
                                    Z: 0.0,
                                })
                                .unwrap();
                            self.current_mounted_views
                                .write()
                                .push(new_shared_mut(intensity_label));

                            let intensity_control =
                                FloatSliderView::new(view_context, intensity, 100.0).unwrap();
                            intensity_control
                                .borrow()
                                .reposition_xrel(0.5, 80.0)
                                .unwrap();
                            self.observation_disconnectors.write().push(Box::new(
                                intensity_control
                                    .borrow()
                                    .value_change_event_bus()
                                    .subscribe({
                                        let app_state = app_state.clone();
                                        let bound_object_id = id.clone();

                                        move |new_value| {
                                            if let Some(e) = app_state
                                                .write()
                                                .current_scene
                                                .objects
                                                .get_mut(&bound_object_id)
                                            {
                                                e.update_sunlight_intensity(new_value);
                                            }
                                        }
                                    }),
                            ));
                            self.current_mounted_views.write().push(intensity_control);
                        }
                        ObjectDetails::Mesh {
                            position,
                            rotation,
                            scale,
                            ..
                        } => {
                            let label = LabelView::new("Position", view_context).unwrap();
                            label
                                .set_position(Vector3 {
                                    X: 0.0,
                                    Y: 60.0,
                                    Z: 0.0,
                                })
                                .unwrap();
                            self.current_mounted_views
                                .write()
                                .push(new_shared_mut(label));

                            let position_x_control =
                                RollableNumberView::new(view_context, position.0).unwrap();
                            position_x_control
                                .borrow()
                                .set_position(0.5, 0.0, 60.0)
                                .unwrap();
                            position_x_control
                                .borrow()
                                .set_relative_width(0.5 / 3.0, -2.0)
                                .unwrap();
                            self.current_mounted_views
                                .write()
                                .push(position_x_control.clone());

                            let position_y_control =
                                RollableNumberView::new(view_context, position.1).unwrap();
                            position_y_control
                                .borrow()
                                .set_position(0.5 + 0.5 / 3.0, 1.0, 60.0)
                                .unwrap();
                            position_y_control
                                .borrow()
                                .set_relative_width(0.5 / 3.0, -2.0)
                                .unwrap();
                            self.current_mounted_views
                                .write()
                                .push(position_y_control.clone());

                            let position_z_control =
                                RollableNumberView::new(view_context, position.2).unwrap();
                            position_z_control
                                .borrow()
                                .set_position(1.0 - 0.5 / 3.0, 2.0, 60.0)
                                .unwrap();
                            position_z_control
                                .borrow()
                                .set_relative_width(0.5 / 3.0, -2.0)
                                .unwrap();
                            self.current_mounted_views
                                .write()
                                .push(position_z_control.clone());

                            self.observation_disconnectors.write().push(Box::new(
                                position_x_control
                                    .borrow()
                                    .value_change_event_bus()
                                    .subscribe({
                                        let app_state = app_state.clone();
                                        let position_y_control = Rc::downgrade(&position_y_control);
                                        let position_z_control = Rc::downgrade(&position_z_control);
                                        let bound_object_id = id.clone();

                                        move |new_value| {
                                            let (Some(yc), Some(zc)) = (
                                                position_y_control.upgrade(),
                                                position_z_control.upgrade(),
                                            ) else {
                                                return;
                                            };

                                            let mut state_mut = app_state.write();
                                            let target_mut = state_mut
                                                .current_scene
                                                .objects
                                                .get_mut(&bound_object_id)
                                                .unwrap();
                                            if let ObjectDetails::Mesh {
                                                ref mut position, ..
                                            } = target_mut.details
                                            {
                                                *position = peridot_math::Vector3(
                                                    new_value,
                                                    yc.borrow().current_value(),
                                                    zc.borrow().current_value(),
                                                );
                                                target_mut.is_dirty = true;
                                            }
                                        }
                                    }),
                            ));
                            self.observation_disconnectors.write().push(Box::new(
                                position_y_control
                                    .borrow()
                                    .value_change_event_bus()
                                    .subscribe({
                                        let app_state = app_state.clone();
                                        let position_x_control = Rc::downgrade(&position_x_control);
                                        let position_z_control = Rc::downgrade(&position_z_control);
                                        let bound_object_id = id.clone();

                                        move |new_value| {
                                            let (Some(xc), Some(zc)) = (
                                                position_x_control.upgrade(),
                                                position_z_control.upgrade(),
                                            ) else {
                                                return;
                                            };

                                            let mut state_mut = app_state.write();
                                            let target_mut = state_mut
                                                .current_scene
                                                .objects
                                                .get_mut(&bound_object_id)
                                                .unwrap();
                                            if let ObjectDetails::Mesh {
                                                ref mut position, ..
                                            } = target_mut.details
                                            {
                                                *position = peridot_math::Vector3(
                                                    xc.borrow().current_value(),
                                                    new_value,
                                                    zc.borrow().current_value(),
                                                );
                                                target_mut.is_dirty = true;
                                            }
                                        }
                                    }),
                            ));
                            self.observation_disconnectors.write().push(Box::new(
                                position_z_control
                                    .borrow()
                                    .value_change_event_bus()
                                    .subscribe({
                                        let app_state = app_state.clone();
                                        let position_x_control = Rc::downgrade(&position_x_control);
                                        let position_y_control = Rc::downgrade(&position_y_control);
                                        let bound_object_id = id.clone();

                                        move |new_value| {
                                            let (Some(xc), Some(yc)) = (
                                                position_x_control.upgrade(),
                                                position_y_control.upgrade(),
                                            ) else {
                                                return;
                                            };

                                            let mut state_mut = app_state.write();
                                            let target_mut = state_mut
                                                .current_scene
                                                .objects
                                                .get_mut(&bound_object_id)
                                                .unwrap();
                                            if let ObjectDetails::Mesh {
                                                ref mut position, ..
                                            } = target_mut.details
                                            {
                                                *position = peridot_math::Vector3(
                                                    xc.borrow().current_value(),
                                                    yc.borrow().current_value(),
                                                    new_value,
                                                );
                                                target_mut.is_dirty = true;
                                            }
                                        }
                                    }),
                            ));

                            let re = rotation.euler_angles();
                            let rotation_label = LabelView::new("Rotation", view_context).unwrap();
                            rotation_label
                                .set_position(Vector3 {
                                    X: 0.0,
                                    Y: 80.0,
                                    Z: 0.0,
                                })
                                .unwrap();
                            self.current_mounted_views
                                .write()
                                .push(new_shared_mut(rotation_label));

                            let rotation_x_control =
                                RollableNumberView::new(view_context, re.0.to_degrees()).unwrap();
                            rotation_x_control
                                .borrow()
                                .set_position(0.5, 0.0, 80.0)
                                .unwrap();
                            rotation_x_control
                                .borrow()
                                .set_relative_width(0.5 / 3.0, -2.0)
                                .unwrap();
                            self.current_mounted_views
                                .write()
                                .push(rotation_x_control.clone());

                            let rotation_y_control =
                                RollableNumberView::new(view_context, re.1.to_degrees()).unwrap();
                            rotation_y_control
                                .borrow()
                                .set_position(0.5 + 0.5 / 3.0, 1.0, 80.0)
                                .unwrap();
                            rotation_y_control
                                .borrow()
                                .set_relative_width(0.5 / 3.0, -2.0)
                                .unwrap();
                            self.current_mounted_views
                                .write()
                                .push(rotation_y_control.clone());

                            let rotation_z_control =
                                RollableNumberView::new(view_context, re.2.to_degrees()).unwrap();
                            rotation_z_control
                                .borrow()
                                .set_position(1.0 - 0.5 / 3.0, 2.0, 80.0)
                                .unwrap();
                            rotation_z_control
                                .borrow()
                                .set_relative_width(0.5 / 3.0, -2.0)
                                .unwrap();
                            self.current_mounted_views
                                .write()
                                .push(rotation_z_control.clone());

                            self.observation_disconnectors.write().push(Box::new(
                                rotation_x_control
                                    .borrow()
                                    .value_change_event_bus()
                                    .subscribe({
                                        let app_state = app_state.clone();
                                        let rotation_y_control = Rc::downgrade(&rotation_y_control);
                                        let rotation_z_control = Rc::downgrade(&rotation_z_control);
                                        let bound_object_id = id.clone();

                                        move |new_value| {
                                            let (Some(yc), Some(zc)) = (
                                                rotation_y_control.upgrade(),
                                                rotation_z_control.upgrade(),
                                            ) else {
                                                return;
                                            };

                                            let mut state_mut = app_state.write();
                                            let target_mut = state_mut
                                                .current_scene
                                                .objects
                                                .get_mut(&bound_object_id)
                                                .unwrap();
                                            if let ObjectDetails::Mesh {
                                                ref mut rotation, ..
                                            } = target_mut.details
                                            {
                                                *rotation =
                                                    peridot_math::Quaternion::from_euler_angles(
                                                        peridot_math::Vector3(
                                                            new_value.to_radians(),
                                                            yc.borrow()
                                                                .current_value()
                                                                .to_radians(),
                                                            zc.borrow()
                                                                .current_value()
                                                                .to_radians(),
                                                        ),
                                                    );
                                                target_mut.is_dirty = true;
                                            }
                                        }
                                    }),
                            ));
                            self.observation_disconnectors.write().push(Box::new(
                                rotation_y_control
                                    .borrow()
                                    .value_change_event_bus()
                                    .subscribe({
                                        let app_state = app_state.clone();
                                        let rotation_x_control = Rc::downgrade(&rotation_x_control);
                                        let rotation_z_control = Rc::downgrade(&rotation_z_control);
                                        let bound_object_id = id.clone();

                                        move |new_value| {
                                            let (Some(xc), Some(zc)) = (
                                                rotation_x_control.upgrade(),
                                                rotation_z_control.upgrade(),
                                            ) else {
                                                return;
                                            };

                                            let mut state_mut = app_state.write();
                                            let target_mut = state_mut
                                                .current_scene
                                                .objects
                                                .get_mut(&bound_object_id)
                                                .unwrap();
                                            if let ObjectDetails::Mesh {
                                                ref mut rotation, ..
                                            } = target_mut.details
                                            {
                                                *rotation =
                                                    peridot_math::Quaternion::from_euler_angles(
                                                        peridot_math::Vector3(
                                                            xc.borrow()
                                                                .current_value()
                                                                .to_radians(),
                                                            new_value.to_radians(),
                                                            zc.borrow()
                                                                .current_value()
                                                                .to_radians(),
                                                        ),
                                                    );
                                                target_mut.is_dirty = true;
                                            }
                                        }
                                    }),
                            ));
                            self.observation_disconnectors.write().push(Box::new(
                                rotation_z_control
                                    .borrow()
                                    .value_change_event_bus()
                                    .subscribe({
                                        let app_state = app_state.clone();
                                        let rotation_x_control = Rc::downgrade(&rotation_x_control);
                                        let rotation_y_control = Rc::downgrade(&rotation_y_control);
                                        let bound_object_id = id.clone();

                                        move |new_value| {
                                            let (Some(xc), Some(yc)) = (
                                                rotation_x_control.upgrade(),
                                                rotation_y_control.upgrade(),
                                            ) else {
                                                return;
                                            };

                                            let mut state_mut = app_state.write();
                                            let target_mut = state_mut
                                                .current_scene
                                                .objects
                                                .get_mut(&bound_object_id)
                                                .unwrap();
                                            if let ObjectDetails::Mesh {
                                                ref mut rotation, ..
                                            } = target_mut.details
                                            {
                                                *rotation =
                                                    peridot_math::Quaternion::from_euler_angles(
                                                        peridot_math::Vector3(
                                                            xc.borrow()
                                                                .current_value()
                                                                .to_radians(),
                                                            yc.borrow()
                                                                .current_value()
                                                                .to_radians(),
                                                            new_value.to_radians(),
                                                        ),
                                                    );
                                                target_mut.is_dirty = true;
                                            }
                                        }
                                    }),
                            ));

                            let label = LabelView::new("Scale", view_context).unwrap();
                            label
                                .set_position(Vector3 {
                                    X: 0.0,
                                    Y: 100.0,
                                    Z: 0.0,
                                })
                                .unwrap();
                            self.current_mounted_views
                                .write()
                                .push(new_shared_mut(label));

                            let x_control = RollableNumberView::new(view_context, scale.0).unwrap();
                            x_control.borrow().set_position(0.5, 0.0, 100.0).unwrap();
                            x_control
                                .borrow()
                                .set_relative_width(0.5 / 3.0, -2.0)
                                .unwrap();
                            self.current_mounted_views.write().push(x_control.clone());

                            let y_control = RollableNumberView::new(view_context, scale.1).unwrap();
                            y_control
                                .borrow()
                                .set_position(0.5 + 0.5 / 3.0, 1.0, 100.0)
                                .unwrap();
                            y_control
                                .borrow()
                                .set_relative_width(0.5 / 3.0, -2.0)
                                .unwrap();
                            self.current_mounted_views.write().push(y_control.clone());

                            let z_control = RollableNumberView::new(view_context, scale.2).unwrap();
                            z_control
                                .borrow()
                                .set_position(1.0 - 0.5 / 3.0, 2.0, 100.0)
                                .unwrap();
                            z_control
                                .borrow()
                                .set_relative_width(0.5 / 3.0, -2.0)
                                .unwrap();
                            self.current_mounted_views.write().push(z_control.clone());

                            self.observation_disconnectors.write().push(Box::new(
                                x_control.borrow().value_change_event_bus().subscribe({
                                    let app_state = app_state.clone();
                                    let y_control = Rc::downgrade(&y_control);
                                    let z_control = Rc::downgrade(&z_control);
                                    let bound_object_id = id.clone();

                                    move |new_value| {
                                        let (Some(yc), Some(zc)) =
                                            (y_control.upgrade(), z_control.upgrade())
                                        else {
                                            return;
                                        };

                                        let mut state_mut = app_state.write();
                                        let target_mut = state_mut
                                            .current_scene
                                            .objects
                                            .get_mut(&bound_object_id)
                                            .unwrap();
                                        if let ObjectDetails::Mesh { ref mut scale, .. } =
                                            target_mut.details
                                        {
                                            *scale = peridot_math::Vector3(
                                                new_value,
                                                yc.borrow().current_value(),
                                                zc.borrow().current_value(),
                                            );
                                            target_mut.is_dirty = true;
                                        }
                                    }
                                }),
                            ));
                            self.observation_disconnectors.write().push(Box::new(
                                y_control.borrow().value_change_event_bus().subscribe({
                                    let app_state = app_state.clone();
                                    let x_control = Rc::downgrade(&x_control);
                                    let z_control = Rc::downgrade(&z_control);
                                    let bound_object_id = id.clone();

                                    move |new_value| {
                                        let (Some(xc), Some(zc)) =
                                            (x_control.upgrade(), z_control.upgrade())
                                        else {
                                            return;
                                        };

                                        let mut state_mut = app_state.write();
                                        let target_mut = state_mut
                                            .current_scene
                                            .objects
                                            .get_mut(&bound_object_id)
                                            .unwrap();
                                        if let ObjectDetails::Mesh { ref mut scale, .. } =
                                            target_mut.details
                                        {
                                            *scale = peridot_math::Vector3(
                                                xc.borrow().current_value(),
                                                new_value,
                                                zc.borrow().current_value(),
                                            );
                                            target_mut.is_dirty = true;
                                        }
                                    }
                                }),
                            ));
                            self.observation_disconnectors.write().push(Box::new(
                                z_control.borrow().value_change_event_bus().subscribe({
                                    let app_state = app_state.clone();
                                    let x_control = Rc::downgrade(&x_control);
                                    let y_control = Rc::downgrade(&y_control);
                                    let bound_object_id = id.clone();

                                    move |new_value| {
                                        let (Some(xc), Some(yc)) =
                                            (x_control.upgrade(), y_control.upgrade())
                                        else {
                                            return;
                                        };

                                        let mut state_mut = app_state.write();
                                        let target_mut = state_mut
                                            .current_scene
                                            .objects
                                            .get_mut(&bound_object_id)
                                            .unwrap();
                                        if let ObjectDetails::Mesh { ref mut scale, .. } =
                                            target_mut.details
                                        {
                                            *scale = peridot_math::Vector3(
                                                xc.borrow().current_value(),
                                                yc.borrow().current_value(),
                                                new_value,
                                            );
                                            target_mut.is_dirty = true;
                                        }
                                    }
                                }),
                            ));
                        }
                        ObjectDetails::Camera {} => (),
                    }
                } else {
                    let id_label =
                        LabelView::new(format!("Object: {id:?} (gone)"), view_context).unwrap();
                    self.current_mounted_views
                        .write()
                        .push(new_shared_mut(id_label));
                }
            }
        }

        let children = self.content_root.Children().unwrap();
        for v in self.current_mounted_views.read().iter() {
            v.borrow().mount(&children, &self.root_ht).unwrap();
        }
    }
}
pub struct InspectorTabPresenter {
    selection_changed_event_handler: Arc<InspectorTabSelectionChangedEventHandler>,
}
impl PaneTabContentPresenter for InspectorTabPresenter {
    fn build_content_view(
        &mut self,
        onto: &ContainerVisual,
        onto_ht: &HitTestTree,
        view_context: &dyn ViewContext,
        app_state: &MTSharedMut<AppState>,
    ) -> windows::core::Result<()> {
        AppState::observe_current_selection_changes(
            &app_state,
            &self.selection_changed_event_handler,
            view_context,
        );

        onto.Children()?
            .InsertAtTop(&self.selection_changed_event_handler.content_root)?;
        onto_ht.add_child(&self.selection_changed_event_handler.root_ht);

        Ok(())
    }

    fn on_hide_content_view(
        &mut self,
        _view_context: &dyn ViewContext,
        app_state: &MTSharedMut<AppState>,
    ) -> windows::core::Result<()> {
        self.selection_changed_event_handler
            .content_root
            .Parent()?
            .Children()?
            .Remove(&self.selection_changed_event_handler.content_root)?;
        self.selection_changed_event_handler.root_ht.unmount();
        app_state
            .write()
            .unobserve_current_selection_changes(&Arc::downgrade(
                &self.selection_changed_event_handler,
            ));

        Ok(())
    }
}
impl PaneTabPresenter for InspectorTabPresenter {
    const INIT_TAB_NAME: &'static str = "Inspector";

    fn new(
        _tab_header_view: &SharedMut<PaneTabHeaderView>,
        _view_ctx: &(impl ViewContext + ?Sized),
        _app_state: &MTSharedMut<AppState>,
    ) -> Self {
        let content_root = AppSubsystemInstances::get()
            .compositor
            .CreateContainerVisual()
            .expect("Failed to create content root");
        content_root
            .set_properties()
            .expand_to_parent()
            .expect("Failed to set content root size")
            .offset(Vector3 {
                X: 8.0,
                Y: 8.0,
                Z: 0.0,
            })
            .expect("Failed to set content offset margin")
            .size(Vector2 { X: -16.0, Y: -16.0 })
            .expect("Failed to set content size margin");

        let content_root_ht = HitTestTree::new_fit_to_parent(None::<()>);
        content_root_ht.set_rect(8.0, 8.0, -16.0, -16.0);

        Self {
            selection_changed_event_handler: Arc::new(InspectorTabSelectionChangedEventHandler {
                content_root,
                root_ht: content_root_ht,
                current_mounted_views: RwLock::new(Vec::new()),
                observation_disconnectors: RwLock::new(Vec::new()),
            }),
        }
    }
}

pub struct ProjectSettingsTabPresenter {}
impl PaneTabContentPresenter for ProjectSettingsTabPresenter {
    fn build_content_view(
        &mut self,
        _onto: &ContainerVisual,
        _onto_ht: &HitTestTree,
        _view_context: &dyn ViewContext,
        _app_state: &MTSharedMut<AppState>,
    ) -> windows::core::Result<()> {
        Ok(())
    }

    fn on_hide_content_view(
        &mut self,
        _view_context: &dyn ViewContext,
        _app_state: &MTSharedMut<AppState>,
    ) -> windows::core::Result<()> {
        Ok(())
    }
}
impl PaneTabPresenter for ProjectSettingsTabPresenter {
    const INIT_TAB_NAME: &'static str = "Project Settings";

    fn new(
        _tab_header_view: &SharedMut<PaneTabHeaderView>,
        _view_ctx: &(impl ViewContext + ?Sized),
        _app_state: &MTSharedMut<AppState>,
    ) -> Self {
        Self {}
    }
}

pub struct TimelineTabPresenter {}
impl PaneTabContentPresenter for TimelineTabPresenter {
    fn build_content_view(
        &mut self,
        _onto: &ContainerVisual,
        _onto_ht: &HitTestTree,
        _view_context: &dyn ViewContext,
        _app_state: &MTSharedMut<AppState>,
    ) -> windows::core::Result<()> {
        Ok(())
    }

    fn on_hide_content_view(
        &mut self,
        _view_context: &dyn ViewContext,
        _app_state: &MTSharedMut<AppState>,
    ) -> windows::core::Result<()> {
        Ok(())
    }
}
impl PaneTabPresenter for TimelineTabPresenter {
    const INIT_TAB_NAME: &'static str = "Timeline";

    fn new(
        _tab_header_view: &SharedMut<PaneTabHeaderView>,
        _view_ctx: &(impl ViewContext + ?Sized),
        _app_state: &MTSharedMut<AppState>,
    ) -> Self {
        Self {}
    }
}

pub struct PresentationAvailableEventEntry {
    pub event: EventHandle,
    pub buffer: IPresentationBuffer,
    pub buffer_res: ID3D11Texture2D,
}

const BACK_BUFFER_COUNT: usize = 3;

pub struct PerObjectUniformData {
    pub array: ObjectUniformDataArrayBlock,
    pub index_by_object_id: HashMap<Uuid, usize>,
}
impl PerObjectUniformData {
    pub fn set_trs(&mut self, id: &Uuid, trs: peridot_math::Matrix4F32) -> br::Result<bool> {
        let (index, allocation_changed) = match self.index_by_object_id.get(id) {
            Some(x) => (*x, false),
            None => {
                let new_index = self.array.allocate().expect("failed to allocate uniform");
                self.index_by_object_id.insert(id.clone(), new_index);
                (new_index, true)
            }
        };
        let offset = self.array.offset(index);

        // 本当はアップロードバッファまとめて一気に更新したほうがいいんだけどちょっと仕組みづくり面倒なのであとで考える
        let mut update_buffer = AppSubsystemInstances::get()
            .mini_engine
            .borrow_mut()
            .alloc_upload_buffer(br::BufferDesc::new_for_type::<peridot_math::Matrix4F32>(
                br::BufferUsage::TRANSFER_SRC,
            ))?;
        unsafe {
            update_buffer.write_content_unchecked(trs)?;
        }
        AppSubsystemInstances::get()
            .mini_engine
            .borrow_mut()
            .submit_transient_commands_and_wait(|rec| {
                rec.copy_buffer(
                    &update_buffer,
                    &self.array.buffer,
                    &[br::BufferCopy::copy_data::<peridot_math::Matrix4F32>(
                        0, offset,
                    )],
                )
                .pipeline_barrier_2(&br::DependencyInfo::new(
                    &[br::MemoryBarrier2::new()
                        .from(
                            br::PipelineStageFlags2::COPY,
                            br::AccessFlags2::TRANSFER.write,
                        )
                        .to(
                            br::PipelineStageFlags2::VERTEX_SHADER,
                            br::AccessFlags2::UNIFORM_READ,
                        )],
                    &[],
                    &[],
                ))
            })?;

        Ok(allocation_changed)
    }
}

pub struct BackBuffer {
    pub presentation_buffer: IPresentationBuffer,
    pub command_buffer: RefCell<br::CommandBufferObject<StdVkDevice>>,
    pub final_destination: ID3D11Texture2D,
    pub render_target: ID3D11Texture2D,
    pub keyed_mutex: IDXGIKeyedMutex,
    pub framebuffer: br::FramebufferObject<'static, StdVkDevice>,
}

pub struct StageTabContentRenderer {
    presentation_manager: IPresentationManager,
    presentation_surface: IPresentationSurface,
    graphics_queue: Rc<RefCell<br::QueueObject<StdVkDevice>>>,
    d3d11_device_context: ID3D11DeviceContext,
    main_command_pool: RefCell<br::CommandPoolObject<StdVkDevice>>,
    buffer_size: br::vk::VkExtent2D,
    back_buffers: Vec<BackBuffer>,
    render_resources: SharedMut<EditorStageRenderResources>,
    app_state: MTWeakMut<AppState>,
}
impl SignalEventReceiver for StageTabContentRenderer {
    fn on_signal(&self, arg: usize, _view_ctx: &dyn ViewContext) {
        let bb = &self.back_buffers[arg];

        unsafe {
            bb.keyed_mutex
                .AcquireSync(0, INFINITE)
                .expect("Failed to acquire keyed mutex");
        }

        let mut command_buffer_dirty = false;
        if let Some(st) = self.app_state.upgrade() {
            command_buffer_dirty =
                core::mem::replace(&mut st.write().current_scene.is_dirty, false);

            for o in st
                .write()
                .current_scene
                .objects
                .values_mut()
                .filter(|x| x.is_dirty)
            {
                o.is_dirty = false;
                match o.details {
                    ObjectDetails::SunLight {
                        intensity,
                        rotation,
                    } => {
                        self.render_resources
                            .borrow()
                            .skybox_renderer
                            .update_primary_directional_light_data(
                                &mut AppSubsystemInstances::get().mini_engine.borrow_mut(),
                                PrimaryDirectionalLightUniformData {
                                    incident_light_dir: peridot_math::Matrix3::from(
                                        rotation.clone(),
                                    ) * peridot_math::Vector3::back(),
                                    light_intensity: intensity,
                                },
                            )
                            .expect("Failed to update primary sunlight data");

                        let rr = self.render_resources.borrow();
                        let mut forward_light_buffer_stg = AppSubsystemInstances::get()
                            .mini_engine
                            .borrow_mut()
                            .alloc_upload_buffer(br::BufferDesc::new_for_type::<
                                ForwardLightUniformData,
                            >(
                                br::BufferUsage::TRANSFER_SRC
                            ))
                            .unwrap();
                        forward_light_buffer_stg
                            .write_content(ForwardLightUniformData {
                                light_incident_dir: peridot_math::Matrix3::from(rotation.clone())
                                    * peridot_math::Vector3::back(),
                                light_intensity: intensity,
                            })
                            .unwrap();
                        AppSubsystemInstances::get()
                            .mini_engine
                            .borrow_mut()
                            .submit_transient_commands_and_wait(|rec| {
                                rec.copy_buffer(
                                    &forward_light_buffer_stg,
                                    &rr.forward_light_buffer,
                                    &[br::BufferCopy::mirror_data::<ForwardLightUniformData>(0)],
                                )
                                .pipeline_barrier_2(
                                    &br::DependencyInfo::new(
                                        &[br::MemoryBarrier2::new()
                                            .from(
                                                br::PipelineStageFlags2::COPY,
                                                br::AccessFlags2::TRANSFER.write,
                                            )
                                            .to(
                                                br::PipelineStageFlags2::FRAGMENT_SHADER,
                                                br::AccessFlags2::UNIFORM_READ,
                                            )],
                                        &[],
                                        &[],
                                    ),
                                )
                            })
                            .unwrap();
                    }
                    ObjectDetails::Mesh {
                        position,
                        rotation,
                        scale,
                        ..
                    } => {
                        let allocation_changed = self
                            .render_resources
                            .borrow_mut()
                            .per_object_uniform_data
                            .set_trs(
                                &o.id,
                                peridot_math::Matrix4F32::trs(
                                    position.clone(),
                                    rotation.clone(),
                                    scale.clone(),
                                ),
                            )
                            .expect("Failed to update object trs data");

                        command_buffer_dirty = command_buffer_dirty || allocation_changed;
                    }
                    ObjectDetails::Camera { .. } => (),
                }
            }
        }

        if command_buffer_dirty {
            self.main_command_pool.borrow_mut().reset(true).unwrap();
            for bb in self.back_buffers.iter() {
                self.render_resources.borrow().populate_commands(
                    unsafe {
                        bb.command_buffer
                            .borrow_mut()
                            .begin(AppSubsystemInstances::get().mini_engine.borrow().device())
                            .expect("Failed to begin command recording")
                    },
                    &bb.framebuffer,
                    self.buffer_size,
                    &self.app_state.upgrade().unwrap(),
                );
            }
        }

        self.graphics_queue
            .borrow_mut()
            .submit(
                &[br::EmptySubmissionBatch.with_command_buffers(&[bb.command_buffer.borrow()])],
                None::<&mut br::FenceObject<StdVkDevice>>,
            )
            .expect("Failed to send command");
        self.graphics_queue
            .borrow_mut()
            .wait()
            .expect("Failed to wait work");

        unsafe {
            bb.keyed_mutex
                .ReleaseSync(1)
                .expect("Failed to release keyed mutex");
        }

        unsafe {
            bb.keyed_mutex
                .AcquireSync(1, INFINITE)
                .expect("Failed to acquire keyed mutex");
        }
        unsafe {
            // Note: rtそのままでは表示できないらしい（Composition SwapchainでKeyedMutexいじれたらワンチャンありそうな気がする）
            self.d3d11_device_context
                .CopyResource(&bb.final_destination, &bb.render_target);
        }
        unsafe {
            bb.keyed_mutex
                .ReleaseSync(0)
                .expect("Failed to release keyed mutex");
        }

        unsafe {
            self.presentation_surface
                .SetBuffer(&bb.presentation_buffer)
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
    view: SharedMut<EditorStageView>,
}
impl PaneTabContentPresenter for StageTabPresenter {
    fn build_content_view(
        &mut self,
        onto: &ContainerVisual,
        onto_ht: &HitTestTree,
        _view_context: &dyn ViewContext,
        _app_state: &MTSharedMut<AppState>,
    ) -> windows::core::Result<()> {
        self.view.borrow().mount(&onto.Children()?, onto_ht)?;

        Ok(())
    }

    fn on_hide_content_view(
        &mut self,
        _view_context: &dyn ViewContext,
        _app_state: &MTSharedMut<AppState>,
    ) -> windows::core::Result<()> {
        self.view.borrow().unmount()?;

        Ok(())
    }

    fn on_resize(
        &mut self,
        new_size: Vector2,
        resize_ctx: &ResizeContext,
    ) -> windows::core::Result<()> {
        self.view.borrow_mut().resize(new_size, resize_ctx)
    }
}
impl PaneTabPresenter for StageTabPresenter {
    const INIT_TAB_NAME: &'static str = "Stage";

    fn new(
        _tab_header_view: &SharedMut<PaneTabHeaderView>,
        view_ctx: &(impl ViewContext + ?Sized),
        app_state: &MTSharedMut<AppState>,
    ) -> Self {
        Self {
            view: EditorStageView::new(&view_ctx, app_state)
                .expect("Failed to create EditorStageView"),
        }
    }
}

macro_rules! ArrayBuilderOp {
    ([try] $($base: tt).+, { $($vname: ident <- $arg: expr),* $(,)? }) => {
        let [$($vname),*] = $($base).+([$($arg),*])?;
    };
    ([ref, try] $($base: tt).+, { $($vname: ident <- $arg: expr),* $(,)? }) => {
        let [$($vname),*] = $($base).+(&[$($arg),*])?;
    }
}

pub struct SkyboxPrecomputedTextures {
    pub transmittance: br::ImageViewObject<peridot_memory_manager::Image>,
    pub scatter: br::ImageViewObject<peridot_memory_manager::Image>,
    pub gathered: br::ImageViewObject<peridot_memory_manager::Image>,
    pub k_scatter: br::ImageViewObject<peridot_memory_manager::Image>,
    pub k_gathered: br::ImageViewObject<peridot_memory_manager::Image>,
}
impl SkyboxPrecomputedTextures {
    const TRANSMITTANCE_SIZE: peridot::math::Vector2<u32> = peridot::math::Vector2(128, 32);
    const SCATTER_SIZE: peridot::math::Vector3<u32> = peridot::math::Vector3(32, 64 * 2, 32);
    const GATHERED_SIZE: peridot::math::Vector2<u32> = peridot::math::Vector2(32, 32);

    pub fn new(engine: &mut MiniEngine) -> br::Result<Self> {
        ArrayBuilderOp! {
            [try] engine.alloc_device_local_image_array, {
                transmittance <- br::ImageDesc::new(Self::TRANSMITTANCE_SIZE, br::vk::VK_FORMAT_R16G16B16A16_SFLOAT)
                    .sampled()
                    .use_as_storage(),
                scatter <- br::ImageDesc::new(Self::SCATTER_SIZE, br::vk::VK_FORMAT_R16G16B16A16_SFLOAT)
                    .sampled()
                    .use_as_storage(),
                gathered <- br::ImageDesc::new(Self::GATHERED_SIZE, br::vk::VK_FORMAT_R16G16B16A16_SFLOAT)
                    .sampled()
                    .use_as_storage(),
                k_scatter <- br::ImageDesc::new(Self::SCATTER_SIZE, br::vk::VK_FORMAT_R16G16B16A16_SFLOAT)
                    .sampled()
                    .use_as_storage(),
                k_gathered <- br::ImageDesc::new(Self::GATHERED_SIZE, br::vk::VK_FORMAT_R16G16B16A16_SFLOAT)
                    .sampled()
                    .use_as_storage(),
            }
        }
        let transmittance = transmittance
            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
            .view_builder()
            .create()?;
        let scatter = scatter
            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
            .view_builder()
            .create()?;
        let gathered = gathered
            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
            .view_builder()
            .create()?;
        let k_scatter = k_scatter
            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
            .view_builder()
            .create()?;
        let k_gathered = k_gathered
            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
            .view_builder()
            .create()?;

        transmittance
            .image()
            .set_name(Some(c"PeridotSkyBox:Precompute:Transmittance"))?;
        scatter
            .image()
            .set_name(Some(c"PeridotSkyBox:Precompute:Scatter"))?;
        gathered
            .image()
            .set_name(Some(c"PeridotSkyBox:Precompute:Gathered"))?;
        k_scatter
            .image()
            .set_name(Some(c"PeridotSkyBox:Precompute:K-Scatter"))?;
        k_gathered
            .image()
            .set_name(Some(c"PeridotSkyBox:Precompute:K-Gathered"))?;

        let sampler = engine.sampler(SamplerDesc {
            address_mode: (
                br::AddressingMode::ClampToEdge,
                br::AddressingMode::ClampToEdge,
                br::AddressingMode::ClampToEdge,
            ),
            min_filter: br::FilterMode::Linear,
            mag_filter: br::FilterMode::Linear,
            mip_filter: br::MipmapFilterMode::Linear,
            ..Default::default()
        })?;
        let dsl_compute_si1 =
            br::DescriptorSetLayoutBuilder::with_bindings(vec![br::DescriptorType::StorageImage
                .make_binding(1)
                .only_for_compute()])
            .create(engine.device().clone())?;
        let dsl_compute_si1_si1 = br::DescriptorSetLayoutBuilder::with_bindings(vec![
            br::DescriptorType::StorageImage
                .make_binding(1)
                .only_for_compute(),
            br::DescriptorType::StorageImage
                .make_binding(1)
                .only_for_compute(),
        ])
        .create(engine.device().clone())?;
        let dsl_compute_cis1_si1 = br::DescriptorSetLayoutBuilder::with_bindings(vec![
            br::DescriptorType::CombinedImageSampler
                .make_binding(1)
                .only_for_compute()
                .with_immutable_samplers(vec![br::SamplerObjectRef::new(&sampler)]),
            br::DescriptorType::StorageImage
                .make_binding(1)
                .only_for_compute(),
        ])
        .create(engine.device().clone())?;
        let dsl_compute_cis1_cis1_si1 = br::DescriptorSetLayoutBuilder::with_bindings(vec![
            br::DescriptorType::CombinedImageSampler
                .make_binding(1)
                .only_for_compute()
                .with_immutable_samplers(vec![br::SamplerObjectRef::new(&sampler)]),
            br::DescriptorType::CombinedImageSampler
                .make_binding(1)
                .only_for_compute()
                .with_immutable_samplers(vec![br::SamplerObjectRef::new(&sampler)]),
            br::DescriptorType::StorageImage
                .make_binding(1)
                .only_for_compute(),
        ])
        .create(engine.device().clone())?;

        let input_only_layout = br::PipelineLayoutBuilder::new(vec![&dsl_compute_si1], vec![])
            .create(engine.device().clone())?;
        let tex_io_layout = br::PipelineLayoutBuilder::new(vec![&dsl_compute_cis1_si1], vec![])
            .create(engine.device().clone())?;
        let tex_i2o_layout =
            br::PipelineLayoutBuilder::new(vec![&dsl_compute_cis1_cis1_si1], vec![])
                .create(engine.device().clone())?;
        let tex_io_pure_layout = br::PipelineLayoutBuilder::new(vec![&dsl_compute_si1_si1], vec![])
            .create(engine.device().clone())?;
        let transmittance_compute =
            engine.shader("shaders/skybox/transmittance_precompute.cspv")?;
        let single_scatter_compute =
            engine.shader("shaders/skybox/single_scatter_precompute.cspv")?;
        let gather_compute = engine.shader("shaders/skybox/gather_precompute.cspv")?;
        let multiple_scatter_compute =
            engine.shader("shaders/skybox/multiple_scatter_precompute.cspv")?;
        let accum2_compute = engine.shader("shaders/skybox/accum2.cspv")?;
        let accum3_compute = engine.shader("shaders/skybox/accum3.cspv")?;
        ArrayBuilderOp! {
            [ref, try] engine.create_compute_pipeline_array, {
                transmittance_compute_pipeline <- br::ComputePipelineBuilder::new(
                    &input_only_layout,
                    br::PipelineShader2::new(&transmittance_compute, c"main".to_owned()),
                ),
                single_scatter_compute_pipeline <- br::ComputePipelineBuilder::new(
                    &tex_io_layout,
                    br::PipelineShader2::new(&single_scatter_compute, c"main".to_owned()),
                ),
                gather_compute_pipeline <- br::ComputePipelineBuilder::new(
                    &tex_io_layout,
                    br::PipelineShader2::new(&gather_compute, c"main".to_owned()),
                ),
                multiple_scatter_compute_pipeline <- br::ComputePipelineBuilder::new(
                    &tex_i2o_layout,
                    br::PipelineShader2::new(&multiple_scatter_compute, c"main".to_owned()),
                ),
                accum2_pipeline <- br::ComputePipelineBuilder::new(
                    &tex_io_pure_layout,
                    br::PipelineShader2::new(&accum2_compute, c"main".to_owned()),
                ),
                accum3_pipeline <- br::ComputePipelineBuilder::new(
                    &tex_io_pure_layout,
                    br::PipelineShader2::new(&accum3_compute, c"main".to_owned()),
                ),
            }
        }

        let mut descriptor_pool = br::DescriptorPoolBuilder::new(8)
            .reserve_all([
                br::DescriptorType::StorageImage.with_count(10),
                br::DescriptorType::CombinedImageSampler.with_count(7),
            ])
            .create(engine.device().clone())?;
        ArrayBuilderOp! {
            [ref, try] descriptor_pool.alloc_array, {
                transmittance_set <- br::DescriptorSetLayoutObjectRef::new(&dsl_compute_si1),
                transmittance_to_scatter_set <- br::DescriptorSetLayoutObjectRef::new(&dsl_compute_cis1_si1),
                scatter_to_gathered_set <- br::DescriptorSetLayoutObjectRef::new(&dsl_compute_cis1_si1),
                transmittance_gathered_to_k_scatter_set <- br::DescriptorSetLayoutObjectRef::new(&dsl_compute_cis1_cis1_si1),
                k_scatter_to_k_gathered_set <- br::DescriptorSetLayoutObjectRef::new(&dsl_compute_cis1_si1),
                k_scatter_to_scatter_set <- br::DescriptorSetLayoutObjectRef::new(&dsl_compute_si1_si1),
                k_gathered_to_k_gathered_set <- br::DescriptorSetLayoutObjectRef::new(&dsl_compute_si1_si1),
                transmittance_k_gathered_to_k_scatter_set <- br::DescriptorSetLayoutObjectRef::new(&dsl_compute_cis1_cis1_si1),
            }
        };
        engine.device().update_descriptor_sets(
            &[
                transmittance_set
                    .binding_at(0)
                    .write(br::DescriptorContents::storage_image(
                        &transmittance,
                        br::ImageLayout::General,
                    )),
                transmittance_to_scatter_set.binding_at(0).write(
                    br::DescriptorContents::combined_image_sampler(
                        &transmittance,
                        br::ImageLayout::ShaderReadOnlyOpt,
                    ),
                ),
                transmittance_to_scatter_set.binding_at(1).write(
                    br::DescriptorContents::storage_image(&scatter, br::ImageLayout::General),
                ),
                scatter_to_gathered_set.binding_at(0).write(
                    br::DescriptorContents::combined_image_sampler(
                        &scatter,
                        br::ImageLayout::ShaderReadOnlyOpt,
                    ),
                ),
                scatter_to_gathered_set
                    .binding_at(1)
                    .write(br::DescriptorContents::storage_image(
                        &gathered,
                        br::ImageLayout::General,
                    )),
                transmittance_gathered_to_k_scatter_set.binding_at(0).write(
                    br::DescriptorContents::combined_image_sampler(
                        &transmittance,
                        br::ImageLayout::ShaderReadOnlyOpt,
                    ),
                ),
                transmittance_gathered_to_k_scatter_set.binding_at(1).write(
                    br::DescriptorContents::combined_image_sampler(
                        &gathered,
                        br::ImageLayout::ShaderReadOnlyOpt,
                    ),
                ),
                transmittance_gathered_to_k_scatter_set.binding_at(2).write(
                    br::DescriptorContents::storage_image(&k_scatter, br::ImageLayout::General),
                ),
                k_scatter_to_k_gathered_set.binding_at(0).write(
                    br::DescriptorContents::combined_image_sampler(
                        &k_scatter,
                        br::ImageLayout::ShaderReadOnlyOpt,
                    ),
                ),
                k_scatter_to_k_gathered_set.binding_at(1).write(
                    br::DescriptorContents::storage_image(&k_gathered, br::ImageLayout::General),
                ),
                k_scatter_to_scatter_set.binding_at(0).write(
                    br::DescriptorContents::storage_image(&k_scatter, br::ImageLayout::General),
                ),
                k_scatter_to_scatter_set.binding_at(1).write(
                    br::DescriptorContents::storage_image(&scatter, br::ImageLayout::General),
                ),
                k_gathered_to_k_gathered_set.binding_at(0).write(
                    br::DescriptorContents::storage_image(&k_gathered, br::ImageLayout::General),
                ),
                k_gathered_to_k_gathered_set.binding_at(1).write(
                    br::DescriptorContents::storage_image(&k_gathered, br::ImageLayout::General),
                ),
                transmittance_k_gathered_to_k_scatter_set
                    .binding_at(0)
                    .write(br::DescriptorContents::combined_image_sampler(
                        &transmittance,
                        br::ImageLayout::ShaderReadOnlyOpt,
                    )),
                transmittance_k_gathered_to_k_scatter_set
                    .binding_at(1)
                    .write(br::DescriptorContents::combined_image_sampler(
                        &k_gathered,
                        br::ImageLayout::ShaderReadOnlyOpt,
                    )),
                transmittance_k_gathered_to_k_scatter_set
                    .binding_at(2)
                    .write(br::DescriptorContents::storage_image(
                        &k_scatter,
                        br::ImageLayout::General,
                    )),
            ],
            &[],
        );

        engine.submit_transient_commands_and_wait(|rec| {
            let mut rec = rec
                .pipeline_barrier_2(&br::DependencyInfo::new(
                    &[],
                    &[],
                    &[transmittance
                        .image()
                        .by_ref()
                        .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                        .memory_barrier2()
                        .transit_to(br::ImageLayout::General.from_undefined())],
                ))
                .bind_compute_pipeline_pair(&transmittance_compute_pipeline, &input_only_layout)
                .bind_compute_descriptor_sets(0, &[transmittance_set.into()], &[])
                .dispatch(
                    Self::TRANSMITTANCE_SIZE.0 / 32,
                    Self::TRANSMITTANCE_SIZE.1 / 32,
                    1,
                )
                .pipeline_barrier_2(&br::DependencyInfo::new(
                    &[],
                    &[],
                    &[
                        transmittance
                            .image()
                            .by_ref()
                            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                            .memory_barrier2()
                            .transit_from(
                                br::ImageLayout::General.to(br::ImageLayout::ShaderReadOnlyOpt),
                            )
                            .from(
                                br::PipelineStageFlags2::COMPUTE_SHADER,
                                br::AccessFlags2::SHADER.write,
                            )
                            .to(
                                br::PipelineStageFlags2::COMPUTE_SHADER,
                                br::AccessFlags2::SHADER.read,
                            ),
                        scatter
                            .image()
                            .by_ref()
                            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                            .memory_barrier2()
                            .transit_to(br::ImageLayout::General.from_undefined()),
                    ],
                ))
                .bind_compute_pipeline_pair(&single_scatter_compute_pipeline, &tex_io_layout)
                .bind_compute_descriptor_sets(0, &[transmittance_to_scatter_set.into()], &[])
                .dispatch(
                    Self::SCATTER_SIZE.0 / 8,
                    Self::SCATTER_SIZE.1 / 8,
                    Self::SCATTER_SIZE.2 / 8,
                )
                .pipeline_barrier_2(&br::DependencyInfo::new(
                    &[],
                    &[],
                    &[
                        scatter
                            .image()
                            .by_ref()
                            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                            .memory_barrier2()
                            .transit_from(
                                br::ImageLayout::General.to(br::ImageLayout::ShaderReadOnlyOpt),
                            )
                            .from(
                                br::PipelineStageFlags2::COMPUTE_SHADER,
                                br::AccessFlags2::SHADER.write,
                            )
                            .to(
                                br::PipelineStageFlags2::COMPUTE_SHADER,
                                br::AccessFlags2::SHADER.read,
                            ),
                        gathered
                            .image()
                            .by_ref()
                            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                            .memory_barrier2()
                            .transit_to(br::ImageLayout::General.from_undefined()),
                    ],
                ))
                .bind_compute_pipeline_pair(&gather_compute_pipeline, &tex_io_layout)
                .bind_compute_descriptor_sets(0, &[scatter_to_gathered_set.into()], &[])
                .dispatch(Self::GATHERED_SIZE.0 / 32, Self::GATHERED_SIZE.1 / 32, 1)
                .pipeline_barrier_2(&br::DependencyInfo::new(
                    &[],
                    &[],
                    &[
                        gathered
                            .image()
                            .by_ref()
                            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                            .memory_barrier2()
                            .transit_from(
                                br::ImageLayout::General.to(br::ImageLayout::ShaderReadOnlyOpt),
                            )
                            .from(
                                br::PipelineStageFlags2::COMPUTE_SHADER,
                                br::AccessFlags2::SHADER.write,
                            )
                            .to(
                                br::PipelineStageFlags2::COMPUTE_SHADER,
                                br::AccessFlags2::SHADER.read,
                            ),
                        k_scatter
                            .image()
                            .by_ref()
                            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                            .memory_barrier2()
                            .transit_to(br::ImageLayout::General.from_undefined()),
                    ],
                ))
                .bind_compute_pipeline_pair(&multiple_scatter_compute_pipeline, &tex_i2o_layout)
                .bind_compute_descriptor_sets(
                    0,
                    &[transmittance_gathered_to_k_scatter_set.into()],
                    &[],
                )
                .dispatch(
                    Self::SCATTER_SIZE.0 / 8,
                    Self::SCATTER_SIZE.1 / 8,
                    Self::SCATTER_SIZE.2 / 8,
                )
                .pipeline_barrier_2(&br::DependencyInfo::new(
                    &[],
                    &[],
                    &[
                        k_scatter
                            .image()
                            .by_ref()
                            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                            .memory_barrier2()
                            .transit_from(
                                br::ImageLayout::General.to(br::ImageLayout::ShaderReadOnlyOpt),
                            )
                            .from(
                                br::PipelineStageFlags2::COMPUTE_SHADER,
                                br::AccessFlags2::SHADER.write,
                            )
                            .to(
                                br::PipelineStageFlags2::COMPUTE_SHADER,
                                br::AccessFlags2::SHADER.read,
                            ),
                        k_gathered
                            .image()
                            .by_ref()
                            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                            .memory_barrier2()
                            .transit_to(br::ImageLayout::General.from_undefined()),
                    ],
                ))
                .bind_compute_pipeline_pair(&gather_compute_pipeline, &tex_io_layout)
                .bind_compute_descriptor_sets(0, &[k_scatter_to_k_gathered_set.into()], &[])
                .dispatch(Self::GATHERED_SIZE.0 / 32, Self::GATHERED_SIZE.1 / 32, 1)
                .pipeline_barrier_2(&br::DependencyInfo::new(
                    &[br::MemoryBarrier2::new()
                        .from(
                            br::PipelineStageFlags2::COMPUTE_SHADER,
                            br::AccessFlags2::SHADER.write,
                        )
                        .to(
                            br::PipelineStageFlags2::COMPUTE_SHADER,
                            br::AccessFlags2::SHADER.read,
                        )],
                    &[],
                    &[],
                ))
                .bind_compute_pipeline_pair(&accum2_pipeline, &tex_io_pure_layout)
                .bind_compute_descriptor_sets(0, &[k_gathered_to_k_gathered_set.into()], &[])
                .dispatch(Self::GATHERED_SIZE.0 / 32, Self::GATHERED_SIZE.1 / 32, 1)
                .pipeline_barrier_2(&br::DependencyInfo::new(
                    &[],
                    &[],
                    &[
                        scatter
                            .image()
                            .by_ref()
                            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                            .memory_barrier2()
                            .transit_from(
                                br::ImageLayout::ShaderReadOnlyOpt.to(br::ImageLayout::General),
                            ),
                        k_scatter
                            .image()
                            .by_ref()
                            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                            .memory_barrier2()
                            .transit_from(
                                br::ImageLayout::ShaderReadOnlyOpt.to(br::ImageLayout::General),
                            ),
                    ],
                ))
                .bind_compute_pipeline_pair(&accum3_pipeline, &tex_io_pure_layout)
                .bind_compute_descriptor_sets(0, &[k_scatter_to_scatter_set.into()], &[])
                .dispatch(
                    Self::SCATTER_SIZE.0 / 8,
                    Self::SCATTER_SIZE.1 / 8,
                    Self::SCATTER_SIZE.2 / 8,
                );

            // multiple scatters after 2nd
            for _ in 0..2 {
                rec = rec
                    .pipeline_barrier_2(&br::DependencyInfo::new(
                        &[],
                        &[],
                        &[k_gathered
                            .image()
                            .by_ref()
                            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                            .memory_barrier2()
                            .transit_from(
                                br::ImageLayout::General.to(br::ImageLayout::ShaderReadOnlyOpt),
                            )
                            .from(
                                br::PipelineStageFlags2::COMPUTE_SHADER,
                                br::AccessFlags2::SHADER.write,
                            )
                            .to(
                                br::PipelineStageFlags2::COMPUTE_SHADER,
                                br::AccessFlags2::SHADER.read,
                            )],
                    ))
                    .bind_compute_pipeline_pair(&multiple_scatter_compute_pipeline, &tex_i2o_layout)
                    .bind_compute_descriptor_sets(
                        0,
                        &[transmittance_k_gathered_to_k_scatter_set.into()],
                        &[],
                    )
                    .dispatch(
                        Self::SCATTER_SIZE.0 / 8,
                        Self::SCATTER_SIZE.1 / 8,
                        Self::SCATTER_SIZE.2 / 8,
                    )
                    .pipeline_barrier_2(&br::DependencyInfo::new(
                        &[],
                        &[],
                        &[
                            k_scatter
                                .image()
                                .by_ref()
                                .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                                .memory_barrier2()
                                .transit_from(
                                    br::ImageLayout::General.to(br::ImageLayout::ShaderReadOnlyOpt),
                                )
                                .from(
                                    br::PipelineStageFlags2::COMPUTE_SHADER,
                                    br::AccessFlags2::SHADER.write,
                                )
                                .to(
                                    br::PipelineStageFlags2::COMPUTE_SHADER,
                                    br::AccessFlags2::SHADER.read,
                                ),
                            k_gathered
                                .image()
                                .by_ref()
                                .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                                .memory_barrier2()
                                .transit_from(
                                    br::ImageLayout::ShaderReadOnlyOpt.to(br::ImageLayout::General),
                                ),
                        ],
                    ))
                    .bind_compute_pipeline_pair(&gather_compute_pipeline, &tex_io_layout)
                    .bind_compute_descriptor_sets(0, &[k_scatter_to_k_gathered_set.into()], &[])
                    .dispatch(Self::GATHERED_SIZE.0 / 32, Self::GATHERED_SIZE.1 / 32, 1)
                    .pipeline_barrier_2(&br::DependencyInfo::new(
                        &[br::MemoryBarrier2::new()
                            .from(
                                br::PipelineStageFlags2::COMPUTE_SHADER,
                                br::AccessFlags2::SHADER.write,
                            )
                            .to(
                                br::PipelineStageFlags2::COMPUTE_SHADER,
                                br::AccessFlags2::SHADER.read,
                            )],
                        &[],
                        &[],
                    ))
                    .bind_compute_pipeline_pair(&accum2_pipeline, &tex_io_pure_layout)
                    .bind_compute_descriptor_sets(0, &[k_gathered_to_k_gathered_set.into()], &[])
                    .dispatch(Self::GATHERED_SIZE.0 / 32, Self::GATHERED_SIZE.1 / 32, 1)
                    .pipeline_barrier_2(&br::DependencyInfo::new(
                        &[],
                        &[],
                        &[k_scatter
                            .image()
                            .by_ref()
                            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                            .memory_barrier2()
                            .transit_from(
                                br::ImageLayout::ShaderReadOnlyOpt.to(br::ImageLayout::General),
                            )],
                    ))
                    .bind_compute_pipeline_pair(&accum3_pipeline, &tex_io_pure_layout)
                    .bind_compute_descriptor_sets(0, &[k_scatter_to_scatter_set.into()], &[])
                    .dispatch(
                        Self::SCATTER_SIZE.0 / 8,
                        Self::SCATTER_SIZE.1 / 8,
                        Self::SCATTER_SIZE.2 / 8,
                    );
            }

            rec.pipeline_barrier_2(&br::DependencyInfo::new(
                &[],
                &[],
                &[scatter
                    .image()
                    .by_ref()
                    .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                    .memory_barrier2()
                    .transit_from(br::ImageLayout::General.to(br::ImageLayout::ShaderReadOnlyOpt))
                    .from(
                        br::PipelineStageFlags2::COMPUTE_SHADER,
                        br::AccessFlags2::SHADER.write,
                    )
                    .to(
                        br::PipelineStageFlags2::FRAGMENT_SHADER,
                        br::AccessFlags2::SHADER.read,
                    )],
            ))
        })?;

        Ok(Self {
            transmittance,
            scatter,
            gathered,
            k_scatter,
            k_gathered,
        })
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct SkyboxVertex {
    pos: peridot_math::Vector2F32,
    uv: peridot_math::Vector2F32,
}

pub struct SkyboxRenderer {
    pub precomputed: SkyboxPrecomputedTextures,
    pub _descriptor_pool: br::DescriptorPoolObject<StdVkDevice>,
    pub renderer_descriptor: br::DescriptorSet,
    pub pipeline_layout: br::PipelineLayoutObject<StdVkDevice>,
    pub pipeline: br::PipelineObject<StdVkDevice>,
    pub primary_directional_light_data_buffer: peridot_memory_manager::Buffer,
}
impl SkyboxRenderer {
    pub fn new(
        engine: &mut MiniEngine,
        render_camera_descriptor_set_layout: &impl br::DescriptorSetLayout<ConcreteDevice = StdVkDevice>,
        render_pass: &(impl br::RenderPass + ?Sized),
        subpass: u32,
        precomputed: SkyboxPrecomputedTextures,
        init_light_data: PrimaryDirectionalLightUniformData,
    ) -> br::Result<Self> {
        let linear_sampler = engine.sampler(SamplerDesc {
            address_mode: (
                br::AddressingMode::ClampToEdge,
                br::AddressingMode::ClampToEdge,
                br::AddressingMode::ClampToEdge,
            ),
            min_filter: br::FilterMode::Linear,
            mag_filter: br::FilterMode::Linear,
            mip_filter: br::MipmapFilterMode::Linear,
            ..Default::default()
        })?;
        let dsl = br::DescriptorSetLayoutBuilder::with_bindings(vec![
            br::DescriptorType::UniformBuffer
                .make_binding(1)
                .only_for_fragment(),
            br::DescriptorType::CombinedImageSampler
                .make_binding(1)
                .only_for_fragment()
                .with_immutable_samplers(vec![br::SamplerObjectRef::new(&linear_sampler)]),
            br::DescriptorType::CombinedImageSampler
                .make_binding(1)
                .only_for_fragment()
                .with_immutable_samplers(vec![br::SamplerObjectRef::new(&linear_sampler)]),
        ])
        .create(engine.device().clone())?;

        let pipeline_layout =
            br::PipelineLayoutBuilder::new(vec![render_camera_descriptor_set_layout, &dsl], vec![])
                .create(engine.device().clone())?;
        let vsh = engine.shader("shaders/skybox/vert.vspv")?;
        let fsh = engine.shader("shaders/skybox/frag.fspv")?;
        let mut pipeline = br::NonDerivedGraphicsPipelineBuilder::new(
            &pipeline_layout,
            (&render_pass, subpass),
            br::VertexProcessingStages::new(
                br::VertexShaderStage::new(br::PipelineShader2::new(&vsh, c"main".to_owned()))
                    .with_fragment_shader_stage(br::PipelineShader2::new(&fsh, c"main".to_owned())),
                &[],
                &[],
                br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP,
            ),
        );
        pipeline
            .multisample_state(Some(br::MultisampleState::new()))
            .add_attachment_blend(br::AttachmentColorBlendState::noblend())
            .viewport_scissors(
                br::DynamicArrayState::Dynamic(1),
                br::DynamicArrayState::Dynamic(1),
            )
            .depth_test_settings(Some(br::CompareOp::LessOrEqual), false);
        let pipeline = pipeline.create(engine.device().clone(), Some(engine.pipeline_cache()))?;
        engine.writeback_pipeline_cache();

        struct BufferInitializationContents {
            pub primary_directional_light_data: PrimaryDirectionalLightUniformData,
        }
        let [primary_directional_light_data_buffer] =
            engine.alloc_device_local_buffer_array([br::BufferDesc::new(
                core::mem::size_of::<PrimaryDirectionalLightUniformData>(),
                br::BufferUsage::UNIFORM_BUFFER.transfer_dest(),
            )])?;
        let mut stg_buffer = engine.alloc_upload_buffer(br::BufferDesc::new(
            core::mem::size_of::<BufferInitializationContents>(),
            br::BufferUsage::TRANSFER_SRC,
        ))?;
        stg_buffer.write_content(BufferInitializationContents {
            primary_directional_light_data: init_light_data,
        })?;

        engine.submit_transient_commands_and_wait(|rec| {
            rec.copy_buffer(
                &stg_buffer,
                &primary_directional_light_data_buffer,
                &[br::BufferCopy::copy_data::<
                    PrimaryDirectionalLightUniformData,
                >(
                    core::mem::offset_of!(
                        BufferInitializationContents,
                        primary_directional_light_data
                    ) as _,
                    0,
                )],
            )
            .pipeline_barrier_2(&br::DependencyInfo::new(
                &[br::MemoryBarrier2::new()
                    .from(
                        br::PipelineStageFlags2::COPY,
                        br::AccessFlags2::TRANSFER.write,
                    )
                    .to(
                        br::PipelineStageFlags2::FRAGMENT_SHADER,
                        br::AccessFlags2::SHADER.read,
                    )],
                &[],
                &[],
            ))
        })?;

        let mut dp = br::DescriptorPoolBuilder::new(1)
            .with_reservations(vec![
                br::DescriptorType::UniformBuffer.with_count(1),
                br::DescriptorType::CombinedImageSampler.with_count(2),
            ])
            .create(engine.device().clone())?;
        let [descriptor] = dp.alloc_array(&[br::DescriptorSetLayoutObjectRef::new(&dsl)])?;
        engine.device().update_descriptor_sets(
            &[
                descriptor
                    .binding_at(0)
                    .write(br::DescriptorContents::uniform_buffer(
                        &primary_directional_light_data_buffer,
                        0..core::mem::size_of::<PrimaryDirectionalLightUniformData>()
                            as br::vk::VkDeviceSize,
                    )),
                descriptor
                    .binding_at(1)
                    .write(br::DescriptorContents::combined_image_sampler(
                        &precomputed.scatter,
                        br::ImageLayout::ShaderReadOnlyOpt,
                    )),
                descriptor
                    .binding_at(2)
                    .write(br::DescriptorContents::combined_image_sampler(
                        &precomputed.transmittance,
                        br::ImageLayout::ShaderReadOnlyOpt,
                    )),
            ],
            &[],
        );

        Ok(Self {
            precomputed,
            _descriptor_pool: dp,
            renderer_descriptor: descriptor,
            pipeline_layout,
            pipeline,
            primary_directional_light_data_buffer,
        })
    }

    pub fn update_primary_directional_light_data(
        &self,
        e: &mut MiniEngine,
        new_data: PrimaryDirectionalLightUniformData,
    ) -> br::Result<()> {
        let mut upload_buffer = e.alloc_upload_buffer(br::BufferDesc::new(
            core::mem::size_of::<PrimaryDirectionalLightUniformData>(),
            br::BufferUsage::TRANSFER_SRC,
        ))?;
        upload_buffer.write_content(new_data)?;

        e.submit_transient_commands_and_wait(|rec| {
            rec.copy_buffer(
                &upload_buffer,
                &self.primary_directional_light_data_buffer,
                &[br::BufferCopy::mirror_data::<
                    PrimaryDirectionalLightUniformData,
                >(0)],
            )
            .pipeline_barrier_2(&br::DependencyInfo::new(
                &[br::MemoryBarrier2::new()
                    .from(
                        br::PipelineStageFlags2::COPY,
                        br::AccessFlags2::TRANSFER.write,
                    )
                    .to(
                        br::PipelineStageFlags2::FRAGMENT_SHADER,
                        br::AccessFlags2::UNIFORM_READ,
                    )],
                &[],
                &[],
            ))
        })?;

        Ok(())
    }

    pub fn record_render_commands<
        'r,
        CB: br::VkHandleMut<Handle = br::vk::VkCommandBuffer> + ?Sized,
        Device: br::Device + ?Sized,
    >(
        &self,
        rec: br::CmdRecord<'r, CB, Device>,
    ) -> br::CmdRecord<'r, CB, Device> {
        rec.bind_graphics_pipeline_pair(&self.pipeline, &self.pipeline_layout)
            .bind_graphics_descriptor_sets(1, &[self.renderer_descriptor.0], &[])
            .draw(4, 1, 0, 0)
    }
}

#[repr(C)]
pub struct RenderCameraUniformData {
    pub camera_view_projection_matrix: peridot_math::Matrix4F32,
    pub camera_inverse_view_matrix: peridot_math::Matrix4F32,
    pub camera_persp_fov_rad: f32,
    pub camera_aspect_wh: f32,
}

#[repr(C)]
pub struct PrimaryDirectionalLightUniformData {
    pub incident_light_dir: peridot_math::Vector3F32,
    pub light_intensity: f32,
}

fn d3d11_presentation_texture_desc(width: u32, height: u32) -> D3D11_TEXTURE2D_DESC {
    D3D11_TEXTURE2D_DESC {
        Width: width,
        Height: height,
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
    }
}

trait D3D11ResourceDescriptor {
    type Output;

    fn create(&self, device: &ID3D11Device) -> windows::core::Result<Self::Output>;
}
impl D3D11ResourceDescriptor for D3D11_TEXTURE2D_DESC {
    type Output = ID3D11Texture2D;

    #[inline(always)]
    fn create(&self, device: &ID3D11Device) -> windows::core::Result<Self::Output> {
        let mut h = core::mem::MaybeUninit::uninit();
        unsafe {
            device.CreateTexture2D(self, None, Some(h.as_mut_ptr()))?;
        }
        unsafe { Ok(h.assume_init().expect("resource not created?")) }
    }
}

pub struct ObjectUniformDataArrayBlock {
    pub buffer: peridot_memory_manager::Buffer,
    next_free: usize,
    free_blocks: BTreeSet<usize>,
    cap: usize,
}
impl ObjectUniformDataArrayBlock {
    pub fn new(e: &mut MiniEngine, init_cap: usize) -> br::Result<Self> {
        let buffer = e.alloc_device_local_buffer(br::BufferDesc::new(
            core::mem::size_of::<peridot_math::Matrix4F32>() * init_cap,
            br::BufferUsage::UNIFORM_BUFFER.transfer_dest(),
        ))?;

        Ok(Self {
            buffer,
            next_free: 0,
            free_blocks: BTreeSet::new(),
            cap: init_cap,
        })
    }

    pub fn allocate(&mut self) -> Option<usize> {
        match self.free_blocks.pop_first() {
            Some(x) => Some(x),
            None => {
                let a = self.next_free;
                if a >= self.cap {
                    return None;
                }

                self.next_free += 1;
                Some(a)
            }
        }
    }

    pub fn free(&mut self, at: usize) {
        self.free_blocks.insert(at);
    }

    pub const fn offset(&self, at: usize) -> br::vk::VkDeviceSize {
        (at * core::mem::size_of::<peridot_math::Matrix4F32>()) as _
    }

    pub fn data_range(&self) -> core::ops::Range<br::vk::VkDeviceSize> {
        0..core::mem::size_of::<peridot_math::Matrix4F32>() as _
    }
}

#[repr(C)]
pub struct ForwardLightUniformData {
    pub light_incident_dir: peridot_math::Vector3F32,
    pub light_intensity: f32,
}

pub struct EditorStageRenderResources {
    utility_verts: UtilityVertices,
    skybox_renderer: SkyboxRenderer,
    _descriptor_set_layout_ia1: br::DescriptorSetLayoutObject<StdVkDevice>,
    _descriptor_set_layout_ub1: br::DescriptorSetLayoutObject<StdVkDevice>,
    _descriptor_pool: br::DescriptorPoolObject<StdVkDevice>,
    hdr_temp_rt: TempRT,
    depth_stencil_temp_rt: TempRT,
    main_render_pass: br::RenderPassObject<StdVkDevice>,
    hdr_final_pass_pipeline_layout: br::PipelineLayoutObject<StdVkDevice>,
    hdr_final_pass_pipeline: br::PipelineObject<StdVkDevice>,
    grid_pipeline_layout: br::PipelineLayoutObject<StdVkDevice>,
    grid_pipeline: br::PipelineObject<StdVkDevice>,
    grid_buffer: peridot_memory_manager::Buffer,
    grid_vertex_count: u32,
    default_material_pipeline_layout: br::PipelineLayoutObject<StdVkDevice>,
    default_material_pipeline: br::PipelineObject<StdVkDevice>,
    camera_buffer: peridot_memory_manager::Buffer,
    camera: Camera,
    forward_light_buffer: peridot_memory_manager::Buffer,
    camera_descriptor_set: br::DescriptorSet,
    hdr_final_pass_descriptor_set: br::DescriptorSet,
    per_object_descriptor_set: br::DescriptorSet,
    per_object_uniform_data: PerObjectUniformData,
}
impl EditorStageRenderResources {
    fn grid_vertices() -> Vec<ColoredVertex> {
        (-5..=5)
            .flat_map(|x| {
                [
                    ColoredVertex {
                        pos: Vec4::new(x as _, 0.0, -5.0, 1.0),
                        color: Vec4::new(0.75, 0.75, 0.75, 0.5),
                    },
                    ColoredVertex {
                        pos: Vec4::new(x as _, 0.0, 5.0, 1.0),
                        color: Vec4::new(0.75, 0.75, 0.75, 0.5),
                    },
                ]
            })
            .chain((-5..=5).flat_map(|z| {
                [
                    ColoredVertex {
                        pos: Vec4::new(-5.0, 0.0, z as _, 1.0),
                        color: Vec4::new(0.75, 0.75, 0.75, 0.5),
                    },
                    ColoredVertex {
                        pos: Vec4::new(5.0, 0.0, z as _, 1.0),
                        color: Vec4::new(0.75, 0.75, 0.75, 0.5),
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
            .collect::<Vec<_>>()
    }

    pub fn new(
        e: &mut MiniEngine,
        app_state: &MTSharedMut<AppState>,
        init_size: br::vk::VkExtent2D,
    ) -> Self {
        let mut init_cp = e
            .command_pool_builder_for_graphics_work()
            .transient()
            .create(e.device().clone())
            .unwrap();
        let [mut init_cb] = init_cp.alloc_array::<1>(true).unwrap();
        let cb_device = e.device().clone();
        let mut initialization_command_rec = unsafe { init_cb.begin_once(&cb_device).unwrap() };

        let utility_verts = UtilityVertices::new(e, &mut initialization_command_rec).unwrap();
        let skybox_precomputed = SkyboxPrecomputedTextures::new(e).unwrap();
        let main_render_pass = br::RenderPassBuilder2::new(
            &[
                // color
                br::AttachmentDescription2::new(br::vk::VK_FORMAT_R8G8B8A8_UNORM)
                    .with_layout_from(br::ImageLayout::Undefined.to(br::ImageLayout::General))
                    .color_memory_op(br::LoadOp::DontCare, br::StoreOp::Store),
                // hdr color
                br::AttachmentDescription2::new(br::vk::VK_FORMAT_R16G16B16A16_SFLOAT)
                    .with_layout_from(
                        br::ImageLayout::Undefined.to(br::ImageLayout::ShaderReadOnlyOpt),
                    )
                    .color_memory_op(br::LoadOp::Clear, br::StoreOp::DontCare),
                // depth
                br::AttachmentDescription2::new(br::vk::VK_FORMAT_D24_UNORM_S8_UINT)
                    .with_layout_from(
                        br::ImageLayout::Undefined.to(br::ImageLayout::DepthStencilAttachmentOpt),
                    )
                    .color_memory_op(br::LoadOp::Clear, br::StoreOp::DontCare),
            ],
            &[
                br::SubpassDescription2::new()
                    .colors(&[br::AttachmentReference2::color(
                        1,
                        br::ImageLayout::ColorAttachmentOpt,
                    )])
                    .depth_stencil(&br::AttachmentReference2::depth_stencil(
                        2,
                        br::ImageLayout::DepthStencilAttachmentOpt,
                    )),
                br::SubpassDescription2::new()
                    .colors(&[br::AttachmentReference2::color(
                        0,
                        br::ImageLayout::ColorAttachmentOpt,
                    )])
                    .inputs(&[br::AttachmentReference2::color(
                        1,
                        br::ImageLayout::ShaderReadOnlyOpt,
                    )])
                    .depth_stencil(&br::AttachmentReference2::depth_stencil(
                        2,
                        br::ImageLayout::DepthStencilAttachmentOpt,
                    )),
            ],
            &[
                br::SubpassDependency2::new(
                    br::SubpassIndex::Internal(0),
                    br::SubpassIndex::Internal(1),
                )
                .of_memory(
                    br::AccessFlags::COLOR_ATTACHMENT.write,
                    br::AccessFlags::SHADER.read,
                )
                .of_execution(
                    br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                    br::PipelineStageFlags::FRAGMENT_SHADER,
                ),
                br::SubpassDependency2::new(
                    br::SubpassIndex::Internal(1),
                    br::SubpassIndex::External,
                )
                .of_memory(
                    br::AccessFlags::COLOR_ATTACHMENT.write,
                    br::AccessFlags::MEMORY.read,
                )
                .of_execution(
                    br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                    br::PipelineStageFlags(0),
                ),
            ],
        )
        .create(e.device().clone())
        .expect("Failed to create main render pass");
        let hdr_render_subpass = 0;
        let ldr_gizmos_render_subpass = 1;

        let hdr_temp_rt = TempRT::new(
            e,
            br::ImageDesc::new(init_size, br::vk::VK_FORMAT_R16G16B16A16_SFLOAT)
                .as_color_attachment()
                .as_input_attachment(),
            br::AspectMask::COLOR,
            0..1,
            0..1,
        )
        .expect("Failed to create hdr temp rt");
        let depth_stencil_temp_rt = TempRT::new(
            e,
            br::ImageDesc::new(init_size, br::vk::VK_FORMAT_D24_UNORM_S8_UINT)
                .as_depth_stencil_attachment(),
            br::AspectMask::DEPTH.stencil(),
            0..1,
            0..1,
        )
        .expect("Failed to create depth stencil temp rt");

        let descriptor_set_layout_ia1 = br::DescriptorSetLayoutBuilder::new()
            .bind(
                br::DescriptorType::InputAttachment
                    .make_binding(1)
                    .only_for_fragment(),
            )
            .create(e.device().clone())
            .expect("Failed to create descriptor set layout");
        let descriptor_set_layout_ub1 = br::DescriptorSetLayoutBuilder::new()
            .bind(
                br::DescriptorType::UniformBuffer
                    .make_binding(1)
                    .for_shader_stage(br::ShaderStage::VERTEX | br::ShaderStage::FRAGMENT),
            )
            .create(e.device().clone())
            .expect("Failed to create descriptor set layout");
        let descriptor_set_layout_default_mat = br::DescriptorSetLayoutBuilder::new()
            .bind(
                br::DescriptorType::UniformBufferDynamic
                    .make_binding(1)
                    .only_for_vertex(),
            )
            .bind(
                br::DescriptorType::UniformBuffer
                    .make_binding(1)
                    .only_for_fragment(),
            )
            .create(e.device().clone())
            .expect("Failed to create descriptor set layout for default mat");

        let hdr_final_pass_vsh = e
            .shader("shaders/full_blit.vspv")
            .expect("Failed to load final pass vertex shader");
        let hdr_final_pass_fsh = e
            .shader("shaders/simple2d_hdr_final_pass.fspv")
            .expect("Failed to load final pass fragment shader");
        let hdr_final_pass_pipeline_layout =
            br::PipelineLayoutBuilder::new(vec![&descriptor_set_layout_ia1], vec![])
                .create(e.device().clone())
                .expect("Failed to create hdr final pass pipeline layout");
        let mut hdr_final_pass_pipeline = br::NonDerivedGraphicsPipelineBuilder::new(
            &hdr_final_pass_pipeline_layout,
            (&main_render_pass, ldr_gizmos_render_subpass),
            br::VertexProcessingStages::new(
                br::VertexShaderStage::new(br::PipelineShader2::new(&hdr_final_pass_vsh, c"main"))
                    .with_fragment_shader_stage(br::PipelineShader2::new(
                        &hdr_final_pass_fsh,
                        c"main",
                    )),
                &[],
                &[],
                br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP,
            ),
        );
        hdr_final_pass_pipeline
            .multisample_state(Some(br::MultisampleState::new()))
            .add_attachment_blend(br::AttachmentColorBlendState::noblend())
            .viewport_scissors(
                br::DynamicArrayState::Dynamic(1),
                br::DynamicArrayState::Dynamic(1),
            )
            .depth_test_settings(None, false);

        let grid_vsh = e
            .shader("shaders/simple_transformed_static_pos.vspv")
            .expect("Failed to load vertex shader");
        let grid_fsh = e
            .shader("shaders/vertex_color.fspv")
            .expect("Failed to load fragment shader");
        let (grid_vbinds, grid_vattrs) = ColoredVertex::single_binding(0, 1);
        let grid_pipeline_layout = br::PipelineLayoutBuilder::new(
            vec![&descriptor_set_layout_ub1],
            vec![(br::ShaderStage::VERTEX, 0..64)],
        )
        .create(e.device().clone())
        .expect("Failed to create grid pipeline layout");
        let mut grid_pipeline = br::NonDerivedGraphicsPipelineBuilder::new(
            &grid_pipeline_layout,
            (&main_render_pass, 1),
            br::VertexProcessingStages::new(
                br::VertexShaderStage::new(br::PipelineShader2::new(&grid_vsh, c"main"))
                    .with_fragment_shader_stage(br::PipelineShader2::new(&grid_fsh, c"main")),
                &grid_vbinds,
                &grid_vattrs,
                br::vk::VK_PRIMITIVE_TOPOLOGY_LINE_LIST,
            ),
        );
        let mut rasterization_state = br::RasterizationState::default();
        if e.has_extra_line_rasterization_enabled() {
            rasterization_state.line_state(br::RasterizationLineState::new(
                br::LineRasterizationMode::RectangularSmooth,
            ));
        }
        let multisample_state = br::MultisampleState::new();
        grid_pipeline
            .multisample_state(Some(multisample_state))
            .add_attachment_blend(br::AttachmentColorBlendState::premultiplied())
            .viewport_scissors(
                br::DynamicArrayState::Dynamic(1),
                br::DynamicArrayState::Dynamic(1),
            )
            .depth_test_settings(Some(br::CompareOp::LessOrEqual), true)
            .rasterization_state(rasterization_state);

        let default_material_vsh = e.shader("shaders/default_material.vspv").unwrap();
        let default_material_fsh = e.shader("shaders/default_material.fspv").unwrap();
        let default_material_vertex_binds =
            [br::VertexInputBindingDescription::per_vertex_typed::<
                GenericVertex,
            >(0)];
        let default_material_vertex_attrs = [
            br::vk::VkVertexInputAttributeDescription {
                location: 0,
                binding: 0,
                format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                offset: core::mem::offset_of!(GenericVertex, pos) as _,
            },
            br::vk::VkVertexInputAttributeDescription {
                location: 1,
                binding: 0,
                format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                offset: core::mem::offset_of!(GenericVertex, normal) as _,
            },
            br::vk::VkVertexInputAttributeDescription {
                location: 2,
                binding: 0,
                format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                offset: core::mem::offset_of!(GenericVertex, uv) as _,
            },
        ];
        let default_material_pipeline_layout = br::PipelineLayoutBuilder::new(
            vec![
                &descriptor_set_layout_ub1,
                &descriptor_set_layout_default_mat,
            ],
            vec![],
        )
        .create(e.device().clone())
        .unwrap();
        let mut default_material_raster_state = br::RasterizationState::default();
        default_material_raster_state.cull_mode(br::vk::VK_CULL_MODE_BACK_BIT);
        default_material_raster_state.front_face(br::vk::VK_FRONT_FACE_CLOCKWISE);
        let mut default_material_pipeline = br::NonDerivedGraphicsPipelineBuilder::new(
            &default_material_pipeline_layout,
            (&main_render_pass, hdr_render_subpass),
            br::VertexProcessingStages::new(
                br::VertexShaderStage::new(br::PipelineShader2::new(
                    &default_material_vsh,
                    c"main",
                ))
                .with_fragment_shader_stage(br::PipelineShader2::new(
                    &default_material_fsh,
                    c"main",
                )),
                &default_material_vertex_binds,
                &default_material_vertex_attrs,
                br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
            ),
        );
        default_material_pipeline
            .viewport_scissors(
                br::DynamicArrayState::Dynamic(1),
                br::DynamicArrayState::Dynamic(1),
            )
            .multisample_state(Some(br::MultisampleState::new()))
            .add_attachment_blend(br::AttachmentColorBlendState::premultiplied())
            .depth_test_settings(Some(br::CompareOp::LessOrEqual), true)
            .rasterization_state(default_material_raster_state);

        let hdr_final_pass_pipeline_extras = hdr_final_pass_pipeline.make_extras();
        let grid_pipeline_extras = grid_pipeline.make_extras();
        let default_material_pipeline_extras = default_material_pipeline.make_extras();
        let [hdr_final_pass_pipeline, grid_pipeline, default_material_pipeline] = e
            .create_graphics_pipeline_array(&[
                hdr_final_pass_pipeline.build(&hdr_final_pass_pipeline_extras),
                grid_pipeline.build(&grid_pipeline_extras),
                default_material_pipeline.build(&default_material_pipeline_extras),
            ])
            .expect("Failed to create grid pipeline state");

        let grid_vertices = Self::grid_vertices();
        let default_camera = Camera {
            projection: Some(ProjectionMethod::Perspective {
                fov: 60.0f32.to_radians(),
            }),
            position: peridot_math::Vector3(0.0, 1.6, -10.0),
            rotation: peridot_math::Quaternion::ONE,
            depth_range: 0.1..100.0,
        };
        let init_light_data = match app_state
            .read()
            .current_scene
            .objects
            .values()
            .find(|x| x.is_sunlight_object())
        {
            Some(x) => match x.details {
                ObjectDetails::SunLight {
                    rotation,
                    intensity,
                } => PrimaryDirectionalLightUniformData {
                    incident_light_dir: peridot_math::Matrix3::from(rotation.clone())
                        * peridot_math::Vector3(0.0f32, 0.0, -1.0),
                    light_intensity: intensity,
                },
                _ => unreachable!(),
            },
            None => PrimaryDirectionalLightUniformData {
                incident_light_dir: peridot_math::Vector3(0.0f32, -0.1, -1.0).normalize(),
                // incident_light_dir: peridot_math::Vector3(0.0f32, -0.8, -0.2).normalize(),
                light_intensity: 20.0,
            },
        };

        let [grid_buffer, camera_buffer, forward_light_buffer] = e
            .alloc_device_local_buffer_array([
                br::BufferDesc::new(
                    core::mem::size_of::<ColoredVertex>() * grid_vertices.len(),
                    br::BufferUsage::VERTEX_BUFFER.transfer_dest(),
                ),
                br::BufferDesc::new_for_type::<RenderCameraUniformData>(
                    br::BufferUsage::UNIFORM_BUFFER.transfer_dest(),
                ),
                br::BufferDesc::new_for_type::<ForwardLightUniformData>(
                    br::BufferUsage::UNIFORM_BUFFER.transfer_dest(),
                ),
            ])
            .expect("Failed to allocate device local buffers");
        let [mut grid_buffer_stg, mut camera_buffer_stg, mut forward_light_buffer_stg] = e
            .alloc_upload_buffer_array([
                br::BufferDesc::new(
                    core::mem::size_of::<ColoredVertex>() * grid_vertices.len(),
                    br::BufferUsage::TRANSFER_SRC,
                ),
                br::BufferDesc::new_for_type::<RenderCameraUniformData>(
                    br::BufferUsage::TRANSFER_SRC,
                ),
                br::BufferDesc::new_for_type::<ForwardLightUniformData>(
                    br::BufferUsage::TRANSFER_SRC,
                ),
            ])
            .expect("Failed to allocate upload buffers");
        grid_buffer_stg
            .clone_content_from_slice(&grid_vertices)
            .expect("Failed to write grid vbuffer content");
        camera_buffer_stg
            .write_content(RenderCameraUniformData {
                camera_view_projection_matrix: default_camera.view_projection_matrix(1.0),
                camera_inverse_view_matrix: default_camera.inverse_view_matrix(),
                camera_persp_fov_rad: 60.0f32.to_radians(),
                camera_aspect_wh: 1.0,
            })
            .expect("Failed to write camera matrix");
        forward_light_buffer_stg
            .write_content(ForwardLightUniformData {
                light_incident_dir: init_light_data.incident_light_dir.clone(),
                light_intensity: init_light_data.light_intensity,
            })
            .unwrap();

        // initialize
        initialization_command_rec
            .copy_buffer(
                &grid_buffer_stg,
                &grid_buffer,
                &[br::BufferCopy::mirror(0, grid_buffer.byte_length() as _)],
            )
            .copy_buffer(
                &camera_buffer_stg,
                &camera_buffer,
                &[br::BufferCopy::mirror_data::<RenderCameraUniformData>(0)],
            )
            .copy_buffer(
                &forward_light_buffer_stg,
                &forward_light_buffer,
                &[br::BufferCopy::mirror_data::<ForwardLightUniformData>(0)],
            )
            .pipeline_barrier_2(&br::DependencyInfo::new(
                &[br::MemoryBarrier2::new()
                    .from(
                        br::PipelineStageFlags2::COPY,
                        br::AccessFlags2::TRANSFER.write,
                    )
                    .to(
                        br::PipelineStageFlags2::VERTEX_INPUT
                            | br::PipelineStageFlags2::VERTEX_SHADER
                            | br::PipelineStageFlags2::FRAGMENT_SHADER,
                        br::AccessFlags2::VERTEX_ATTRIBUTE_READ | br::AccessFlags2::UNIFORM_READ,
                    )],
                &[],
                &[],
            ))
            .end()
            .expect("Failed to finish init commands");
        e.submit_graphics_works_and_wait(&[br::SubmitInfo2::new(
            &[],
            &[br::CommandBufferSubmitInfo::new(&init_cb)],
            &[],
        )])
        .expect("Failed to submit init commands");
        drop(init_cp);

        let per_object_uniform_data = PerObjectUniformData {
            array: ObjectUniformDataArrayBlock::new(e, 128).unwrap(),
            index_by_object_id: HashMap::new(),
        };

        let mut dp = br::DescriptorPoolBuilder::new(3)
            .with_reservations(vec![
                br::DescriptorType::UniformBuffer.with_count(2),
                br::DescriptorType::UniformBufferDynamic.with_count(1),
                br::DescriptorType::InputAttachment.with_count(1),
            ])
            .create(e.device().clone())
            .expect("Failed to create descriptor pool");
        let [camera_descriptor_set, hdr_final_pass_descriptor_set, per_object_descriptor_set] = dp
            .alloc_array(&[
                br::DescriptorSetLayoutObjectRef::new(&descriptor_set_layout_ub1),
                br::DescriptorSetLayoutObjectRef::new(&descriptor_set_layout_ia1),
                br::DescriptorSetLayoutObjectRef::new(&descriptor_set_layout_default_mat),
            ])
            .expect("Failed to allocate camera descriptor set");
        e.device().update_descriptor_sets(
            &[
                camera_descriptor_set
                    .binding_at(0)
                    .write(br::DescriptorContents::uniform_buffer(
                        &camera_buffer,
                        0..core::mem::size_of::<RenderCameraUniformData>() as u64,
                    )),
                hdr_final_pass_descriptor_set.binding_at(0).write(
                    br::DescriptorContents::input_attachment(
                        &hdr_temp_rt.resource,
                        br::ImageLayout::ShaderReadOnlyOpt,
                    ),
                ),
                per_object_descriptor_set.binding_at(0).write(
                    br::DescriptorContents::uniform_buffer_dynamic(
                        &per_object_uniform_data.array.buffer,
                        per_object_uniform_data.array.data_range(),
                    ),
                ),
                per_object_descriptor_set.binding_at(1).write(
                    br::DescriptorContents::uniform_buffer(
                        &forward_light_buffer,
                        0..core::mem::size_of::<ForwardLightUniformData>() as _,
                    ),
                ),
            ],
            &[],
        );

        let skybox_renderer = SkyboxRenderer::new(
            e,
            &descriptor_set_layout_ub1,
            &main_render_pass,
            hdr_render_subpass,
            skybox_precomputed,
            init_light_data,
        )
        .unwrap();

        Self {
            utility_verts,
            skybox_renderer,
            _descriptor_set_layout_ia1: descriptor_set_layout_ia1,
            _descriptor_set_layout_ub1: descriptor_set_layout_ub1,
            _descriptor_pool: dp,
            hdr_temp_rt,
            depth_stencil_temp_rt,
            main_render_pass,
            hdr_final_pass_pipeline_layout,
            hdr_final_pass_pipeline,
            grid_pipeline_layout,
            grid_pipeline,
            grid_buffer,
            grid_vertex_count: grid_vertices.len() as _,
            default_material_pipeline_layout,
            default_material_pipeline,
            camera_buffer,
            camera: default_camera,
            forward_light_buffer,
            camera_descriptor_set,
            hdr_final_pass_descriptor_set,
            per_object_descriptor_set,
            per_object_uniform_data,
        }
    }

    pub fn populate_commands(
        &self,
        rec: br::CmdRecord<
            impl br::VkHandleMut<Handle = br::vk::VkCommandBuffer> + ?Sized,
            impl br::Device + ?Sized,
        >,
        fb: &(impl br::Framebuffer + ?Sized),
        size: br::vk::VkExtent2D,
        app_state: &MTSharedMut<AppState>,
    ) {
        let rect = size.into_rect(br::vk::VkOffset2D::ZERO);
        let viewport = rect.make_viewport(0.0..1.0);

        rec.set_viewport(0, &[viewport])
            .set_scissor(0, &[rect])
            .begin_render_pass(
                &self.main_render_pass,
                fb,
                rect,
                &[
                    br::ClearValue::color_f32([0.0, 0.0, 0.0, 1.0]),
                    br::ClearValue::color_f32([0.0, 0.0, 0.0, 1.0]),
                    br::ClearValue::depth_stencil(1.0, 0),
                ],
                true,
            )
            .bind_graphics_pipeline_pair(
                &self.default_material_pipeline,
                &self.default_material_pipeline_layout,
            )
            .inject(|rec| {
                app_state
                    .read()
                    .current_scene
                    .objects
                    .values()
                    .fold(rec, |rec, o| match o.details {
                        ObjectDetails::Mesh {
                            ref vertex_buffer,
                            index_buffer: Some(ref index_buffer),
                            vertex_count,
                            ..
                        } => rec
                            .bind_graphics_descriptor_sets(
                                0,
                                &[
                                    self.camera_descriptor_set.0,
                                    self.per_object_descriptor_set.0,
                                ],
                                &[self
                                    .per_object_uniform_data
                                    .array
                                    .offset(self.per_object_uniform_data.index_by_object_id[&o.id])
                                    as _],
                            )
                            .bind_vertex_buffers(0, &[(&vertex_buffer, 0)])
                            .bind_index_buffer(&index_buffer, 0, br::IndexType::U16)
                            .draw_indexed(vertex_count, 1, 0, 0, 0),
                        ObjectDetails::Mesh {
                            ref vertex_buffer,
                            index_buffer: None,
                            vertex_count,
                            ..
                        } => rec
                            .bind_graphics_descriptor_sets(
                                0,
                                &[
                                    self.camera_descriptor_set.0,
                                    self.per_object_descriptor_set.0,
                                ],
                                &[self
                                    .per_object_uniform_data
                                    .array
                                    .offset(self.per_object_uniform_data.index_by_object_id[&o.id])
                                    as _],
                            )
                            .bind_vertex_buffers(0, &[(&vertex_buffer, 0)])
                            .draw(vertex_count, 1, 0, 0),
                        ObjectDetails::Camera { .. } | ObjectDetails::SunLight { .. } => rec,
                    })
            })
            // どうやらパイプライン切り替えると0番目のDescriptorSetが消滅するので再設定する（これ消えないはずだけどな......？）
            .bind_graphics_pipeline_layout(&self.skybox_renderer.pipeline_layout)
            .bind_graphics_descriptor_sets(0, &[self.camera_descriptor_set.0], &[])
            .inject(|rec| self.skybox_renderer.record_render_commands(rec))
            .next_subpass(true)
            .bind_graphics_pipeline_pair(
                &self.hdr_final_pass_pipeline,
                &self.hdr_final_pass_pipeline_layout,
            )
            .bind_graphics_descriptor_sets(0, &[self.hdr_final_pass_descriptor_set.0], &[])
            .draw(4, 1, 0, 0)
            .bind_graphics_pipeline_pair(&self.grid_pipeline, &self.grid_pipeline_layout)
            .bind_graphics_descriptor_sets(0, &[self.camera_descriptor_set.0], &[])
            .bind_vertex_buffers(0, &[(&self.grid_buffer, 0)])
            .push_graphics_constant(br::ShaderStage::VERTEX, 0, &Mat4::IDENTITY)
            .draw(self.grid_vertex_count, 1, 0, 0)
            .end_render_pass()
            .end()
            .expect("Failed to record commands");
    }

    pub fn resize(&mut self, new_size: br::vk::VkExtent2D) {
        self.hdr_temp_rt.recreate_newsize(new_size).unwrap();
        self.depth_stencil_temp_rt
            .recreate_newsize(new_size)
            .unwrap();
    }
}

pub enum EditorStageViewInputHoldState {
    Direction(peridot_math::Vector2F32),
    Position(peridot_math::Vector2F32),
}

pub struct EditorStageView {
    root: SpriteVisual,
    ht: HitTestTree,
    size: br::vk::VkExtent2D,
    render_resources: SharedMut<EditorStageRenderResources>,
    back_buffer_resources: Vec<(HANDLE, br::DeviceMemoryObject<StdVkDevice>)>,
    renderer: Rc<StageTabContentRenderer>,
    input_hold_state: Option<EditorStageViewInputHoldState>,
}
impl EditorStageView {
    pub fn new(
        _view_ctx: &impl ViewContext,
        app_state: &MTSharedMut<AppState>,
    ) -> windows::core::Result<SharedMut<Self>> {
        let init_size = br::vk::VkExtent2D::spread1(128);

        let composition_surface_handle = unsafe {
            DCompositionCreateSurfaceHandle(
                (COMPOSITIONOBJECT_READ | COMPOSITIONOBJECT_WRITE) as _,
                None,
            )?
        };
        let presentation_surface = unsafe {
            AppSubsystemInstances::get()
                .presentation_manager
                .CreatePresentationSurface(composition_surface_handle)?
        };
        let surface = unsafe {
            AppSubsystemInstances::get()
                .compositor_interop
                .CreateCompositionSurfaceForHandle(composition_surface_handle)?
        };
        unsafe {
            presentation_surface.SetSourceRect(&RECT {
                left: 0,
                top: 0,
                right: init_size.width as _,
                bottom: init_size.height as _,
            })?;

            presentation_surface.SetAlphaMode(DXGI_ALPHA_MODE_IGNORE)?;
            // TODO: G10(Linear色空間のはず)を使うとなんか挙動が怪しいのでいったんG22(Gamma補正バージョン)を使う
            // presentation_surface
            //     .SetColorSpace(DXGI_COLOR_SPACE_RGB_FULL_G10_NONE_P709)?;
            presentation_surface.SetColorSpace(DXGI_COLOR_SPACE_RGB_FULL_G22_NONE_P709)?;
        }

        let root = AppSubsystemInstances::get()
            .compositor
            .CreateSpriteVisual()?;
        root.set_properties()
            .brush(
                &AppSubsystemInstances::get()
                    .compositor
                    .CreateSurfaceBrushWithSurface(&surface)?,
            )?
            .size(Vector2::scalar(128.0))?
            .offset(Vector3::zero())?;

        let render_resources = EditorStageRenderResources::new(
            &mut AppSubsystemInstances::get().mini_engine.borrow_mut(),
            app_state,
            init_size,
        );

        let mut main_render_command_pool = AppSubsystemInstances::get()
            .mini_engine
            .borrow()
            .command_pool_builder_for_graphics_work()
            .create(
                AppSubsystemInstances::get()
                    .mini_engine
                    .borrow()
                    .device()
                    .clone(),
            )
            .expect("Failed to create command pool");
        let main_render_commands = main_render_command_pool
            .alloc(BACK_BUFFER_COUNT as _, true)
            .expect("Failed to allocate command buffers");

        let mut back_buffer_resources = Vec::with_capacity(3);
        let mut back_buffer_render_resources = Vec::with_capacity(3);
        for mut cb in main_render_commands.into_iter() {
            let texture_desc = d3d11_presentation_texture_desc(init_size.width, init_size.height);
            let texture = texture_desc.create(&AppSubsystemInstances::get().d3d11_device)?;
            let presentation_buffer = unsafe {
                AppSubsystemInstances::get()
                    .presentation_manager
                    .AddBufferFromResource(&texture)?
            };
            let eh = unsafe { presentation_buffer.GetAvailableEvent()? };

            let rt_desc = D3D11_TEXTURE2D_DESC {
                BindFlags: D3D11_BIND_RENDER_TARGET.0 as _,
                MiscFlags: (D3D11_RESOURCE_MISC_SHARED_NTHANDLE
                    | D3D11_RESOURCE_MISC_SHARED_KEYEDMUTEX)
                    .0 as _,
                ..texture_desc
            };
            let rt = rt_desc.create(&AppSubsystemInstances::get().d3d11_device)?;

            let tex_handle = unsafe {
                rt.cast::<IDXGIResource1>()?.CreateSharedHandle(
                    None,
                    GENERIC_ALL.0 | DXGI_SHARED_RESOURCE_READ | DXGI_SHARED_RESOURCE_WRITE,
                    None,
                )?
            };
            let external_handle = br::ExternalMemoryHandleTypeWin32::D3D11Texture
                .with_handle(unsafe { core::mem::transmute(tex_handle.0) });
            let external_handle_image_memory_req = unsafe {
                external_handle
                    .properties(
                        AppSubsystemInstances::get().mini_engine.borrow().device(),
                        br::vk::VkMemoryWin32HandlePropertiesKHR::uninit_sink(),
                    )
                    .expect("Failed to query external handle memory properties")
            };
            let mut imported_image =
                br::ImageDesc::new(init_size, br::vk::VK_FORMAT_R8G8B8A8_UNORM)
                    .as_color_attachment()
                    .exportable_as(br::ExternalMemoryHandleTypes::D3D11_TEXTURE)
                    .create(
                        AppSubsystemInstances::get()
                            .mini_engine
                            .borrow()
                            .device()
                            .clone(),
                    )
                    .expect("Failed to create external backbuffer image");
            let imported_image_memory_req = imported_image.requirements();
            let imported_memory_index = AppSubsystemInstances::get()
                .mini_engine
                .borrow()
                .find_device_local_memory_index(
                    imported_image_memory_req.memoryTypeBits
                        & external_handle_image_memory_req.memoryTypeBits,
                )
                .expect("no suitable memory");
            let imported_image_memory = external_handle
                .into_import_request(imported_memory_index, None)
                .execute(
                    AppSubsystemInstances::get()
                        .mini_engine
                        .borrow()
                        .device()
                        .clone(),
                )
                .expect("Failed to import d3d11 memory");
            imported_image
                .bind(&imported_image_memory, 0)
                .expect("Failed to bind image to memory");
            let imported_image = Rc::new(imported_image);

            let vk_framebuffer = br::FramebufferBuilder::new(&render_resources.main_render_pass)
                .with_attachment(
                    imported_image
                        .clone()
                        .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                        .view_builder()
                        .create()
                        .expect("Failed to create image view"),
                )
                .with_attachment(render_resources.hdr_temp_rt.resource.clone())
                .with_attachment(render_resources.depth_stencil_temp_rt.resource.clone())
                .create()
                .expect("Failed to create framebuffer");

            render_resources.populate_commands(
                unsafe {
                    cb.begin(AppSubsystemInstances::get().mini_engine.borrow().device())
                        .unwrap()
                },
                &vk_framebuffer,
                init_size,
                app_state,
            );

            let rt_mutex = rt.cast::<IDXGIKeyedMutex>()?;
            back_buffer_render_resources.push(BackBuffer {
                presentation_buffer,
                command_buffer: RefCell::new(cb),
                final_destination: texture,
                render_target: rt,
                keyed_mutex: rt_mutex,
                framebuffer: vk_framebuffer,
            });
            back_buffer_resources.push((eh, imported_image_memory));
        }

        let render_resources = new_shared_mut(render_resources);
        Ok(new_cyclic_shared_mut(move |wthis| {
            let ht = HitTestTree::new(
                Some(wthis.clone()),
                Rect::from_size(init_size.width as _, init_size.height as _),
                Rect::empty(),
            );

            Self {
                root,
                ht,
                size: init_size,
                render_resources: render_resources.clone(),
                renderer: Rc::new(StageTabContentRenderer {
                    main_command_pool: RefCell::new(main_render_command_pool),
                    render_resources,
                    back_buffers: back_buffer_render_resources,
                    buffer_size: init_size,
                    presentation_manager: AppSubsystemInstances::get().presentation_manager.clone(),
                    presentation_surface,
                    graphics_queue: AppSubsystemInstances::get()
                        .mini_engine
                        .borrow()
                        .graphics_queue()
                        .clone(),
                    d3d11_device_context: unsafe {
                        AppSubsystemInstances::get()
                            .d3d11_device
                            .GetImmediateContext()
                            .expect("Failed to get d3d imm context")
                    },
                    app_state: Arc::downgrade(app_state),
                }),
                back_buffer_resources,
                input_hold_state: None,
            }
        }))
    }

    fn resize(
        &mut self,
        new_size: Vector2,
        resize_ctx: &ResizeContext,
    ) -> windows::core::Result<()> {
        self.root.SetSize(new_size)?;
        self.ht.set_size(new_size.X, new_size.Y);

        for n in 0..BACK_BUFFER_COUNT {
            AppGlobalSignals::get_mut().unregister(&self.renderer, n);
        }

        let renderer_mut = Rc::get_mut(&mut self.renderer).unwrap();
        renderer_mut
            .main_command_pool
            .get_mut()
            .reset(true)
            .expect("Failed to reset old commands");

        let buffer_real_size = br::vk::VkExtent2D {
            width: (new_size.X * resize_ctx.current_dpi / 96.0) as _,
            height: (new_size.Y * resize_ctx.current_dpi / 96.0) as _,
        };
        self.size = buffer_real_size;
        renderer_mut.buffer_size = buffer_real_size;

        unsafe {
            renderer_mut.presentation_surface.SetSourceRect(&RECT {
                left: 0,
                top: 0,
                right: buffer_real_size.width as _,
                bottom: buffer_real_size.height as _,
            })?;
        }

        self.render_resources.borrow_mut().resize(buffer_real_size);

        let mut camera_upload_buffer = AppSubsystemInstances::get()
            .mini_engine
            .borrow_mut()
            .alloc_upload_buffer(br::BufferDesc::new_for_type::<RenderCameraUniformData>(
                br::BufferUsage::TRANSFER_SRC,
            ))
            .expect("Failed to create upload buffer");
        camera_upload_buffer
            .write_content(RenderCameraUniformData {
                camera_view_projection_matrix: self
                    .render_resources
                    .borrow()
                    .camera
                    .view_projection_matrix(new_size.X / new_size.Y),
                camera_inverse_view_matrix: self
                    .render_resources
                    .borrow()
                    .camera
                    .inverse_view_matrix(),
                // TODO: ここの値はself.cameraから取りたい
                camera_persp_fov_rad: 60.0f32.to_radians(),
                camera_aspect_wh: new_size.X / new_size.Y,
            })
            .expect("Failed to write camera vp matrix");

        let mini_engine_ref = AppSubsystemInstances::get().mini_engine.borrow();
        let mut cp = mini_engine_ref
            .command_pool_builder_for_graphics_work()
            .transient()
            .create(mini_engine_ref.device())
            .expect("Failed to create transient command pool");
        let [mut cb] = cp
            .alloc_array::<1>(true)
            .expect("Failed to allocate command buffer");
        unsafe {
            cb.begin_once(mini_engine_ref.device())
                .expect("Failed to begin commands")
        }
        .copy_buffer(
            &camera_upload_buffer,
            &self.render_resources.borrow().camera_buffer,
            &[br::BufferCopy::mirror_data::<RenderCameraUniformData>(0)],
        )
        .pipeline_barrier_2(&br::DependencyInfo::new(
            &[br::MemoryBarrier2::new()
                .from(
                    br::PipelineStageFlags2::COPY,
                    br::AccessFlags2::TRANSFER.write,
                )
                .to(
                    br::PipelineStageFlags2::VERTEX_SHADER,
                    br::AccessFlags2::UNIFORM_READ,
                )],
            &[],
            &[],
        ))
        .end()
        .expect("Failed to finish updating commands");
        mini_engine_ref
            .submit_graphics_works_and_wait(&[br::SubmitInfo2::new(
                &[],
                &[br::CommandBufferSubmitInfo::new(&cb)],
                &[],
            )])
            .expect("Failed to submit updating commands");
        drop(cp);
        drop(mini_engine_ref);

        AppSubsystemInstances::get()
            .mini_engine
            .borrow()
            .device()
            .update_descriptor_sets(
                &[self
                    .render_resources
                    .borrow()
                    .hdr_final_pass_descriptor_set
                    .binding_at(0)
                    .write(br::DescriptorContents::input_attachment(
                        &self.render_resources.borrow().hdr_temp_rt.resource,
                        br::ImageLayout::ShaderReadOnlyOpt,
                    ))],
                &[],
            );

        let resources = self.render_resources.borrow();
        for (renderer, bb) in renderer_mut
            .back_buffers
            .iter_mut()
            .zip(self.back_buffer_resources.iter_mut())
        {
            let texture_desc =
                d3d11_presentation_texture_desc(buffer_real_size.width, buffer_real_size.height);
            let texture = texture_desc
                .create(&AppSubsystemInstances::get().d3d11_device)
                .expect("Failed to create back buffer texture");
            let presentation_buffer = unsafe {
                AppSubsystemInstances::get()
                    .presentation_manager
                    .AddBufferFromResource(&texture)
                    .expect("Failed to add texture as presentation buffer")
            };
            let eh = unsafe {
                presentation_buffer
                    .GetAvailableEvent()
                    .expect("Failed to get available event handle")
            };

            let rt = D3D11_TEXTURE2D_DESC {
                BindFlags: D3D11_BIND_RENDER_TARGET.0 as _,
                MiscFlags: (D3D11_RESOURCE_MISC_SHARED_NTHANDLE
                    | D3D11_RESOURCE_MISC_SHARED_KEYEDMUTEX)
                    .0 as _,
                ..texture_desc
            }
            .create(&AppSubsystemInstances::get().d3d11_device)
            .expect("Failed to create render target texture");

            let texture_res = rt
                .cast::<IDXGIResource1>()
                .expect("Failed to query underlying resource");
            let tex_handle = unsafe {
                texture_res
                    .CreateSharedHandle(
                        None,
                        GENERIC_ALL.0 | DXGI_SHARED_RESOURCE_READ | DXGI_SHARED_RESOURCE_WRITE,
                        None,
                    )
                    .expect("Failed to get shared handle")
            };
            let external_handle = br::ExternalMemoryHandleTypeWin32::D3D11Texture
                .with_handle(unsafe { core::mem::transmute(tex_handle.0) });
            let external_handle_image_memory_req = unsafe {
                external_handle
                    .properties(
                        AppSubsystemInstances::get().mini_engine.borrow().device(),
                        br::vk::VkMemoryWin32HandlePropertiesKHR::uninit_sink(),
                    )
                    .expect("Failed to query external handle memory properties")
            };
            let mut vk_image =
                br::ImageDesc::new(buffer_real_size.clone(), br::vk::VK_FORMAT_R8G8B8A8_UNORM)
                    .as_color_attachment()
                    .exportable_as(br::ExternalMemoryHandleTypes::D3D11_TEXTURE)
                    .create(
                        AppSubsystemInstances::get()
                            .mini_engine
                            .borrow()
                            .device()
                            .clone(),
                    )
                    .expect("Failed to create external backbuffer image");
            let vk_image_memory_req = vk_image.requirements();
            let vk_memory_index = AppSubsystemInstances::get()
                .mini_engine
                .borrow()
                .find_device_local_memory_index(
                    vk_image_memory_req.memoryTypeBits
                        & external_handle_image_memory_req.memoryTypeBits,
                )
                .expect("no suitable memory");
            let vk_image_memory = external_handle
                .into_import_request(vk_memory_index, None)
                .execute(
                    AppSubsystemInstances::get()
                        .mini_engine
                        .borrow()
                        .device()
                        .clone(),
                )
                .expect("Failed to import d3d11 memory");
            vk_image
                .bind(&vk_image_memory, 0)
                .expect("Failed to bind image to memory");
            let vk_image = Rc::new(vk_image);

            let vk_framebuffer = br::FramebufferBuilder::new(&resources.main_render_pass)
                .with_attachment(
                    vk_image
                        .clone()
                        .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                        .view_builder()
                        .create()
                        .expect("Failed to create image view"),
                )
                .with_attachment(resources.hdr_temp_rt.resource.clone())
                .with_attachment(resources.depth_stencil_temp_rt.resource.clone())
                .create()
                .expect("Failed to create framebuffer");

            resources.populate_commands(
                unsafe {
                    renderer
                        .command_buffer
                        .get_mut()
                        .begin(AppSubsystemInstances::get().mini_engine.borrow().device())
                        .expect("Failed to begin command recording")
                },
                &vk_framebuffer,
                buffer_real_size,
                &renderer_mut.app_state.upgrade().unwrap(),
            );

            renderer.keyed_mutex = rt
                .cast::<IDXGIKeyedMutex>()
                .expect("Failed to get keyed mutex");

            renderer.presentation_buffer = presentation_buffer;
            renderer.final_destination = texture;
            renderer.render_target = rt;
            renderer.framebuffer = vk_framebuffer;
            bb.0 = eh;
            bb.1 = vk_image_memory;
        }

        for (n, (e, _)) in self.back_buffer_resources.iter().enumerate() {
            AppGlobalSignals::get_mut().register(*e, &self.renderer, n);
        }

        Ok(())
    }
}
impl MountableView2 for EditorStageView {
    fn mount(&self, onto: &VisualCollection, onto_ht: &HitTestTree) -> windows::core::Result<()> {
        onto.InsertAtTop(&self.root)?;
        onto_ht.add_child(&self.ht);

        for (n, (e, _)) in self.back_buffer_resources.iter().enumerate() {
            AppGlobalSignals::get_mut().register(*e, &self.renderer, n);
        }

        Ok(())
    }

    fn unmount(&self) -> windows::core::Result<()> {
        self.root.Parent()?.Children()?.Remove(&self.root)?;
        self.ht.unmount();

        for (n, _) in self.back_buffer_resources.iter().enumerate() {
            AppGlobalSignals::get_mut().unregister(&self.renderer, n);
        }

        Ok(())
    }
}
impl InputEventHandler for WeakMut<EditorStageView> {
    fn on_pointer_down(&self, x: f32, y: f32, ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };

        this.borrow_mut().input_hold_state = Some(EditorStageViewInputHoldState::Direction(
            peridot_math::Vector2(x, y),
        ));
        ctx.capture_mouse();
        unsafe {
            ShowCursor(false);
        }
    }

    fn on_wheel_down(&self, x: f32, y: f32, ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };

        this.borrow_mut().input_hold_state = Some(EditorStageViewInputHoldState::Position(
            peridot_math::Vector2(x, y),
        ));
        ctx.capture_mouse();
        unsafe {
            ShowCursor(false);
        }
    }

    fn on_drag_move(&self, x: f32, y: f32, window: HWND, _ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };

        match this.borrow().input_hold_state {
            Some(EditorStageViewInputHoldState::Direction(base)) => {
                let app_window = AppWindow::wrap(window);
                let mut points = [POINT {
                    x: app_window.dip_to_pixels(base.0) as _,
                    y: app_window.dip_to_pixels(base.1) as _,
                }];
                app_window.map_points_to_desktop(&mut points);
                unsafe {
                    SetCursorPos(points[0].x, points[0].y).expect("Failed to hold cursor");
                }

                let d = peridot_math::Vector2(x, y) - base;
                const DRAG_SENSITIVITY: f32 = 0.05f32;
                let yrot = peridot_math::Quaternion::new(
                    d.1 * DRAG_SENSITIVITY.to_radians(),
                    peridot_math::Matrix3F32::from(
                        this.borrow().render_resources.borrow().camera.rotation,
                    ) * peridot_math::Vector3::left(),
                );
                this.borrow().render_resources.borrow_mut().camera.rotation *= yrot;
                this.borrow().render_resources.borrow_mut().camera.rotation *=
                    peridot_math::Quaternion::new(
                        d.0 * DRAG_SENSITIVITY.to_radians(),
                        peridot_math::Vector3::down(),
                    );
            }
            Some(EditorStageViewInputHoldState::Position(base)) => {
                let app_window = AppWindow::wrap(window);
                let mut points = [POINT {
                    x: app_window.dip_to_pixels(base.0) as _,
                    y: app_window.dip_to_pixels(base.1) as _,
                }];
                app_window.map_points_to_desktop(&mut points);
                unsafe {
                    SetCursorPos(points[0].x, points[0].y).expect("Failed to hold cursor");
                }

                let mut d = peridot_math::Vector2(x, y) - base;
                d.0 *= -1.0;
                let dir = peridot_math::Matrix3::from(
                    this.borrow()
                        .render_resources
                        .borrow_mut()
                        .camera
                        .rotation
                        .clone(),
                ) * (d * 0.01).with_z(0.0);
                this.borrow().render_resources.borrow_mut().camera.position += dir;
            }
            None => {
                return;
            }
        }

        let current_size = this.borrow().ht.rect().clone();
        let mut camera_upload_buffer = AppSubsystemInstances::get()
            .mini_engine
            .borrow_mut()
            .alloc_upload_buffer(br::BufferDesc::new(
                core::mem::size_of::<RenderCameraUniformData>(),
                br::BufferUsage::TRANSFER_SRC,
            ))
            .expect("Failed to create upload buffer");
        camera_upload_buffer
            .write_content(RenderCameraUniformData {
                camera_view_projection_matrix: this
                    .borrow()
                    .render_resources
                    .borrow()
                    .camera
                    .view_projection_matrix(current_size.Width / current_size.Height),
                camera_inverse_view_matrix: this
                    .borrow()
                    .render_resources
                    .borrow()
                    .camera
                    .inverse_view_matrix(),
                // TODO: ここの値はcameraから取りたい
                camera_persp_fov_rad: 60.0f32.to_radians(),
                camera_aspect_wh: current_size.Width / current_size.Height,
            })
            .expect("Failed to write camera vp matrix");

        AppSubsystemInstances::get()
            .mini_engine
            .borrow_mut()
            .submit_transient_commands_and_wait(|rec| {
                rec.copy_buffer(
                    &camera_upload_buffer,
                    &this.borrow().render_resources.borrow().camera_buffer,
                    &[br::BufferCopy::mirror_data::<RenderCameraUniformData>(0)],
                )
                .pipeline_barrier_2(&br::DependencyInfo::new(
                    &[br::MemoryBarrier2::new()
                        .of_memory(
                            br::AccessFlags2::TRANSFER.write,
                            br::AccessFlags2::UNIFORM_READ,
                        )
                        .of_execution(
                            br::PipelineStageFlags2::COPY,
                            br::PipelineStageFlags2::VERTEX_SHADER,
                        )],
                    &[],
                    &[],
                ))
            })
            .expect("Failed to submit updating commands");
    }

    fn on_pointer_up(&self, _x: f32, _y: f32, ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };

        if !matches!(
            this.borrow().input_hold_state,
            Some(EditorStageViewInputHoldState::Direction(_)),
        ) {
            return;
        }

        this.borrow_mut().input_hold_state = None;
        unsafe {
            ShowCursor(true);
        }
        ctx.release_mouse_capture();
    }

    fn on_wheel_up(&self, _x: f32, _y: f32, ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };

        if !matches!(
            this.borrow().input_hold_state,
            Some(EditorStageViewInputHoldState::Position(_)),
        ) {
            return;
        }

        this.borrow_mut().input_hold_state = None;
        unsafe {
            ShowCursor(true);
        }
        ctx.release_mouse_capture();
    }

    fn on_wheel_roll(&self, _x: f32, _y: f32, amount: f32, _ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };

        let dir = peridot_math::Matrix3::from(
            this.borrow()
                .render_resources
                .borrow()
                .camera
                .rotation
                .clone(),
        ) * peridot_math::Vector3(0.0, 0.0, amount * 0.1);
        this.borrow().render_resources.borrow_mut().camera.position += dir;

        let current_size = this.borrow().ht.rect().clone();
        let mut camera_upload_buffer = AppSubsystemInstances::get()
            .mini_engine
            .borrow_mut()
            .alloc_upload_buffer(br::BufferDesc::new_for_type::<RenderCameraUniformData>(
                br::BufferUsage::TRANSFER_SRC,
            ))
            .expect("Failed to create upload buffer");
        camera_upload_buffer
            .write_content(RenderCameraUniformData {
                camera_view_projection_matrix: this
                    .borrow()
                    .render_resources
                    .borrow()
                    .camera
                    .view_projection_matrix(current_size.Width / current_size.Height),
                camera_inverse_view_matrix: this
                    .borrow()
                    .render_resources
                    .borrow()
                    .camera
                    .inverse_view_matrix(),
                // TODO: ここの値はcameraから取りたい
                camera_persp_fov_rad: 60.0f32.to_radians(),
                camera_aspect_wh: current_size.Width / current_size.Height,
            })
            .expect("Failed to write camera vp matrix");

        AppSubsystemInstances::get()
            .mini_engine
            .borrow_mut()
            .submit_transient_commands_and_wait(|rec| {
                rec.copy_buffer(
                    &camera_upload_buffer,
                    &this.borrow().render_resources.borrow().camera_buffer,
                    &[br::BufferCopy::mirror_data::<RenderCameraUniformData>(0)],
                )
                .pipeline_barrier_2(&br::DependencyInfo::new(
                    &[br::MemoryBarrier2::new()
                        .of_memory(
                            br::AccessFlags2::TRANSFER.write,
                            br::AccessFlags2::UNIFORM_READ,
                        )
                        .of_execution(
                            br::PipelineStageFlags2::COPY,
                            br::PipelineStageFlags2::VERTEX_SHADER,
                        )],
                    &[],
                    &[],
                ))
            })
            .expect("Failed to submit updating commands");
    }
}

pub struct PreviewTabPresenter {}
impl PaneTabContentPresenter for PreviewTabPresenter {
    fn build_content_view(
        &mut self,
        _onto: &ContainerVisual,
        _onto_ht: &HitTestTree,
        _view_context: &dyn ViewContext,
        _app_state: &MTSharedMut<AppState>,
    ) -> windows::core::Result<()> {
        Ok(())
    }

    fn on_hide_content_view(
        &mut self,
        _view_context: &dyn ViewContext,
        _app_state: &MTSharedMut<AppState>,
    ) -> windows::core::Result<()> {
        Ok(())
    }
}
impl PaneTabPresenter for PreviewTabPresenter {
    const INIT_TAB_NAME: &'static str = "Preview";

    fn new(
        _tab_header_view: &SharedMut<PaneTabHeaderView>,
        _view_ctx: &(impl ViewContext + ?Sized),
        _app_state: &MTSharedMut<AppState>,
    ) -> Self {
        Self {}
    }
}

pub struct ObjectTreeElementRowView {
    root: ContainerVisual,
    ht: HitTestTree,
    bg: SpriteVisual,
    bg_hover_animation: ScalarKeyFrameAnimation,
    bg_hover_end_animation: ScalarKeyFrameAnimation,
    bound_object_id: Uuid,
}
impl ObjectTreeElementRowView {
    const PADDING_Y: f32 = 2.0;
    const PADDING_X: f32 = 8.0;
    const HOVER_ANIMATION_DURATION: TimeSpan = timespan_ms(50);
    const HOVER_COLOR: Color = Color {
        A: 16,
        R: 255,
        G: 255,
        B: 255,
    };

    pub fn new(
        ref_dpi: f32,
        init_name: impl Into<Cow<'static, str>>,
        bound_object_id: Uuid,
    ) -> windows::core::Result<SharedMut<Self>> {
        let label_fmt = AppSubsystemInstances::get()
            .text_format_stock
            .borrow_mut()
            .get("system-ui", 12.0, DWRITE_FONT_WEIGHT_NORMAL)?;
        let label_surface = AppSubsystemInstances::get()
            .text_surface_stock
            .borrow_mut()
            .get(&label_fmt, ref_dpi, init_name)?;

        let root = AppSubsystemInstances::get()
            .compositor
            .CreateContainerVisual()?;
        root.set_properties()
            .size(Vector2 {
                X: 0.0,
                Y: label_surface.height + Self::PADDING_Y * 2.0,
            })?
            .relative_size_adjustment(Vector2 { X: 1.0, Y: 0.0 })?;

        let bg = AppSubsystemInstances::get()
            .compositor
            .CreateSpriteVisual()?;
        bg.set_properties()
            .brush(
                &AppSubsystemInstances::get()
                    .compositor
                    .CreateColorBrushWithColor(Self::HOVER_COLOR)?,
            )?
            .expand_to_parent()?
            .opacity(0.0)?;
        root.Children()?.InsertAtTop(&bg)?;

        let label = AppSubsystemInstances::get()
            .compositor
            .CreateSpriteVisual()?;
        label
            .set_properties()
            .brush(
                &AppSubsystemInstances::get()
                    .compositor
                    .CreateSurfaceBrushWithSurface(&label_surface.surface)?,
            )?
            .rect(&Rect {
                X: Self::PADDING_X,
                Y: Self::PADDING_Y,
                Width: label_surface.width,
                Height: label_surface.height,
            })?;
        root.Children()?.InsertAtTop(&label)?;

        let linear_easing = AppSubsystemInstances::get()
            .compositor
            .CreateLinearEasingFunction()?;
        let bg_hover_animation = AppSubsystemInstances::get()
            .compositor
            .CreateScalarKeyFrameAnimation()?;
        bg_hover_animation
            .keyframe(0.0, 0.0)?
            .interpolate(1.0, 1.0, &linear_easing)?
            .set_properties()
            .duration(Self::HOVER_ANIMATION_DURATION)?;
        let bg_hover_end_animation = AppSubsystemInstances::get()
            .compositor
            .CreateScalarKeyFrameAnimation()?;
        bg_hover_end_animation
            .keyframe(0.0, 1.0)?
            .interpolate(1.0, 0.0, &linear_easing)?
            .set_properties()
            .duration(Self::HOVER_ANIMATION_DURATION)?;

        Ok(new_cyclic_shared_mut(|wthis| {
            let ht = HitTestTree::new(
                Some(wthis.clone()),
                Rect::from_size(core::f32::MAX, label_surface.height + Self::PADDING_Y * 2.0),
                Rect::empty(),
            );

            Self {
                root,
                ht,
                bg,
                bg_hover_animation,
                bg_hover_end_animation,
                bound_object_id,
            }
        }))
    }

    pub fn height(&self) -> f32 {
        self.ht.rect().Height
    }

    pub fn reposition(&mut self, pos: Vector2) -> windows::core::Result<()> {
        self.root.SetOffset(pos.with_z(0.0))?;
        self.ht.set_offset(pos.X, pos.Y);

        Ok(())
    }
}
impl MountableView2 for ObjectTreeElementRowView {
    fn mount(&self, onto: &VisualCollection, onto_ht: &HitTestTree) -> windows::core::Result<()> {
        onto.InsertAtTop(&self.root)?;
        onto_ht.add_child(&self.ht);

        Ok(())
    }

    fn unmount(&self) -> windows::core::Result<()> {
        self.root.Parent()?.Children()?.Remove(&self.root)?;
        self.ht.unmount();

        Ok(())
    }
}
impl InputEventHandler for WeakMut<ObjectTreeElementRowView> {
    fn on_pointer_enter(&self, _ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };

        this.borrow()
            .bg
            .StartAnimation(h!("Opacity"), &this.borrow().bg_hover_animation)
            .expect("Failed to start hover animation");
    }

    fn on_pointer_leave(&self, _ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };

        this.borrow()
            .bg
            .StartAnimation(h!("Opacity"), &this.borrow().bg_hover_end_animation)
            .expect("Failed to start hover animation");
    }

    fn on_click(&self, window: HWND, mut ctx: &mut dyn InputContext) {
        let app_window = AppWindow::wrap(window);

        let Some(this) = self.upgrade() else {
            return;
        };
        let Some(state) = app_window.get_state_store() else {
            return;
        };

        AppState::set_current_selection(
            &state.app_state,
            Some(this.borrow().bound_object_id.clone()),
            &mut ctx,
        );
    }

    fn on_sub_pointer_up(&self, x: f32, y: f32, window: HWND, ctx: &mut dyn InputContext) {
        let mut p = [POINT {
            x: x as _,
            y: y as _,
        }];
        unsafe {
            MapWindowPoints(window, None, &mut p);
        }

        ContextMenu::get_mut()
            .pop_new(
                &[
                    MenuItem::SubMenu(
                        "Create child...".into(),
                        vec![
                            MenuItem::Command(
                                "Empty".into(),
                                new_shared_mut(|| println!("Create Empty")),
                                true,
                            ),
                            MenuItem::Header("General Meshes".into()),
                            MenuItem::Command(
                                "Cube".into(),
                                new_shared_mut(|| println!("Create Cube")),
                                true,
                            ),
                            MenuItem::Command(
                                "Plane".into(),
                                new_shared_mut(|| println!("Create Plane")),
                                true,
                            ),
                            MenuItem::Command(
                                "Icosphere".into(),
                                new_shared_mut(|| println!("Create Icosphere")),
                                true,
                            ),
                            MenuItem::Command(
                                "Cylinder".into(),
                                new_shared_mut(|| println!("Create Cylinder")),
                                true,
                            ),
                            MenuItem::Command(
                                "Capsule".into(),
                                new_shared_mut(|| println!("Create Capsule")),
                                true,
                            ),
                            MenuItem::Header("Special".into()),
                            MenuItem::Command(
                                "Terrain".into(),
                                new_shared_mut(|| println!("Create Terrain")),
                                true,
                            ),
                        ],
                    ),
                    MenuItem::Command(
                        "Create Empty at Parent".into(),
                        new_shared_mut(|| println!("Create Empty at Parent")),
                        true,
                    ),
                    MenuItem::Command(
                        "Delete".into(),
                        new_shared_mut(|| println!("Delete Object")),
                        true,
                    ),
                ],
                p[0].x as _,
                p[0].y as _,
                ctx.current_dpi(),
            )
            .expect("Failed to pop context menu");
    }
}

pub struct ObjectTreeTabPresenter {
    rows: SharedMut<Vec<SharedMut<ObjectTreeElementRowView>>>,
    app_state: MTSharedMut<AppState>,
    mounted_visual_root: Option<ContainerVisual>,
    mounted_ht: Option<HitTestTree>,
}
impl PaneTabContentPresenter for ObjectTreeTabPresenter {
    fn build_content_view(
        &mut self,
        onto: &ContainerVisual,
        onto_ht: &HitTestTree,
        _view_context: &dyn ViewContext,
        _app_state: &MTSharedMut<AppState>,
    ) -> windows::core::Result<()> {
        let children = onto.Children()?;
        for r in self.rows.borrow().iter() {
            r.borrow().mount(&children, onto_ht)?;
        }

        self.mounted_visual_root = Some(onto.clone());
        self.mounted_ht = Some(onto_ht.clone());

        Ok(())
    }

    fn on_hide_content_view(
        &mut self,
        _view_context: &dyn ViewContext,
        _app_state: &MTSharedMut<AppState>,
    ) -> windows::core::Result<()> {
        for r in self.rows.borrow().iter() {
            r.borrow().unmount()?;
        }

        self.mounted_visual_root = None;
        self.mounted_ht = None;

        Ok(())
    }

    fn on_context_menu(
        &mut self,
        desktop_x_px: f32,
        desktop_y_px: f32,
        input_context: &dyn InputContext,
    ) {
        ContextMenu::get_mut()
            .pop_new(
                &[
                    MenuItem::SubMenu(
                        "Create...".into(),
                        vec![
                            MenuItem::Command(
                                "Empty".into(),
                                new_shared_mut(|| println!("Create Empty")),
                                true,
                            ),
                            MenuItem::Header("General Meshes".into()),
                            MenuItem::Command(
                                "Cube".into(),
                                {
                                    let app_state = self.app_state.clone();
                                    let rows = self.rows.clone();
                                    let mounted_visual_root =
                                        self.mounted_visual_root.clone().unwrap();
                                    let mounted_ht = self.mounted_ht.clone().unwrap();
                                    let ref_dpi = input_context.current_dpi();

                                    new_shared_mut(move || {
                                        let [vertex_buffer, index_buffer] =
                                            AppSubsystemInstances::get()
                                                .mini_engine
                                                .borrow_mut()
                                                .alloc_device_local_buffer_array([
                                                    br::BufferDesc::new(
                                                        core::mem::size_of::<GenericVertex>() * 24,
                                                        br::BufferUsage::VERTEX_BUFFER
                                                            .transfer_dest(),
                                                    ),
                                                    br::BufferDesc::new(
                                                        core::mem::size_of::<u16>() * 36,
                                                        br::BufferUsage::INDEX_BUFFER
                                                            .transfer_dest(),
                                                    ),
                                                ])
                                                .expect("Failed to allocate new cube buffers");
                                        let [mut vertex_buffer_stg, mut index_buffer_stg] =
                                            AppSubsystemInstances::get()
                                                .mini_engine
                                                .borrow_mut()
                                                .alloc_upload_buffer_array([
                                                    br::BufferDesc::new(
                                                        core::mem::size_of::<GenericVertex>() * 24,
                                                        br::BufferUsage::TRANSFER_SRC,
                                                    ),
                                                    br::BufferDesc::new(
                                                        core::mem::size_of::<u16>() * 36,
                                                        br::BufferUsage::TRANSFER_SRC,
                                                    ),
                                                ])
                                                .expect("Failed to allocate new cube stg buffers");
                                        let (v, i) = GenericVertex::unit_cube();
                                        vertex_buffer_stg
                                            .guard_map(
                                                peridot_memory_manager::BufferMapMode::Write,
                                                |p| unsafe { p.clone_slice_to(0, &v) },
                                            )
                                            .unwrap();
                                        index_buffer_stg
                                            .guard_map(
                                                peridot_memory_manager::BufferMapMode::Write,
                                                |p| unsafe { p.clone_slice_to(0, &i) },
                                            )
                                            .unwrap();
                                        AppSubsystemInstances::get()
                                            .mini_engine
                                            .borrow_mut()
                                            .submit_transient_commands_and_wait(|rec| {
                                                rec.copy_buffer(
                                                    &vertex_buffer_stg,
                                                    &vertex_buffer,
                                                    &[br::BufferCopy::mirror(
                                                        0,
                                                        (core::mem::size_of::<GenericVertex>() * 24)
                                                            as _,
                                                    )],
                                                )
                                                .copy_buffer(
                                                    &index_buffer_stg,
                                                    &index_buffer,
                                                    &[br::BufferCopy::mirror(
                                                        0,
                                                        (core::mem::size_of::<u16>() * 36) as _,
                                                    )],
                                                )
                                                .pipeline_barrier_2(&br::DependencyInfo::new(
                                                    &[br::MemoryBarrier2::new()
                                                        .from(
                                                            br::PipelineStageFlags2::COPY,
                                                            br::AccessFlags2::TRANSFER.write,
                                                        )
                                                        .to(
                                                            br::PipelineStageFlags2::VERTEX_INPUT,
                                                            br::AccessFlags2::VERTEX_ATTRIBUTE_READ
                                                                | br::AccessFlags2::INDEX_READ,
                                                        )],
                                                    &[],
                                                    &[],
                                                ))
                                            })
                                            .unwrap();

                                        let new_object = ObjectEditState {
                                            id: Uuid::new_v4(),
                                            name: "New Cube".into(),
                                            order: app_state.read().current_scene.next_order(),
                                            is_dirty: true,
                                            details: ObjectDetails::Mesh {
                                                vertex_buffer,
                                                index_buffer: Some(index_buffer),
                                                vertex_count: 36,
                                                position: peridot_math::Vector3(0.0, 0.0, 0.0),
                                                rotation: peridot_math::Quaternion::ONE,
                                                scale: peridot_math::Vector3::ONE,
                                            },
                                        };

                                        app_state.write().current_scene.add_object(new_object);

                                        // refresh list
                                        for v in rows.borrow().iter() {
                                            v.borrow()
                                                .unmount()
                                                .expect("Failed to unmount old element rows");
                                        }

                                        let app_state_borrow = app_state.read();
                                        let mut init_objects = app_state_borrow
                                            .current_scene
                                            .objects
                                            .values()
                                            .collect::<Vec<_>>();
                                        init_objects.sort_by_key(|x| x.order);

                                        *rows.borrow_mut() = init_objects
                                            .into_iter()
                                            .scan(0.0f32, |y, x| {
                                                let p = ObjectTreeElementRowView::new(
                                                    ref_dpi,
                                                    x.name.to_owned(),
                                                    x.id.clone(),
                                                )
                                                .expect("Failed to create row view");
                                                p.borrow_mut()
                                                    .reposition(Vector2 { X: 0.0, Y: *y })
                                                    .expect("Failed to reposition row view");
                                                *y += p.borrow().height();

                                                Some(p)
                                            })
                                            .collect::<Vec<_>>();

                                        let children = mounted_visual_root.Children().unwrap();
                                        for v in rows.borrow().iter() {
                                            v.borrow()
                                                .mount(&children, &mounted_ht)
                                                .expect("Failed to mount new rows");
                                        }
                                    })
                                },
                                true,
                            ),
                            MenuItem::Command(
                                "Plane".into(),
                                {
                                    let app_state = self.app_state.clone();
                                    let rows = self.rows.clone();
                                    let mounted_visual_root =
                                        self.mounted_visual_root.clone().unwrap();
                                    let mounted_ht = self.mounted_ht.clone().unwrap();
                                    let ref_dpi = input_context.current_dpi();

                                    new_shared_mut(move || {
                                        let [vertex_buffer, index_buffer] =
                                            AppSubsystemInstances::get()
                                                .mini_engine
                                                .borrow_mut()
                                                .alloc_device_local_buffer_array([
                                                    br::BufferDesc::new(
                                                        core::mem::size_of::<GenericVertex>() * 4,
                                                        br::BufferUsage::VERTEX_BUFFER
                                                            .transfer_dest(),
                                                    ),
                                                    br::BufferDesc::new(
                                                        core::mem::size_of::<u16>() * 6,
                                                        br::BufferUsage::INDEX_BUFFER
                                                            .transfer_dest(),
                                                    ),
                                                ])
                                                .expect("Failed to allocate new cube buffers");
                                        let [mut vertex_buffer_stg, mut index_buffer_stg] =
                                            AppSubsystemInstances::get()
                                                .mini_engine
                                                .borrow_mut()
                                                .alloc_upload_buffer_array([
                                                    br::BufferDesc::new(
                                                        core::mem::size_of::<GenericVertex>() * 4,
                                                        br::BufferUsage::TRANSFER_SRC,
                                                    ),
                                                    br::BufferDesc::new(
                                                        core::mem::size_of::<u16>() * 6,
                                                        br::BufferUsage::TRANSFER_SRC,
                                                    ),
                                                ])
                                                .expect("Failed to allocate new cube stg buffers");
                                        let (v, i) = GenericVertex::unit_plane();
                                        vertex_buffer_stg
                                            .guard_map(
                                                peridot_memory_manager::BufferMapMode::Write,
                                                |p| unsafe { p.clone_slice_to(0, &v) },
                                            )
                                            .unwrap();
                                        index_buffer_stg
                                            .guard_map(
                                                peridot_memory_manager::BufferMapMode::Write,
                                                |p| unsafe { p.clone_slice_to(0, &i) },
                                            )
                                            .unwrap();
                                        AppSubsystemInstances::get()
                                            .mini_engine
                                            .borrow_mut()
                                            .submit_transient_commands_and_wait(|rec| {
                                                rec.copy_buffer(
                                                    &vertex_buffer_stg,
                                                    &vertex_buffer,
                                                    &[br::BufferCopy::mirror(
                                                        0,
                                                        (core::mem::size_of::<GenericVertex>() * 4)
                                                            as _,
                                                    )],
                                                )
                                                .copy_buffer(
                                                    &index_buffer_stg,
                                                    &index_buffer,
                                                    &[br::BufferCopy::mirror(
                                                        0,
                                                        (core::mem::size_of::<u16>() * 6) as _,
                                                    )],
                                                )
                                                .pipeline_barrier_2(&br::DependencyInfo::new(
                                                    &[br::MemoryBarrier2::new()
                                                        .from(
                                                            br::PipelineStageFlags2::COPY,
                                                            br::AccessFlags2::TRANSFER.write,
                                                        )
                                                        .to(
                                                            br::PipelineStageFlags2::VERTEX_INPUT,
                                                            br::AccessFlags2::VERTEX_ATTRIBUTE_READ
                                                                | br::AccessFlags2::INDEX_READ,
                                                        )],
                                                    &[],
                                                    &[],
                                                ))
                                            })
                                            .unwrap();

                                        let new_object = ObjectEditState {
                                            id: Uuid::new_v4(),
                                            name: "New Plane".into(),
                                            order: app_state.read().current_scene.next_order(),
                                            is_dirty: true,
                                            details: ObjectDetails::Mesh {
                                                vertex_buffer,
                                                index_buffer: Some(index_buffer),
                                                vertex_count: 6,
                                                position: peridot_math::Vector3(0.0, 0.0, 0.0),
                                                rotation: peridot_math::Quaternion::ONE,
                                                scale: peridot_math::Vector3::ONE,
                                            },
                                        };

                                        app_state.write().current_scene.add_object(new_object);

                                        // refresh list
                                        for v in rows.borrow().iter() {
                                            v.borrow()
                                                .unmount()
                                                .expect("Failed to unmount old element rows");
                                        }

                                        let app_state_borrow = app_state.read();
                                        let mut init_objects = app_state_borrow
                                            .current_scene
                                            .objects
                                            .values()
                                            .collect::<Vec<_>>();
                                        init_objects.sort_by_key(|x| x.order);

                                        *rows.borrow_mut() = init_objects
                                            .into_iter()
                                            .scan(0.0f32, |y, x| {
                                                let p = ObjectTreeElementRowView::new(
                                                    ref_dpi,
                                                    x.name.to_owned(),
                                                    x.id.clone(),
                                                )
                                                .expect("Failed to create row view");
                                                p.borrow_mut()
                                                    .reposition(Vector2 { X: 0.0, Y: *y })
                                                    .expect("Failed to reposition row view");
                                                *y += p.borrow().height();

                                                Some(p)
                                            })
                                            .collect::<Vec<_>>();

                                        let children = mounted_visual_root.Children().unwrap();
                                        for v in rows.borrow().iter() {
                                            v.borrow()
                                                .mount(&children, &mounted_ht)
                                                .expect("Failed to mount new rows");
                                        }
                                    })
                                },
                                true,
                            ),
                            MenuItem::Command(
                                "Icosphere".into(),
                                new_shared_mut(|| println!("Create Icosphere")),
                                true,
                            ),
                            MenuItem::Command(
                                "Cylinder".into(),
                                new_shared_mut(|| println!("Create Cylinder")),
                                true,
                            ),
                            MenuItem::Command(
                                "Capsule".into(),
                                new_shared_mut(|| println!("Create Capsule")),
                                true,
                            ),
                            MenuItem::Header("Special".into()),
                            MenuItem::Command(
                                "Terrain".into(),
                                new_shared_mut(|| println!("Create Terrain")),
                                true,
                            ),
                        ],
                    ),
                    MenuItem::Command(
                        "Create Empty at Parent".into(),
                        new_shared_mut(|| println!("Create Empty at Parent")),
                        false,
                    ),
                    MenuItem::Command(
                        "Delete".into(),
                        new_shared_mut(|| println!("Delete Object")),
                        false,
                    ),
                ],
                desktop_x_px,
                desktop_y_px,
                input_context.current_dpi(),
            )
            .expect("Failed to pop context menu");
    }
}
impl PaneTabPresenter for ObjectTreeTabPresenter {
    const INIT_TAB_NAME: &'static str = "Object Tree";

    fn new(
        _tab_header_view: &SharedMut<PaneTabHeaderView>,
        view_ctx: &(impl ViewContext + ?Sized),
        app_state: &MTSharedMut<AppState>,
    ) -> Self {
        let app_state_borrow = app_state.read();
        let mut init_objects = app_state_borrow
            .current_scene
            .objects
            .values()
            .collect::<Vec<_>>();
        init_objects.sort_by_key(|x| x.order);

        let rows = init_objects
            .into_iter()
            .scan(0.0f32, |y, x| {
                let p = ObjectTreeElementRowView::new(
                    view_ctx.current_dpi(),
                    x.name.to_owned(),
                    x.id.clone(),
                )
                .expect("Failed to create row view");
                p.borrow_mut()
                    .reposition(Vector2 { X: 0.0, Y: *y })
                    .expect("Failed to reposition row view");
                *y += p.borrow().height();

                Some(p)
            })
            .collect::<Vec<_>>();

        Self {
            rows: new_shared_mut(rows),
            app_state: app_state.clone(),
            mounted_visual_root: None,
            mounted_ht: None,
        }
    }
}

pub struct AssetExplorerTabPresenter {}
impl PaneTabContentPresenter for AssetExplorerTabPresenter {
    fn build_content_view(
        &mut self,
        _onto: &ContainerVisual,
        _onto_ht: &HitTestTree,
        _view_context: &dyn ViewContext,
        _app_state: &MTSharedMut<AppState>,
    ) -> windows::core::Result<()> {
        Ok(())
    }

    fn on_hide_content_view(
        &mut self,
        _view_context: &dyn ViewContext,
        _app_state: &MTSharedMut<AppState>,
    ) -> windows::core::Result<()> {
        Ok(())
    }
}
impl PaneTabPresenter for AssetExplorerTabPresenter {
    const INIT_TAB_NAME: &'static str = "Asset Explorer";

    fn new(
        _tab_header_view: &SharedMut<PaneTabHeaderView>,
        _view_ctx: &(impl ViewContext + ?Sized),
        _app_state: &MTSharedMut<AppState>,
    ) -> Self {
        Self {}
    }
}

pub trait AppStateCurrentSelectionChangedHandler {
    fn on_changed(&self, app_state: &MTSharedMut<AppState>, view_context: &dyn ViewContext);
}
#[repr(transparent)]
pub struct AppStateCurrentSelectionChangedHandlerEntry(
    pub AtomicWeak<dyn AppStateCurrentSelectionChangedHandler + Sync + Send>,
);
impl PartialEq for AppStateCurrentSelectionChangedHandlerEntry {
    fn eq(&self, other: &Self) -> bool {
        AtomicWeak::ptr_eq(&self.0, &other.0)
    }
}
impl Eq for AppStateCurrentSelectionChangedHandlerEntry {}
impl Hash for AppStateCurrentSelectionChangedHandlerEntry {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.as_ptr().hash(state)
    }
}

pub struct AppState {
    pub current_scene: SceneEditState,
    pub current_selection_object_id: Option<Uuid>,
    pub current_selection_changed_handlers: HashSet<AppStateCurrentSelectionChangedHandlerEntry>,
}
impl AppState {
    pub fn new() -> Self {
        Self {
            current_scene: SceneEditState::new(),
            current_selection_object_id: None,
            current_selection_changed_handlers: HashSet::new(),
        }
    }

    pub fn set_current_selection(
        this: &MTSharedMut<Self>,
        selection: Option<Uuid>,
        view_context: &impl ViewContext,
    ) {
        this.write().current_selection_object_id = selection;

        let callbacks = this
            .read()
            .current_selection_changed_handlers
            .iter()
            .filter_map(|x| x.0.upgrade())
            .collect::<Vec<_>>();
        for h in callbacks {
            h.on_changed(this, view_context);
        }
    }

    pub fn observe_current_selection_changes(
        this: &MTSharedMut<Self>,
        handler: &Arc<impl AppStateCurrentSelectionChangedHandler + 'static + Sync + Send>,
        view_ctx: &dyn ViewContext,
    ) {
        let wh = Arc::downgrade(handler);
        this.write()
            .current_selection_changed_handlers
            .insert(AppStateCurrentSelectionChangedHandlerEntry(wh));

        handler.on_changed(this, view_ctx);
    }

    pub fn unobserve_current_selection_changes(
        &mut self,
        handler: &AtomicWeak<impl AppStateCurrentSelectionChangedHandler + 'static + Sync + Send>,
    ) {
        self.current_selection_changed_handlers
            .remove(unsafe { core::mem::transmute(handler) });
    }
}

pub struct SceneEditState {
    pub objects: HashMap<Uuid, ObjectEditState>,
    pub is_dirty: bool,
}
impl SceneEditState {
    pub fn new() -> Self {
        Self {
            objects: HashMap::new(),
            is_dirty: false,
        }
    }

    pub fn add_object(&mut self, mut state: ObjectEditState) {
        // 次のレンダリングサイクルでデータ載せてほしいのでdirtyフラグを立てておく
        state.is_dirty = true;
        self.is_dirty = true;
        self.objects.insert(state.id.clone(), state);
    }

    pub fn next_order(&self) -> u32 {
        self.objects.values().map(|x| x.order).max().unwrap_or(0) + 1
    }
}

pub struct ObjectEditState {
    pub id: Uuid,
    pub name: String,
    pub order: u32,
    pub is_dirty: bool,
    pub details: ObjectDetails,
}
impl ObjectEditState {
    pub fn is_sunlight_object(&self) -> bool {
        matches!(self.details, ObjectDetails::SunLight { .. })
    }

    pub fn update_sunlight_intensity(&mut self, new_intensity: f32) {
        let ObjectDetails::SunLight {
            ref mut intensity, ..
        } = self.details
        else {
            return;
        };

        *intensity = new_intensity;
        self.is_dirty = true;
    }

    pub fn update_sunlight_rotation(&mut self, new_rotation: peridot_math::QuaternionF32) {
        let ObjectDetails::SunLight {
            ref mut rotation, ..
        } = self.details
        else {
            return;
        };

        *rotation = new_rotation;
        self.is_dirty = true;
    }
}

pub enum ObjectDetails {
    Camera {},
    SunLight {
        rotation: peridot_math::QuaternionF32,
        intensity: f32,
    },
    Mesh {
        vertex_buffer: peridot_memory_manager::Buffer,
        index_buffer: Option<peridot_memory_manager::Buffer>,
        vertex_count: u32,
        position: peridot_math::Vector3F32,
        rotation: peridot_math::QuaternionF32,
        scale: peridot_math::Vector3F32,
    },
}

pub trait System {}

pub struct World {
    entities: Vec<Entity>,
    free_entity_slots: BTreeSet<u32>,
    components: HashMap<core::any::TypeId, ComponentSparseSet>,
    systems: Vec<Box<dyn System>>,
}
impl World {
    pub fn new() -> Self {
        Self {
            entities: Vec::new(),
            free_entity_slots: BTreeSet::new(),
            components: HashMap::new(),
            systems: Vec::new(),
        }
    }

    pub fn new_entity(&mut self) -> EntityID {
        match self.free_entity_slots.pop_first() {
            Some(x) => {
                self.entities[x as usize].id.version += 1;
                let new_entity_id = self.entities[x as usize].id;

                new_entity_id
            }
            None => {
                let new_entity_id = EntityID {
                    slot_index: self.entities.len() as _,
                    version: 0,
                };
                self.entities.push(Entity {
                    id: new_entity_id,
                    name: String::from("New Entity"),
                });

                new_entity_id
            }
        }
    }

    pub fn remove_entity(&mut self, id: EntityID) {
        self.free_entity_slots.insert(id.slot_index);
    }

    pub fn is_entity_dead(&self, id: EntityID) -> bool {
        self.free_entity_slots.contains(&id.slot_index)
            || self.entities[id.slot_index as usize].id.version != id.version
    }

    pub fn add_component<T: 'static>(&mut self, id: EntityID, component: T) {
        self.components
            .entry(core::any::TypeId::of::<T>())
            .or_insert_with(ComponentSparseSet::new::<T>)
            .set(id, component);
    }

    pub fn remove_component<T: 'static>(&mut self, id: EntityID) -> Option<T> {
        let Some(components) = self.components.get_mut(&core::any::TypeId::of::<T>()) else {
            return None;
        };

        components.clear::<T>(id)
    }

    pub fn get_component<T: 'static>(&self, id: EntityID) -> Option<&T> {
        self.components
            .get(&core::any::TypeId::of::<T>())
            .and_then(|cs| cs.get(id))
    }

    pub fn get_component_mut<T: 'static>(&mut self, id: EntityID) -> Option<&mut T> {
        self.components
            .get_mut(&core::any::TypeId::of::<T>())
            .and_then(|cs| cs.get_mut(id))
    }

    pub fn add_system(&mut self, sys: impl System + 'static) {
        self.systems.push(Box::new(sys));
    }
}

pub struct Entity {
    pub id: EntityID,
    pub name: String,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct EntityID {
    pub slot_index: u32,
    pub version: u32,
}

pub struct AnyVec {
    head_ptr: *mut u8,
    count: usize,
    capacity: usize,
    element_size: usize,
    element_alignment: usize,
    drop_fn: fn(*mut u8),
    init_type_id: core::any::TypeId,
}
impl AnyVec {
    const INIT_CAPACITY: usize = 8;

    pub fn new<T: 'static>() -> Self {
        let dense_array_layout = core::alloc::Layout::from_size_align(
            core::mem::size_of::<T>() * Self::INIT_CAPACITY,
            core::mem::align_of::<T>(),
        )
        .expect("invalid memory layout");
        let dense_array = unsafe { std::alloc::alloc(dense_array_layout) };

        Self {
            head_ptr: dense_array,
            count: 0,
            capacity: Self::INIT_CAPACITY,
            element_size: core::mem::size_of::<T>(),
            element_alignment: core::mem::align_of::<T>(),
            drop_fn: |ptr| unsafe { core::ptr::drop_in_place(ptr as *mut T) },
            init_type_id: core::any::TypeId::of::<T>(),
        }
    }

    fn extend_capacity(&mut self, new_capacity: usize) {
        let new_head_ptr = unsafe {
            std::alloc::realloc(
                self.head_ptr,
                core::alloc::Layout::from_size_align_unchecked(
                    self.element_size * self.capacity,
                    self.element_alignment,
                ),
                self.element_size * new_capacity,
            )
        };

        self.head_ptr = new_head_ptr;
        self.capacity = new_capacity;
    }

    pub fn push<T: 'static>(&mut self, component: T) {
        assert_eq!(core::any::TypeId::of::<T>(), self.init_type_id);

        if self.capacity <= self.count {
            self.extend_capacity(self.capacity * 2);
        }

        unsafe {
            core::ptr::write((self.head_ptr as *mut T).add(self.count), component);
        }
        self.count += 1;
    }

    pub fn pop<T: 'static>(&mut self) -> Option<T> {
        assert_eq!(core::any::TypeId::of::<T>(), self.init_type_id);

        if self.count <= 0 {
            return None;
        }

        let r = unsafe { core::ptr::read((self.head_ptr as *mut T).add(self.count - 1)) };
        self.count -= 1;
        Some(r)
    }

    pub fn swap_remove<T: 'static>(&mut self, at: usize) -> T {
        let last_index = self.count - 1;
        self.as_typed_slice_mut::<T>().swap(at, last_index);
        self.count -= 1;
        unsafe { core::ptr::read((self.head_ptr as *mut T).add(self.count)) }
    }

    pub fn as_typed_slice<T: 'static>(&self) -> &[T] {
        assert_eq!(core::any::TypeId::of::<T>(), self.init_type_id);

        unsafe { self.as_typed_slice_unchecked::<T>() }
    }

    #[inline(always)]
    pub const unsafe fn as_typed_slice_unchecked<T>(&self) -> &[T] {
        core::slice::from_raw_parts(self.head_ptr as *mut T, self.count)
    }

    pub fn as_typed_slice_mut<T: 'static>(&mut self) -> &mut [T] {
        assert_eq!(core::any::TypeId::of::<T>(), self.init_type_id);

        unsafe { self.as_typed_slice_mut_unchecked::<T>() }
    }

    #[inline(always)]
    pub unsafe fn as_typed_slice_mut_unchecked<T>(&mut self) -> &mut [T] {
        core::slice::from_raw_parts_mut(self.head_ptr as *mut T, self.count)
    }

    #[inline(always)]
    pub const fn len(&self) -> usize {
        self.count
    }
}
impl Drop for AnyVec {
    fn drop(&mut self) {
        for x in 0..self.count {
            (self.drop_fn)(unsafe { self.head_ptr.add(self.element_size * x) });
        }

        unsafe {
            std::alloc::dealloc(
                self.head_ptr,
                core::alloc::Layout::from_size_align_unchecked(
                    self.element_size * self.capacity,
                    self.element_alignment,
                ),
            )
        }
    }
}

pub struct ComponentSparseSet {
    sparse_array_index_to_dense: Vec<Option<usize>>,
    dense_array: AnyVec,
    dense_array_to_slot_index: Vec<usize>,
}
impl ComponentSparseSet {
    pub fn new<T: 'static>() -> Self {
        Self {
            sparse_array_index_to_dense: Vec::new(),
            dense_array: AnyVec::new::<T>(),
            dense_array_to_slot_index: Vec::new(),
        }
    }

    pub fn set<T: 'static>(&mut self, entity: EntityID, component: T) {
        self.dense_array.push(component);
        let component_index = self.dense_array.len() - 1;

        if self.sparse_array_index_to_dense.len() <= entity.slot_index as usize {
            self.sparse_array_index_to_dense
                .resize(entity.slot_index as usize + 1, None);
        }
        self.sparse_array_index_to_dense[entity.slot_index as usize] = Some(component_index);
        self.dense_array_to_slot_index
            .push(entity.slot_index as usize);
    }

    pub fn clear<T: 'static>(&mut self, entity: EntityID) -> Option<T> {
        let Some(component_index) =
            self.sparse_array_index_to_dense[entity.slot_index as usize].take()
        else {
            return None;
        };

        let c = self.dense_array.swap_remove::<T>(component_index);
        self.dense_array_to_slot_index.swap_remove(component_index);
        if !self.dense_array_to_slot_index.is_empty() {
            let swapped_slot_index = self.dense_array_to_slot_index[component_index];
            self.sparse_array_index_to_dense[swapped_slot_index] = Some(component_index);
        }

        Some(c)
    }

    pub fn get<T: 'static>(&self, entity: EntityID) -> Option<&T> {
        self.sparse_array_index_to_dense
            .get(entity.slot_index as usize)
            .copied()
            .flatten()
            .and_then(|x| self.dense_array.as_typed_slice::<T>().get(x))
    }

    pub fn get_mut<T: 'static>(&mut self, entity: EntityID) -> Option<&mut T> {
        self.sparse_array_index_to_dense
            .get(entity.slot_index as usize)
            .copied()
            .flatten()
            .and_then(|x| self.dense_array.as_typed_slice_mut::<T>().get_mut(x))
    }

    pub fn iter_objects<'s, T: 'static>(&'s self) -> impl Iterator<Item = &'s T> + 's {
        self.dense_array.as_typed_slice::<T>().iter()
    }
}

struct AppWindowState {
    input_state: InputState,
    pane_group_docking_manager: SharedMut<PaneGroupDockingManager>,
    app_title_bar_view: SharedMut<AppTitleBarView>,
    currently_maximized: bool,
    current_dpi: f32,
    app_state: MTSharedMut<AppState>,
}
impl ViewContext for AppWindowState {
    fn current_dpi(&self) -> f32 {
        self.current_dpi
    }
}
impl InputContext for AppWindowState {
    fn make_resize_context(&self) -> ResizeContext {
        ResizeContext {
            current_dpi: self.current_dpi,
        }
    }

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

    let mut state = AppState::new();
    let obj = ObjectEditState {
        id: Uuid::new_v4(),
        name: "Camera".into(),
        order: 0,
        is_dirty: false,
        details: ObjectDetails::Camera {},
    };
    state.current_scene.objects.insert(obj.id.clone(), obj);
    let obj = ObjectEditState {
        id: Uuid::new_v4(),
        name: "Sun Light".into(),
        order: 1,
        is_dirty: false,
        details: ObjectDetails::SunLight {
            rotation: peridot_math::QuaternionF32::ONE,
            intensity: 20.0,
        },
    };
    state.current_scene.objects.insert(obj.id.clone(), obj);
    let state = new_mt_shared_mut(state);

    let _dispatcher_queue_controller = unsafe {
        CreateDispatcherQueueController(DispatcherQueueOptions {
            dwSize: core::mem::size_of::<DispatcherQueueOptions>() as _,
            threadType: DQTYPE_THREAD_CURRENT,
            apartmentType: DQTAT_COM_ASTA,
        })
        .expect("Failed to create dispatcher queue controller")
    };

    let app_subsystem_instances_finalizer = AppSubsystemInstances::initialize();
    let app_global_signals_finalizer = AppGlobalSignals::initialize();
    let app_subsystem_instances = AppSubsystemInstances::get();
    let cm_finalizer = ContextMenu::initialize();

    let desktop_interop = app_subsystem_instances
        .compositor
        .cast::<ICompositorDesktopInterop>()
        .expect("This compositor does not support desktop interop");
    let desktop_window_target = unsafe {
        desktop_interop
            .CreateDesktopWindowTarget(window_handle.handle, false)
            .expect("Failed to create desktop window compositor target")
    };

    let app_global_scale = window_handle.current_dpi as f64 / 96.0;
    println!("global scale: {app_global_scale}");
    let composition_root = app_subsystem_instances
        .compositor
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

    let bg = app_subsystem_instances
        .compositor
        .CreateSpriteVisual()
        .expect("Failed to create bg");
    bg.SetBrush(&{
        let b = app_subsystem_instances
            .compositor
            .CreateLinearGradientBrush()
            .expect("Failed to create bg brush");
        let color_stops = b.ColorStops().expect("Failed to get color stop collection");
        color_stops
            .Append(
                &app_subsystem_instances
                    .compositor
                    .CreateColorGradientStopWithOffsetAndColor(
                        0.0,
                        Color {
                            A: 16,
                            R: 128,
                            G: 32,
                            B: 24,
                        },
                    )
                    .expect("Failed to create color stop"),
            )
            .expect("Failed to append color stop");
        color_stops
            .Append(
                &app_subsystem_instances
                    .compositor
                    .CreateColorGradientStopWithOffsetAndColor(
                        1.0,
                        Color {
                            A: 72,
                            R: 24,
                            G: 64,
                            B: 128,
                        },
                    )
                    .expect("Failed to create color stop"),
            )
            .expect("Failed to append color stop");
        b.SetStartPoint(Vector2 { X: 0.0, Y: 0.0 })
            .expect("Failed to set start point");
        b.SetEndPoint(Vector2 { X: 1.0, Y: 1.0 })
            .expect("Failed to set end point");

        b
    })
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
        CompositionDebugSettings::TryGetSettings(&app_subsystem_instances.compositor)
            .expect("Failed to get settings");

    let overlay_layer = app_subsystem_instances
        .compositor
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

    let hittest_tree_root = HitTestTree::new_unsized(Some(()), 0.0, 0.0);

    let mut view_context = ViewContext1 {
        current_dpi: window_handle.current_dpi,
    };

    let pane_group_docking_manager = new_shared_mut(
        PaneGroupDockingManager::new(&hittest_tree_root)
            .expect("Failed to initialize docking manager"),
    );

    let pane_group1 = TabGroupPaneView::new(&pane_group_docking_manager)
        .expect("Failed to create TabGroupPaneView");
    TabGroupPaneView::add_tab::<TimelineTabPresenter>(&pane_group1, &mut view_context, &state)
        .expect("Failed to create SceneViewPaneTabHeader");
    pane_group1.borrow_mut().rearrange(&ResizeContext {
        current_dpi: window_handle.current_dpi,
    });

    let main_pane = TabGroupPaneView::new(&pane_group_docking_manager)
        .expect("Failed to create TabGroupPaneView");
    TabGroupPaneView::add_tab::<StageTabPresenter>(&main_pane, &mut view_context, &state)
        .expect("Failed to create StagePaneTab");
    TabGroupPaneView::add_tab::<PreviewTabPresenter>(&main_pane, &mut view_context, &state)
        .expect("Failed to create PreviewPaneTab");
    TabGroupPaneView::add_tab::<ProjectSettingsTabPresenter>(&main_pane, &mut view_context, &state)
        .expect("Failed to create ProjectSettingsPaneTabHeader");
    main_pane.borrow_mut().rearrange(&ResizeContext {
        current_dpi: window_handle.current_dpi,
    });

    let pane_group3 = TabGroupPaneView::new(&pane_group_docking_manager)
        .expect("Failed to create TabGroupPaneView");
    TabGroupPaneView::add_tab::<InspectorTabPresenter>(&pane_group3, &mut view_context, &state)
        .expect("Failed to create InspectorPaneTabHeader");
    pane_group3.borrow_mut().rearrange(&ResizeContext {
        current_dpi: window_handle.current_dpi,
    });
    pane_group3
        .borrow_mut()
        .resize(
            256.0,
            256.0,
            &ResizeContext {
                current_dpi: window_handle.current_dpi,
            },
        )
        .expect("Failed to resize pane");

    let explorers_pane = TabGroupPaneView::new(&pane_group_docking_manager)
        .expect("Failed to create TabGroupPaneView");
    TabGroupPaneView::add_tab::<AssetExplorerTabPresenter>(
        &explorers_pane,
        &mut view_context,
        &state,
    )
    .expect("Failed to create AssetExplorerTab");
    explorers_pane.borrow_mut().rearrange(&ResizeContext {
        current_dpi: window_handle.current_dpi,
    });
    explorers_pane
        .borrow_mut()
        .resize(
            256.0,
            256.0,
            &ResizeContext {
                current_dpi: window_handle.current_dpi,
            },
        )
        .expect("Failed to resize pane");

    let scene_subinfo_pane = TabGroupPaneView::new(&pane_group_docking_manager)
        .expect("Failed to create TabGroupPaneView");
    TabGroupPaneView::add_tab::<ObjectTreeTabPresenter>(
        &scene_subinfo_pane,
        &mut view_context,
        &state,
    )
    .expect("Failed to create ObjectTreeTab");
    scene_subinfo_pane.borrow_mut().rearrange(&ResizeContext {
        current_dpi: window_handle.current_dpi,
    });
    scene_subinfo_pane
        .borrow_mut()
        .resize(
            256.0,
            256.0,
            &ResizeContext {
                current_dpi: window_handle.current_dpi,
            },
        )
        .expect("Failed to resize pane");

    let layout = PaneDockLayer::new_root(|parent| {
        Some(
            PaneDockLayer::new_on(
                DockDirection::Right,
                parent,
                |parent, _| PaneDockLayer::new_filled(&pane_group3, parent),
                |parent, ctx| {
                    PaneDockLayer::new_on(
                        DockDirection::Top,
                        parent,
                        |parent, _| PaneDockLayer::new_filled(&pane_group1, parent),
                        |parent, ctx| {
                            PaneDockLayer::new_on(
                                DockDirection::Bottom,
                                parent,
                                |parent, _| PaneDockLayer::new_filled(&explorers_pane, parent),
                                |parent, ctx| {
                                    PaneDockLayer::new_on(
                                        DockDirection::Left,
                                        parent,
                                        |parent, _| {
                                            PaneDockLayer::new_filled(&scene_subinfo_pane, parent)
                                        },
                                        |parent, _| PaneDockLayer::new_filled(&main_pane, parent),
                                        ctx,
                                    )
                                    .expect("Failed to create pane dock layer")
                                },
                                ctx,
                            )
                            .expect("Failed to create pane dock layer")
                        },
                        ctx,
                    )
                    .expect("Failed to create pane dock state")
                },
                &mut view_context,
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
        .resize_root(
            client_width,
            client_height - AppTitleBarView::HEIGHT,
            &ResizeContext {
                current_dpi: window_handle.current_dpi,
            },
        )
        .expect("Failed to initial relayout");

    let app_title = AppTitleBarView::new(&view_context, app_global_scale)
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
        pane_group_docking_manager,
        app_title_bar_view: app_title,
        currently_maximized: window_handle
            .is_maximized()
            .expect("Failed to query maximized state"),
        current_dpi: window_handle.current_dpi,
        app_state: state,
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
        let r = AppGlobalSignals::get().wait();
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
            SignalEventType::Receiver(handler, arg) => handler.on_signal(arg, &ws),
            SignalEventType::Unknown => (),
        }
    }

    window_handle.clear_state_store();
    drop(cm_finalizer);
    drop(app_global_signals_finalizer);
    drop(app_subsystem_instances_finalizer);

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
        let (x, y) = (
            app_window.pixels_to_dip(x as _),
            app_window.pixels_to_dip(y as _),
        );
        let actions = state.input_state.on_mouse_move(x, y);
        for a in actions {
            a.execute(x, y, state, hwnd);
        }

        let mut tme = TRACKMOUSEEVENT {
            cbSize: core::mem::size_of::<TRACKMOUSEEVENT>() as _,
            dwFlags: TME_LEAVE
                | if msg == WM_NCMOUSEMOVE {
                    TME_NONCLIENT
                } else {
                    TRACKMOUSEEVENT_FLAGS(0)
                },
            hwndTrack: hwnd,
            dwHoverTime: HOVER_DEFAULT,
        };
        unsafe {
            TrackMouseEvent(&mut tme).expect("Failed to track mouse event");
        }

        return LRESULT(0);
    }
    if msg == WM_LBUTTONDOWN {
        let app_window = AppWindow::wrap(hwnd);
        let Some(state) = app_window.get_state_store() else {
            return LRESULT(0);
        };

        let (x, y) = ((lp.0 & 0xffff) as i16, ((lp.0 >> 16) & 0xffff) as i16);
        let (x, y) = (
            app_window.pixels_to_dip(x as _),
            app_window.pixels_to_dip(y as _),
        );
        let actions = state.input_state.on_mouse_down(x, y);
        for a in actions {
            a.execute(x, y, state, hwnd);
        }

        return LRESULT(0);
    }
    if msg == WM_LBUTTONUP {
        let app_window = AppWindow::wrap(hwnd);
        let Some(state) = app_window.get_state_store() else {
            return LRESULT(0);
        };

        let (x, y) = ((lp.0 & 0xffff) as i16, ((lp.0 >> 16) & 0xffff) as i16);
        let (x, y) = (
            app_window.pixels_to_dip(x as _),
            app_window.pixels_to_dip(y as _),
        );
        let actions = state.input_state.on_mouse_up(x, y);
        for a in actions {
            a.execute(x, y, state, hwnd);
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
            .resize_root(w, h - AppTitleBarView::HEIGHT, &state.make_resize_context())
            .expect("Failed to resize root");

        let maximized = app_window
            .is_maximized()
            .expect("Failed to query maximized state");
        if maximized != state.currently_maximized {
            // split borrowing
            let title_bar_view = state.app_title_bar_view.clone();

            title_bar_view
                .borrow()
                .change_maximize_restore_icon(maximized)
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
    if msg == WM_RBUTTONUP {
        let app_window = AppWindow::wrap(hwnd);
        let Some(state) = app_window.get_state_store() else {
            return LRESULT(0);
        };

        let (x, y) = ((lp.0 & 0xffff) as i16, ((lp.0 >> 16) & 0xffff) as i16);
        let actions = state.input_state.on_mouse_right_up(
            app_window.pixels_to_dip(x as _),
            app_window.pixels_to_dip(y as _),
        );
        for a in actions {
            a.execute(x as _, y as _, state, hwnd);
        }

        return LRESULT(0);
    }
    if msg == WM_MOUSELEAVE || msg == WM_NCMOUSELEAVE {
        let app_window = AppWindow::wrap(hwnd);
        let Some(state) = app_window.get_state_store() else {
            // not initialized
            return unsafe { DefWindowProcA(hwnd, msg, wp, lp) };
        };

        let actions = state.input_state.on_mouse_leave();
        for a in actions {
            a.execute(0.0, 0.0, state, hwnd);
        }

        return LRESULT(0);
    }

    if msg == WM_MOUSEWHEEL {
        let app_window = AppWindow::wrap(hwnd);
        let Some(state) = app_window.get_state_store() else {
            // not initialized
            return unsafe { DefWindowProcA(hwnd, msg, wp, lp) };
        };

        let mut p = [POINT {
            x: (lp.0 & 0xffff) as i16 as _,
            y: ((lp.0 >> 16) & 0xffff) as i16 as _,
        }];
        app_window.map_points_from_desktop(&mut p);
        let (cx, cy) = (
            app_window.pixels_to_dip(p[0].x as _),
            app_window.pixels_to_dip(p[0].y as _),
        );

        let actions = state.input_state.on_mouse_wheel_roll(
            cx,
            cy,
            ((wp.0 >> 16) & 0xffff) as i16 as f32 / WHEEL_DELTA as f32,
        );
        for a in actions {
            a.execute(cx, cy, state, hwnd);
        }

        return LRESULT(0);
    }
    if msg == WM_MBUTTONDOWN {
        let app_window = AppWindow::wrap(hwnd);
        let Some(state) = app_window.get_state_store() else {
            // not initialized
            return unsafe { DefWindowProcA(hwnd, msg, wp, lp) };
        };

        let (cx, cy) = (
            app_window.pixels_to_dip((lp.0 & 0xffff) as i16 as _),
            app_window.pixels_to_dip(((lp.0 >> 16) & 0xffff) as i16 as _),
        );

        let actions = state.input_state.on_mouse_wheel_down(cx, cy);
        for a in actions {
            a.execute(cx, cy, state, hwnd);
        }

        return LRESULT(0);
    }
    if msg == WM_MBUTTONUP {
        let app_window = AppWindow::wrap(hwnd);
        let Some(state) = app_window.get_state_store() else {
            // not initialized
            return unsafe { DefWindowProcA(hwnd, msg, wp, lp) };
        };

        let (cx, cy) = (
            app_window.pixels_to_dip((lp.0 & 0xffff) as i16 as _),
            app_window.pixels_to_dip(((lp.0 >> 16) & 0xffff) as i16 as _),
        );

        let actions = state.input_state.on_mouse_wheel_up(cx, cy);
        for a in actions {
            a.execute(cx, cy, state, hwnd);
        }

        return LRESULT(0);
    }

    if msg == WM_KILLFOCUS {
        ContextMenu::get_mut()
            .hide_all()
            .expect("Failed to hide context menu");
    }

    unsafe { DefWindowProcA(hwnd, msg, wp, lp) }
}
