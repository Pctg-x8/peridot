use std::{
    borrow::Cow,
    cell::RefCell,
    collections::{BTreeSet, HashMap, HashSet},
    hash::Hash,
    rc::{Rc, Weak},
    sync::{atomic::AtomicBool, Arc, Weak as AtomicWeak},
};

use app_global_signals::{AppGlobalSignals, SignalEventReceiver, SignalEventType};
use app_subsystem_instances::AppSubsystemInstances;
use bedrock::{self as br, Image, ImageChild, RenderPass, VkHandle, VkObject};
use br::{
    CommandBuffer, CommandPool, DescriptorPool, Device, GraphicsPipelineBuilder,
    ImageSubresourceSlice, MemoryBound, PipelineShaderStageProvider, Queue, SubmissionBatch,
};
use components::{FloatSliderView, LabelView, RollableNumberView};
use features::{
    AppTitleBarView, ContextMenu, DockingPanePreview, MenuItem, PaneSplitterView, SplitDirection,
};
use miniengine::{
    ColoredVertex, DescriptorBinding, GenericVertex, Mat4, PrimaryDirectionalLightUniformData,
    RenderPassDescription, SamplerDesc, SkyboxPrecomputedTextures, SkyboxRenderer, StdVkDevice,
    SubpassDescription, TempRT, UtilityVertices, Vec4,
};
use observable::ObservationDisconnector;
use parking_lot::RwLock;
use peridot_math::{Camera, One, ProjectionMethod, Zero};
use uikit::{
    HitTestTree, InputContext, InputEventHandler, InputState, MountableView, ResizeContext,
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
                    DXGI_ALPHA_MODE_IGNORE, DXGI_ALPHA_MODE_PREMULTIPLIED,
                    DXGI_COLOR_SPACE_RGB_FULL_G22_NONE_P709, DXGI_FORMAT_R8G8B8A8_UNORM,
                    DXGI_SAMPLE_DESC,
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
            Desktop::DesktopWindowTarget, Diagnostics::CompositionDebugSettings, LayerVisual,
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

const TAB_MARGIN_X: f32 = 16.0;
const TAB_MARGIN_Y: f32 = 6.0;
const TAB_RADIUS: f32 = 4.0;
const TAB_ACTIVE_LIT_COLOR: Color = Color {
    A: 255,
    R: 96,
    G: 255,
    B: 204,
};
const TAB_ACTIVE_BASE_COLOR: Color = Color {
    A: 255,
    R: 32,
    G: 128,
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
    active_overlay_visual: SpriteVisual,
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
            Y: text_height + TAB_MARGIN_Y * 2.0,
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
        base.SetSize(Vector2 {
            X: title_text.width + TAB_MARGIN_X * 2.0,
            Y: title_text.height + TAB_MARGIN_Y * 2.0,
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
            let v = AppSubsystemInstances::get()
                .compositor
                .CreateSpriteVisual()?;
            v.set_properties()
                .brush(
                    &AppSubsystemInstances::get()
                        .ui_common_objects
                        .tab_active_overlay_brush,
                )?
                .expand_to_parent()?;

            v
        };

        bg.SetOpacity(0.0)?;
        active_overlay.SetOpacity(if init_active { 1.0 } else { 0.0 })?;

        let children = base.Children()?;
        children.InsertAtBottom(&active_overlay)?;
        children.InsertAtBottom(&bg)?;

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

        if self.is_active {
            // アクティブ状態のときは背景はアクティブにできない
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
impl MountableView for PaneTabHeaderView {
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
    current_mounted_views: RwLock<Vec<SharedMut<dyn MountableView>>>,
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
                                FloatSliderView::new(view_context, intensity, 200000.0).unwrap();
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
                                            }
                                            state_mut
                                                .current_scene
                                                .mark_dirty_recursive(bound_object_id);
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
                                            }
                                            state_mut
                                                .current_scene
                                                .mark_dirty_recursive(bound_object_id);
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
                                            }
                                            state_mut
                                                .current_scene
                                                .mark_dirty_recursive(bound_object_id);
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
                                            }
                                            state_mut
                                                .current_scene
                                                .mark_dirty_recursive(bound_object_id);
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
                                            }
                                            state_mut
                                                .current_scene
                                                .mark_dirty_recursive(bound_object_id);
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
                                            }
                                            state_mut
                                                .current_scene
                                                .mark_dirty_recursive(bound_object_id);
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
                                        }
                                        state_mut
                                            .current_scene
                                            .mark_dirty_recursive(bound_object_id);
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
                                        }
                                        state_mut
                                            .current_scene
                                            .mark_dirty_recursive(bound_object_id);
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
                                        }
                                        state_mut
                                            .current_scene
                                            .mark_dirty_recursive(bound_object_id);
                                    }
                                }),
                            ));
                        }
                        ObjectDetails::Camera {} => {
                            let label =
                                LabelView::new("Luminance Histogram:", view_context).unwrap();
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

                            let luminance_histogram_view = LuminanceHistogramView::new(Rect {
                                X: 12.0,
                                Y: 80.0,
                                Width: 256.0,
                                Height: 64.0,
                            })
                            .unwrap();
                            self.current_mounted_views
                                .write()
                                .push(new_shared_mut(luminance_histogram_view));
                        }
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

pub struct LuminanceHistogramViewFrame {
    ready_event: HANDLE,
    presentation_buffer: IPresentationBuffer,
    presentation_tex: ID3D11Texture2D,
    staging_tex_mutex: IDXGIKeyedMutex,
    staging_tex: ID3D11Texture2D,
    _vk_imported_memory: br::DeviceMemoryObject<StdVkDevice>,
    framebuffer: br::FramebufferObject<'static, StdVkDevice>,
    command_buffer: br::CommandBufferObject<StdVkDevice>,
}

pub struct LuminanceHistogramViewRenderer {
    presentation_manager: IPresentationManager,
    presentation_surface: IPresentationSurface,
    main_command_pool: RefCell<br::CommandPoolObject<StdVkDevice>>,
    graphics_queue: SharedMut<br::QueueObject<StdVkDevice>>,
    d3d11_device_context: ID3D11DeviceContext,
    frames: Vec<LuminanceHistogramViewFrame>,
}
impl SignalEventReceiver for LuminanceHistogramViewRenderer {
    fn on_signal(&self, arg: usize, _view_ctx: &dyn ViewContext) {
        let f = &self.frames[arg];

        unsafe {
            f.staging_tex_mutex
                .AcquireSync(0, INFINITE)
                .expect("Failed to acquire keyed mutex");
        }

        self.graphics_queue
            .borrow_mut()
            .submit(
                &[br::EmptySubmissionBatch.with_command_buffers(&[f.command_buffer])],
                None::<&mut br::FenceObject<StdVkDevice>>,
            )
            .expect("Failed to send command");
        self.graphics_queue
            .borrow_mut()
            .wait()
            .expect("Failed to wait work");

        unsafe {
            f.staging_tex_mutex
                .ReleaseSync(1)
                .expect("Failed to release keyed mutex");
        }

        unsafe {
            f.staging_tex_mutex
                .AcquireSync(1, INFINITE)
                .expect("Failed to acquire keyed mutex");
        }
        unsafe {
            // Note: rtそのままでは表示できないらしい（Composition SwapchainでKeyedMutexいじれたらワンチャンありそうな気がする）
            self.d3d11_device_context
                .CopyResource(&f.presentation_tex, &f.staging_tex);
        }
        unsafe {
            f.staging_tex_mutex
                .ReleaseSync(0)
                .expect("Failed to release keyed mutex");
        }

        unsafe {
            self.presentation_surface
                .SetBuffer(&f.presentation_buffer)
                .expect("Failed to set new buffer");
        }
        unsafe {
            self.presentation_manager
                .Present()
                .expect("Failed to queue present");
        }
    }
}

pub struct LuminanceHistogramViewRenderResources {
    main_render_pass: Rc<br::RenderPassObject<StdVkDevice>>,
    main_pipeline_layout: br::PipelineLayoutObject<StdVkDevice>,
    main_pipeline: br::PipelineObject<StdVkDevice>,
    _dp: br::DescriptorPoolObject<StdVkDevice>,
    main_descriptor_set: br::DescriptorSet,
}
impl LuminanceHistogramViewRenderResources {
    pub fn new(
        e: &mut MiniEngine,
        histogram_buffer: &peridot_memory_manager::Buffer,
        postfx_global_work_buffer: &peridot_memory_manager::Buffer,
    ) -> br::Result<Self> {
        let main_render_pass = e.render_pass(RenderPassDescription {
            attachments: vec![
                br::AttachmentDescription2::new(br::vk::VK_FORMAT_R8G8B8A8_UNORM)
                    .with_layout_to(br::ImageLayout::General.from_undefined())
                    .color_memory_op(br::LoadOp::Clear, br::StoreOp::Store),
            ],
            subpasses: vec![SubpassDescription {
                color_outputs: vec![br::AttachmentReference2::color_attachment_opt(0)],
                ..SubpassDescription::EMPTY
            }],
            dependencies: vec![br::SubpassDependency2::new(
                br::SubpassIndex::Internal(0),
                br::SubpassIndex::External,
            )
            .of_memory(
                br::AccessFlags::COLOR_ATTACHMENT.write,
                br::AccessFlags::MEMORY.read,
            )
            .of_execution(
                br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                br::PipelineStageFlags(0),
            )],
        })?;

        let main_vsh = e.shader("shaders/full_blit_pixel_snap.vspv")?;
        let main_fsh = e.shader("shaders/posteffects/autoexposure/histogram_graph.fspv")?;
        let main_dsl = e.descriptor_set_layout(vec![
            DescriptorBinding::UniformBuffer(1, br::ShaderStage::FRAGMENT),
            DescriptorBinding::StorageBuffer(1, br::ShaderStage::FRAGMENT),
        ])?;
        let main_pipeline_layout =
            br::PipelineLayoutBuilder::new(vec![&main_dsl], vec![(br::ShaderStage::VERTEX, 0..8)])
                .create(e.device().clone())?;
        let mut main_pipeline = br::NonDerivedGraphicsPipelineBuilder::new(
            &main_pipeline_layout,
            main_render_pass.subpass(0),
            br::VertexProcessingStages::new(
                br::VertexShaderStage::new(br::PipelineShader2::new(&main_vsh, c"main"))
                    .with_fragment_shader_stage(br::PipelineShader2::new(&main_fsh, c"main")),
                &[],
                &[],
                br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP,
            ),
        );
        main_pipeline
            .multisample_state(Some(br::MultisampleState::new()))
            .dynamic_viewport_scissors(1)
            .add_attachment_blend(br::AttachmentColorBlendState::premultiplied());
        let main_pipeline = e.create_graphics_pipeline(main_pipeline)?;

        let mut dp = br::DescriptorPoolBuilder::new(1)
            .with_reservations(vec![
                br::DescriptorType::UniformBuffer.with_count(1),
                br::DescriptorType::StorageBuffer.with_count(1),
            ])
            .create(e.device().clone())?;
        let [main_descriptor_set] =
            dp.alloc_array(&[br::DescriptorSetLayoutObjectRef::new(&main_dsl)])?;

        e.device().update_descriptor_sets(
            &[
                main_descriptor_set
                    .binding_at(0)
                    .write(br::DescriptorContents::uniform_buffer(
                        postfx_global_work_buffer,
                        0..core::mem::size_of::<PostEffectGlobalWorkBuffer>() as u64,
                    )),
                main_descriptor_set
                    .binding_at(1)
                    .write(br::DescriptorContents::storage_buffer(
                        histogram_buffer,
                        0..core::mem::size_of::<[u32; 256]>() as u64,
                    )),
            ],
            &[],
        );

        Ok(Self {
            main_render_pass,
            main_pipeline_layout,
            main_pipeline,
            _dp: dp,
            main_descriptor_set,
        })
    }

    pub fn populate_commands<
        'r,
        CB: br::VkHandleMut<Handle = br::vk::VkCommandBuffer> + ?Sized,
        Device: br::Device + ?Sized,
    >(
        &self,
        rec: br::CmdRecord<'r, CB, Device>,
        fb: &(impl br::Framebuffer + ?Sized),
        fb_region: br::vk::VkRect2D,
    ) -> br::CmdRecord<'r, CB, Device> {
        rec.begin_render_pass(
            &self.main_render_pass,
            fb,
            fb_region.clone(),
            &[br::ClearValue::color_f32([0.0; 4])],
            true,
        )
        .set_viewport(0, &[fb_region.make_viewport(0.0..1.0)])
        .set_scissor(0, &[fb_region])
        .bind_graphics_pipeline_pair(&self.main_pipeline, &self.main_pipeline_layout)
        .bind_graphics_descriptor_sets(0, &[self.main_descriptor_set.0], &[])
        .push_graphics_constant(
            br::ShaderStage::VERTEX,
            0,
            &[
                1.0 / fb_region.extent.width as f32,
                1.0 / fb_region.extent.height as f32,
            ],
        )
        .draw(4, 1, 0, 0)
        .end_render_pass()
    }
}

pub struct LuminanceHistogramView {
    root: SpriteVisual,
    _buffer_refs: Option<(
        Rc<peridot_memory_manager::Buffer>,
        Rc<peridot_memory_manager::Buffer>,
    )>,
    render_resources: Option<LuminanceHistogramViewRenderResources>,
    renderer: Option<Rc<LuminanceHistogramViewRenderer>>,
}
impl LuminanceHistogramView {
    pub fn new(init_rect: Rect) -> windows::core::Result<Self> {
        let root = AppSubsystemInstances::get()
            .compositor
            .CreateSpriteVisual()?;
        root.set_properties().rect(&init_rect)?;

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
                right: init_rect.Width as _,
                bottom: init_rect.Height as _,
            })?;

            presentation_surface.SetAlphaMode(DXGI_ALPHA_MODE_PREMULTIPLIED)?;
            // TODO: G10(Linear色空間のはず)を使うとなんか挙動が怪しいのでいったんG22(Gamma補正バージョン)を使う
            // presentation_surface
            //     .SetColorSpace(DXGI_COLOR_SPACE_RGB_FULL_G10_NONE_P709)?;
            presentation_surface.SetColorSpace(DXGI_COLOR_SPACE_RGB_FULL_G22_NONE_P709)?;
        }

        root.SetBrush(
            &AppSubsystemInstances::get()
                .compositor
                .CreateSurfaceBrushWithSurface(&surface)?,
        )?;

        let (render_resources, renderer, buffer_refs);
        if let (Some(postfx_global_work_buffer), Some(histogram_buffer)) = (
            AppGlobalSharedInstances::get()
                .editor_window_postfx_global_work_buffer
                .as_ref()
                .and_then(|x| x.upgrade()),
            AppGlobalSharedInstances::get()
                .editor_window_histogram_buffer
                .as_ref()
                .and_then(|x| x.upgrade()),
        ) {
            let res = LuminanceHistogramViewRenderResources::new(
                &mut AppSubsystemInstances::get().mini_engine.borrow_mut(),
                &histogram_buffer,
                &postfx_global_work_buffer,
            )
            .unwrap();

            let mut main_command_pool = AppSubsystemInstances::get()
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
                .unwrap();
            let mut command_buffers = main_command_pool
                .alloc(BACK_BUFFER_COUNT as _, true)
                .unwrap();
            let mut frames = Vec::with_capacity(BACK_BUFFER_COUNT);
            for n in 0..BACK_BUFFER_COUNT {
                let texture_desc =
                    d3d11_presentation_texture_desc(init_rect.Width as _, init_rect.Height as _);
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
                let mut imported_image = br::ImageDesc::new(
                    br::vk::VkExtent2D {
                        width: init_rect.Width as _,
                        height: init_rect.Height as _,
                    },
                    br::vk::VK_FORMAT_R8G8B8A8_UNORM,
                )
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

                let vk_framebuffer = br::FramebufferBuilder::new(&res.main_render_pass)
                    .with_attachment(
                        imported_image
                            .clone()
                            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                            .view_builder()
                            .create()
                            .expect("Failed to create image view"),
                    )
                    .create()
                    .expect("Failed to create framebuffer");

                res.populate_commands(
                    unsafe {
                        command_buffers[n]
                            .begin(AppSubsystemInstances::get().mini_engine.borrow().device())
                            .unwrap()
                    },
                    &vk_framebuffer,
                    br::vk::VkExtent2D {
                        width: init_rect.Width as _,
                        height: init_rect.Height as _,
                    }
                    .into_rect(br::vk::VkOffset2D::ZERO),
                )
                .end()
                .unwrap();

                let rt_mutex = rt.cast::<IDXGIKeyedMutex>()?;
                frames.push(LuminanceHistogramViewFrame {
                    ready_event: eh,
                    presentation_buffer,
                    presentation_tex: texture,
                    staging_tex_mutex: rt_mutex,
                    staging_tex: rt,
                    _vk_imported_memory: imported_image_memory,
                    framebuffer: vk_framebuffer,
                    command_buffer: command_buffers[n],
                });
            }

            buffer_refs = Some((postfx_global_work_buffer, histogram_buffer));
            render_resources = Some(res);
            renderer = Some(Rc::new(LuminanceHistogramViewRenderer {
                presentation_manager: AppSubsystemInstances::get().presentation_manager.clone(),
                presentation_surface,
                main_command_pool: RefCell::new(main_command_pool),
                graphics_queue: AppSubsystemInstances::get()
                    .mini_engine
                    .borrow()
                    .graphics_queue()
                    .clone(),
                d3d11_device_context: unsafe {
                    AppSubsystemInstances::get()
                        .d3d11_device
                        .GetImmediateContext()?
                },
                frames,
            }));
        } else {
            buffer_refs = None;
            render_resources = None;
            renderer = None;
        }

        Ok(Self {
            root,
            _buffer_refs: buffer_refs,
            render_resources,
            renderer,
        })
    }
}
impl MountableView for LuminanceHistogramView {
    fn mount(&self, onto: &VisualCollection, _onto_ht: &HitTestTree) -> windows::core::Result<()> {
        onto.InsertAtTop(&self.root)?;

        if let Some(ref r) = self.renderer {
            for (n, f) in r.frames.iter().enumerate() {
                AppGlobalSignals::get_mut().register(f.ready_event, r, n);
            }
        }

        Ok(())
    }

    fn unmount(&self) -> windows::core::Result<()> {
        self.root.Parent()?.Children()?.Remove(&self.root)?;

        if let Some(ref r) = self.renderer {
            for n in 0..r.frames.len() {
                AppGlobalSignals::get_mut().unregister(r, n);
            }
        }

        Ok(())
    }
}

pub struct MonitorTabPresenter {
    mounting_histogram_view: Option<LuminanceHistogramView>,
}
impl PaneTabContentPresenter for MonitorTabPresenter {
    fn build_content_view(
        &mut self,
        onto: &ContainerVisual,
        onto_ht: &HitTestTree,
        _view_context: &dyn ViewContext,
        _app_state: &MTSharedMut<AppState>,
    ) -> windows::core::Result<()> {
        let mounting_histogram_view = LuminanceHistogramView::new(onto_ht.rect())?;
        mounting_histogram_view.mount(&onto.Children()?, onto_ht)?;
        self.mounting_histogram_view = Some(mounting_histogram_view);

        Ok(())
    }

    fn on_hide_content_view(
        &mut self,
        _view_context: &dyn ViewContext,
        _app_state: &MTSharedMut<AppState>,
    ) -> windows::core::Result<()> {
        if let Some(v) = self.mounting_histogram_view.take() {
            v.unmount()?;
        }

        Ok(())
    }
}
impl PaneTabPresenter for MonitorTabPresenter {
    const INIT_TAB_NAME: &'static str = "Monitor";

    fn new(
        _tab_header_view: &SharedMut<PaneTabHeaderView>,
        _view_ctx: &(impl ViewContext + ?Sized),
        _app_state: &MTSharedMut<AppState>,
    ) -> Self {
        Self {
            mounting_histogram_view: None,
        }
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

fn compute_world_trs(scene: &SceneEditState, obj: &ObjectEditState) -> peridot_math::Matrix4F32 {
    let local_mat = match obj.details {
        ObjectDetails::Mesh {
            position,
            rotation,
            scale,
            ..
        } => peridot_math::Matrix4::trs(position, rotation, scale),
        _ => peridot_math::Matrix4::ONE,
    };

    match obj.parent_id {
        Some(pid) => compute_world_trs(scene, &scene.objects[&pid]) * local_mat,
        None => local_mat,
    }
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
            let mut stw = st.write();
            command_buffer_dirty = core::mem::replace(&mut stw.current_scene.is_dirty, false);

            let mut trs_update_object_ids = Vec::new();
            for o in stw
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
                    ObjectDetails::Mesh { .. } => {
                        trs_update_object_ids.push(o.id);
                    }
                    ObjectDetails::Camera { .. } => (),
                }
            }

            for id in trs_update_object_ids {
                let allocation_changed = self
                    .render_resources
                    .borrow_mut()
                    .per_object_uniform_data
                    .set_trs(
                        &id,
                        compute_world_trs(&stw.current_scene, &stw.current_scene.objects[&id]),
                    )
                    .expect("Failed to update object trs data");

                command_buffer_dirty = command_buffer_dirty || allocation_changed;
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

        // let h = self
        //     .render_resources
        //     .borrow()
        //     .postfx_auto_exposure
        //     .readback_histogram();
        // let st = self
        //     .render_resources
        //     .borrow()
        //     .readback_postfx_global_work_buffer();
        // println!(
        //     "histogram: {h:?} {} {} {}",
        //     st.exposure_base_lum, st.average_ev100, st.histogram_max_value
        // );

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

#[macro_export]
macro_rules! ArrayBuilderOp {
    ([try] $($base: tt).+, { $($vname: ident <- $arg: expr),* $(,)? }) => {
        let [$($vname),*] = $($base).+([$($arg),*])?;
    };
    ([ref, try] $($base: tt).+, { $($vname: ident <- $arg: expr),* $(,)? }) => {
        let [$($vname),*] = $($base).+(&[$($arg),*])?;
    }
}

#[repr(C)]
pub struct RenderCameraUniformData {
    pub camera_view_projection_matrix: peridot_math::Matrix4F32,
    pub camera_inverse_view_matrix: peridot_math::Matrix4F32,
    pub camera_persp_fov_rad: f32,
    pub camera_aspect_wh: f32,
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

#[repr(C)]
#[derive(Clone)]
pub struct PostEffectGlobalWorkBuffer {
    pub exposure_base_lum: f32,
    pub histogram_max_value: u32,
    pub average_ev100: f32,
}

const HDR_COLOR_FORMAT: br::vk::VkFormat = br::vk::VK_FORMAT_R16G16B16A16_SFLOAT;

pub struct HDRBloomPostEffect {
    hdr_color_only_pass: Rc<br::RenderPassObject<StdVkDevice>>,
    hdr_color_overwrite_pass: Rc<br::RenderPassObject<StdVkDevice>>,
    downsample_rts: Vec<TempRT>,
    upsample_rts: Vec<TempRT>,
    extract_pipeline_layout: br::PipelineLayoutObject<StdVkDevice>,
    extract_pipeline: br::PipelineObject<StdVkDevice>,
    downsample_pipeline_layout: br::PipelineLayoutObject<StdVkDevice>,
    downsample_pipeline: br::PipelineObject<StdVkDevice>,
    upsample_pipeline_layout: br::PipelineLayoutObject<StdVkDevice>,
    upsample_pipeline: br::PipelineObject<StdVkDevice>,
    merge_pipeline_layout: br::PipelineLayoutObject<StdVkDevice>,
    merge_pipeline: br::PipelineObject<StdVkDevice>,
    _dp: br::DescriptorPoolObject<StdVkDevice>,
    extract_input_descriptor_set: br::DescriptorSet,
    downsample_input_descriptor_sets: Vec<br::DescriptorSet>,
    upsample_input_descriptor_sets: Vec<br::DescriptorSet>,
    merge_input_descriptor_set: br::DescriptorSet,
    downsample_framebuffers: Vec<br::FramebufferObject<'static, StdVkDevice>>,
    upsample_framebuffers: Vec<br::FramebufferObject<'static, StdVkDevice>>,
}
impl HDRBloomPostEffect {
    pub fn new(
        e: &mut MiniEngine,
        init_tex_size: br::vk::VkExtent2D,
        iteration_count: usize,
    ) -> br::Result<Self> {
        let hdr_color_only_pass = e.render_pass(RenderPassDescription {
            attachments: vec![br::AttachmentDescription2::new(HDR_COLOR_FORMAT)
                .with_layout_to(br::ImageLayout::ShaderReadOnlyOpt.from_undefined())
                .color_memory_op(br::LoadOp::DontCare, br::StoreOp::Store)],
            subpasses: vec![SubpassDescription {
                color_outputs: vec![br::AttachmentReference2::color_attachment_opt(0)],
                ..SubpassDescription::EMPTY
            }],
            dependencies: vec![br::SubpassDependency2::new(
                br::SubpassIndex::Internal(0),
                br::SubpassIndex::External,
            )
            .of_memory(
                br::AccessFlags::COLOR_ATTACHMENT.write,
                br::AccessFlags::SHADER.read,
            )
            .of_execution(
                br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                br::PipelineStageFlags::FRAGMENT_SHADER,
            )],
        })?;
        let hdr_color_overwrite_pass = e.render_pass(RenderPassDescription {
            attachments: vec![br::AttachmentDescription2::new(HDR_COLOR_FORMAT)
                .layout(br::ImageLayout::ShaderReadOnlyOpt)
                .color_memory_op(br::LoadOp::Load, br::StoreOp::Store)],
            subpasses: vec![SubpassDescription {
                color_outputs: vec![br::AttachmentReference2::color_attachment_opt(0)],
                ..SubpassDescription::EMPTY
            }],
            dependencies: vec![br::SubpassDependency2::new(
                br::SubpassIndex::Internal(0),
                br::SubpassIndex::External,
            )
            .by_region()
            .of_memory(
                br::AccessFlags::COLOR_ATTACHMENT.write,
                br::AccessFlags::SHADER.read,
            )
            .of_execution(
                br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                br::PipelineStageFlags::FRAGMENT_SHADER,
            )],
        })?;

        let tex_sampler = e.sampler(SamplerDesc::LINEAR_CLAMP_TO_EDGE)?;
        let dsl_src_input =
            e.descriptor_set_layout(vec![DescriptorBinding::CombinedImageSampler(
                br::ShaderStage::FRAGMENT,
                vec![tex_sampler.native_ptr()],
            )])?;
        let dsl_extract_input = e.descriptor_set_layout(vec![
            DescriptorBinding::CombinedImageSampler(
                br::ShaderStage::FRAGMENT,
                vec![tex_sampler.native_ptr()],
            ),
            DescriptorBinding::UniformBuffer(1, br::ShaderStage::FRAGMENT),
        ])?;

        let extract_vsh = e.shader("shaders/full_blit.vspv")?;
        let extract_fsh = e.shader("shaders/posteffects/bloom/extract.fspv")?;
        let extract_pipeline_layout = br::PipelineLayoutBuilder::new(
            vec![&dsl_extract_input],
            vec![(br::ShaderStage::FRAGMENT, 0..16)],
        )
        .create(e.device().clone())?;
        let mut extract_pipeline = br::NonDerivedGraphicsPipelineBuilder::new(
            &extract_pipeline_layout,
            hdr_color_only_pass.subpass(0),
            br::VertexProcessingStages::new(
                br::VertexShaderStage::new(br::PipelineShader2::new(&extract_vsh, c"main"))
                    .with_fragment_shader_stage(br::PipelineShader2::new(&extract_fsh, c"main")),
                &[],
                &[],
                br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP,
            ),
        );
        extract_pipeline
            .multisample_state(Some(br::MultisampleState::new()))
            .add_attachment_blend(br::AttachmentColorBlendState::noblend())
            .dynamic_viewport_scissors(1);

        let filter_sh = e.shader("shaders/posteffects/bloom/filter.spv")?;
        let downsample_pipeline_layout = br::PipelineLayoutBuilder::new(
            vec![&dsl_src_input],
            vec![(br::ShaderStage::FRAGMENT, 0..8)],
        )
        .create(e.device().clone())?;
        let mut downsample_pipeline = br::NonDerivedGraphicsPipelineBuilder::new(
            &downsample_pipeline_layout,
            hdr_color_only_pass.subpass(0),
            br::VertexProcessingStages::new(
                br::VertexShaderStage::new(br::PipelineShader2::new(&filter_sh, c"vertMain"))
                    .with_fragment_shader_stage(br::PipelineShader2::new(
                        &filter_sh,
                        c"fragDownSample",
                    )),
                &[],
                &[],
                br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP,
            ),
        );
        downsample_pipeline
            .multisample_state(Some(br::MultisampleState::new()))
            .add_attachment_blend(br::AttachmentColorBlendState::noblend())
            .dynamic_viewport_scissors(1);

        let upsample_pipeline_layout = br::PipelineLayoutBuilder::new(
            vec![&dsl_src_input, &dsl_src_input],
            vec![(br::ShaderStage::FRAGMENT, 0..8)],
        )
        .create(e.device().clone())?;
        let mut upsample_pipeline = br::NonDerivedGraphicsPipelineBuilder::new(
            &upsample_pipeline_layout,
            hdr_color_only_pass.subpass(0),
            br::VertexProcessingStages::new(
                br::VertexShaderStage::new(br::PipelineShader2::new(&filter_sh, c"vertMain"))
                    .with_fragment_shader_stage(br::PipelineShader2::new(
                        &filter_sh,
                        c"fragUpSample",
                    )),
                &[],
                &[],
                br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP,
            ),
        );
        upsample_pipeline
            .multisample_state(Some(br::MultisampleState::new()))
            .add_attachment_blend(br::AttachmentColorBlendState::noblend())
            .dynamic_viewport_scissors(1);

        let mut additive_blending = br::AttachmentColorBlendState::noblend();
        additive_blending
            .enable()
            .color_blend(br::BlendFactor::One, br::BlendOp::Add, br::BlendFactor::One)
            .alpha_blend(br::BlendFactor::One, br::BlendOp::Add, br::BlendFactor::One);
        let merge_vsh = e.shader("shaders/full_blit_uvst.vspv")?;
        let merge_fsh = e.shader("shaders/posteffects/bloom/merge.fspv")?;
        let merge_pipeline_layout = br::PipelineLayoutBuilder::new(
            vec![&dsl_src_input],
            vec![
                (br::ShaderStage::VERTEX, 0..16),
                (br::ShaderStage::FRAGMENT, 16..20),
            ],
        )
        .create(e.device().clone())?;
        let mut merge_pipeline = br::NonDerivedGraphicsPipelineBuilder::new(
            &merge_pipeline_layout,
            hdr_color_overwrite_pass.subpass(0),
            br::VertexProcessingStages::new(
                br::VertexShaderStage::new(br::PipelineShader2::new(&merge_vsh, c"main"))
                    .with_fragment_shader_stage(br::PipelineShader2::new(&merge_fsh, c"main")),
                &[],
                &[],
                br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP,
            ),
        );
        merge_pipeline
            .multisample_state(Some(br::MultisampleState::new()))
            .add_attachment_blend(additive_blending)
            .dynamic_viewport_scissors(1);

        let extract_pipeline_extras = extract_pipeline.make_extras();
        let downsample_pipeline_extras = downsample_pipeline.make_extras();
        let upsample_pipeline_extras = upsample_pipeline.make_extras();
        let merge_pipeline_extras = merge_pipeline.make_extras();
        ArrayBuilderOp!([ref, try] e.create_graphics_pipeline_array, {
            extract_pipeline <- extract_pipeline.build(&extract_pipeline_extras),
            downsample_pipeline <- downsample_pipeline.build(&downsample_pipeline_extras),
            upsample_pipeline <- upsample_pipeline.build(&upsample_pipeline_extras),
            merge_pipeline <- merge_pipeline.build(&merge_pipeline_extras)
        });

        let downsample_rts = (0..=iteration_count)
            .scan(1.0f32, |scale, _| {
                let rt = TempRT::new(
                    e,
                    br::ImageDesc::new(
                        br::vk::VkExtent2D {
                            width: (init_tex_size.width as f32 * *scale) as _,
                            height: (init_tex_size.height as f32 * *scale) as _,
                        },
                        HDR_COLOR_FORMAT,
                    )
                    .as_color_attachment()
                    .sampled(),
                    br::AspectMask::COLOR,
                    0..1,
                    0..1,
                )
                .unwrap();
                *scale *= 0.5;
                Some(rt)
            })
            .collect::<Vec<_>>();
        let upsample_rts = (0..iteration_count)
            .scan(1.0f32, |scale, _| {
                let rt = TempRT::new(
                    e,
                    br::ImageDesc::new(
                        br::vk::VkExtent2D {
                            width: (init_tex_size.width as f32 * *scale) as _,
                            height: (init_tex_size.height as f32 * *scale) as _,
                        },
                        HDR_COLOR_FORMAT,
                    )
                    .as_color_attachment()
                    .sampled(),
                    br::AspectMask::COLOR,
                    0..1,
                    0..1,
                )
                .unwrap();
                *scale *= 0.5;
                Some(rt)
            })
            .collect::<Vec<_>>();

        let mut dp =
            br::DescriptorPoolBuilder::new((2 + iteration_count + 1 + iteration_count) as _)
                .with_reservations(vec![
                    br::DescriptorType::CombinedImageSampler
                        .with_count((2 + iteration_count + 1 + iteration_count) as _),
                    br::DescriptorType::UniformBuffer.with_count(1),
                ])
                .create(e.device().clone())?;
        ArrayBuilderOp!([ref, try] dp.alloc_array, {
            extract_input_descriptor_set <- br::DescriptorSetLayoutObjectRef::new(&dsl_extract_input),
            merge_input_descriptor_set <- br::DescriptorSetLayoutObjectRef::new(&dsl_src_input)
        });
        let downsample_input_descriptor_sets = dp.alloc(
            &core::iter::repeat(&dsl_src_input)
                .take(iteration_count + 1)
                .collect::<Vec<_>>(),
        )?;
        let upsample_input_descriptor_sets = dp.alloc(
            &core::iter::repeat(&dsl_src_input)
                .take(iteration_count)
                .collect::<Vec<_>>(),
        )?;

        let downsample_framebuffers = downsample_rts
            .iter()
            .map(|rt| {
                br::FramebufferBuilder::new_with_attachment(
                    &hdr_color_only_pass,
                    rt.resource.clone(),
                )
                .create()
            })
            .collect::<br::Result<Vec<_>>>()?;
        let upsample_framebuffers = upsample_rts
            .iter()
            .map(|rt| {
                br::FramebufferBuilder::new_with_attachment(
                    &hdr_color_only_pass,
                    rt.resource.clone(),
                )
                .create()
            })
            .collect::<br::Result<Vec<_>>>()?;

        Ok(Self {
            hdr_color_only_pass,
            hdr_color_overwrite_pass,
            downsample_rts,
            upsample_rts,
            extract_pipeline_layout,
            extract_pipeline,
            downsample_pipeline_layout,
            downsample_pipeline,
            upsample_pipeline_layout,
            upsample_pipeline,
            merge_pipeline_layout,
            merge_pipeline,
            _dp: dp,
            extract_input_descriptor_set,
            downsample_input_descriptor_sets,
            upsample_input_descriptor_sets,
            merge_input_descriptor_set,
            downsample_framebuffers,
            upsample_framebuffers,
        })
    }

    pub fn update_descriptor_sets<'s>(
        &'s self,
        writes: &mut Vec<br::DescriptorSetWriteInfo<'s>>,
        extract_source: &'s (impl br::VkHandle<Handle = br::vk::VkImageView> + ?Sized),
        postfx_global_work_buffer: &'s (impl br::VkHandle<Handle = br::vk::VkBuffer> + ?Sized),
    ) {
        let postfx_global_work_exposure_base_lum_offset =
            core::mem::offset_of!(PostEffectGlobalWorkBuffer, exposure_base_lum)
                as br::vk::VkDeviceSize;

        writes.extend([
            self.extract_input_descriptor_set.binding_at(0).write(
                br::DescriptorContents::combined_image_sampler(
                    extract_source,
                    br::ImageLayout::ShaderReadOnlyOpt,
                ),
            ),
            self.extract_input_descriptor_set.binding_at(1).write(
                br::DescriptorContents::uniform_buffer(
                    postfx_global_work_buffer,
                    postfx_global_work_exposure_base_lum_offset
                        ..postfx_global_work_exposure_base_lum_offset
                            + core::mem::size_of::<f32>() as u64,
                ),
            ),
            self.merge_input_descriptor_set.binding_at(0).write(
                br::DescriptorContents::combined_image_sampler(
                    &self.upsample_rts[0].resource,
                    br::ImageLayout::ShaderReadOnlyOpt,
                ),
            ),
        ]);
        writes.extend(
            self.downsample_input_descriptor_sets
                .iter()
                .zip(self.downsample_rts.iter())
                .map(|(s, r)| {
                    s.binding_at(0)
                        .write(br::DescriptorContents::combined_image_sampler(
                            &r.resource,
                            br::ImageLayout::ShaderReadOnlyOpt,
                        ))
                }),
        );
        writes.extend(
            self.upsample_input_descriptor_sets
                .iter()
                .zip(self.upsample_rts.iter())
                .map(|(s, r)| {
                    s.binding_at(0)
                        .write(br::DescriptorContents::combined_image_sampler(
                            &r.resource,
                            br::ImageLayout::ShaderReadOnlyOpt,
                        ))
                }),
        );
    }

    pub fn resize(&mut self, new_size: br::vk::VkExtent2D) -> br::Result<()> {
        let mut scale = 1.0f32;
        for x in self.downsample_rts.iter_mut() {
            x.recreate_newsize(br::vk::VkExtent2D {
                width: (new_size.width as f32 * scale) as _,
                height: (new_size.height as f32 * scale) as _,
            })?;

            scale *= 0.5;
        }
        scale = 1.0f32;
        for x in self.upsample_rts.iter_mut() {
            x.recreate_newsize(br::vk::VkExtent2D {
                width: (new_size.width as f32 * scale) as _,
                height: (new_size.height as f32 * scale) as _,
            })?;

            scale *= 0.5;
        }

        self.downsample_framebuffers = self
            .downsample_rts
            .iter()
            .map(|rt| {
                br::FramebufferBuilder::new_with_attachment(
                    &self.hdr_color_only_pass,
                    rt.resource.clone(),
                )
                .create()
            })
            .collect::<br::Result<Vec<_>>>()?;
        self.upsample_framebuffers = self
            .upsample_rts
            .iter()
            .map(|rt| {
                br::FramebufferBuilder::new_with_attachment(
                    &self.hdr_color_only_pass,
                    rt.resource.clone(),
                )
                .create()
            })
            .collect::<br::Result<Vec<_>>>()?;

        Ok(())
    }

    pub fn populate_commands<
        'r,
        CB: br::VkHandleMut<Handle = br::vk::VkCommandBuffer> + ?Sized,
        Device: br::Device + ?Sized,
    >(
        &self,
        rec: br::CmdRecord<'r, CB, Device>,
        merge_framebuffer: &(impl br::Framebuffer + ?Sized),
        merge_framebuffer_region: br::vk::VkRect2D,
    ) -> br::CmdRecord<'r, CB, Device> {
        let base_tex_size = *self.downsample_rts[0].resource.size().as_2d_ref();

        rec.begin_render_pass(
            &self.hdr_color_only_pass,
            &self.downsample_framebuffers[0],
            base_tex_size.into_rect(br::vk::VkOffset2D::ZERO),
            &[br::ClearValue::color_f32([0.0, 0.0, 0.0, 0.0])],
            true,
        )
        .set_viewport(
            0,
            &[base_tex_size
                .into_rect(br::vk::VkOffset2D::ZERO)
                .make_viewport(0.0..1.0)],
        )
        .set_scissor(0, &[base_tex_size.into_rect(br::vk::VkOffset2D::ZERO)])
        .bind_graphics_pipeline_pair(&self.extract_pipeline, &self.extract_pipeline_layout)
        .bind_graphics_descriptor_sets(0, &[self.extract_input_descriptor_set.0], &[])
        .push_graphics_constant(br::ShaderStage::FRAGMENT, 0, &[0.0f32, 100.0, 12.5, 0.65])
        .draw(4, 1, 0, 0)
        .end_render_pass()
        .inject(|r| {
            self.downsample_framebuffers
                .iter()
                .skip(1)
                .zip(self.downsample_input_descriptor_sets.iter())
                .fold((r, 0.5f32), |(r, scale), (target, src)| {
                    let rect = br::vk::VkExtent2D {
                        width: (base_tex_size.width as f32 * scale) as _,
                        height: (base_tex_size.height as f32 * scale) as _,
                    }
                    .into_rect(br::vk::VkOffset2D::ZERO);
                    let viewport = rect.make_viewport(0.0..1.0);
                    let upper_texel_size = [
                        2.0 / (base_tex_size.width as f32 * 2.0 * scale).trunc(),
                        2.0 / (base_tex_size.height as f32 * 2.0 * scale).trunc(),
                    ];

                    let r = r
                        .begin_render_pass(
                            &self.hdr_color_only_pass,
                            &target,
                            rect.clone(),
                            &[br::ClearValue::color_f32([0.0; 4])],
                            true,
                        )
                        .set_viewport(0, &[viewport])
                        .set_scissor(0, &[rect])
                        .bind_graphics_pipeline_pair(
                            &self.downsample_pipeline,
                            &self.downsample_pipeline_layout,
                        )
                        .bind_graphics_descriptor_sets(0, &[src.0], &[])
                        .push_graphics_constant(br::ShaderStage::FRAGMENT, 0, &upper_texel_size)
                        .draw(4, 1, 0, 0)
                        .end_render_pass();
                    (r, scale * 0.5)
                })
                .0
        })
        .inject(|r| {
            let rect = self.upsample_rts[self.upsample_rts.len() - 1]
                .resource
                .size()
                .as_2d_ref()
                .clone()
                .into_rect(br::vk::VkOffset2D::ZERO);
            let viewport = rect.make_viewport(0.0..1.0);
            let lower_texel_size = [
                2.0 / rect.extent.width as f32,
                2.0 / rect.extent.height as f32,
            ];

            r.begin_render_pass(
                &self.hdr_color_only_pass,
                &self.upsample_framebuffers[self.upsample_framebuffers.len() - 1],
                rect.clone(),
                &[br::ClearValue::color_f32([0.0; 4])],
                true,
            )
            .set_viewport(0, &[viewport])
            .set_scissor(0, &[rect])
            .bind_graphics_pipeline_pair(&self.upsample_pipeline, &self.upsample_pipeline_layout)
            .bind_graphics_descriptor_sets(
                0,
                &[
                    self.downsample_input_descriptor_sets
                        [self.downsample_input_descriptor_sets.len() - 1]
                        .0,
                    self.downsample_input_descriptor_sets
                        [self.downsample_input_descriptor_sets.len() - 2]
                        .0,
                ],
                &[],
            )
            .push_graphics_constant(br::ShaderStage::FRAGMENT, 0, &lower_texel_size)
            .draw(4, 1, 0, 0)
            .end_render_pass()
        })
        .inject(|r| {
            let parameters = self
                .upsample_framebuffers
                .iter()
                .zip(
                    self.upsample_input_descriptor_sets
                        .iter()
                        .skip(1)
                        .zip(self.downsample_input_descriptor_sets.iter()),
                )
                .scan(1.0f32, |scale, (target, (src1, src2))| {
                    let rect = br::vk::VkExtent2D {
                        width: (base_tex_size.width as f32 * *scale) as _,
                        height: (base_tex_size.height as f32 * *scale) as _,
                    }
                    .into_rect(br::vk::VkOffset2D::ZERO);
                    let viewport = rect.make_viewport(0.0..1.0);
                    let lower_texel_size = [
                        2.0 / (base_tex_size.width as f32 * *scale * 0.5).trunc(),
                        2.0 / (base_tex_size.height as f32 * *scale * 0.5).trunc(),
                    ];

                    *scale *= 0.5;
                    Some((rect, viewport, lower_texel_size, target, src1, src2))
                })
                .collect::<Vec<_>>();

            parameters.into_iter().rev().fold(
                r,
                |r, (rect, viewport, lower_texel_size, target, src1, src2)| {
                    r.begin_render_pass(
                        &self.hdr_color_only_pass,
                        target,
                        rect.clone(),
                        &[br::ClearValue::color_f32([0.0; 4])],
                        true,
                    )
                    .set_viewport(0, &[viewport])
                    .set_scissor(0, &[rect])
                    .bind_graphics_pipeline_pair(
                        &self.upsample_pipeline,
                        &self.upsample_pipeline_layout,
                    )
                    .bind_graphics_descriptor_sets(0, &[src1.0, src2.0], &[])
                    .push_graphics_constant(br::ShaderStage::FRAGMENT, 0, &lower_texel_size)
                    .draw(4, 1, 0, 0)
                    .end_render_pass()
                },
            )
        })
        .begin_render_pass(
            &self.hdr_color_overwrite_pass,
            merge_framebuffer,
            merge_framebuffer_region,
            &[],
            true,
        )
        .set_viewport(0, &[merge_framebuffer_region.make_viewport(0.0..1.0)])
        .set_scissor(0, &[merge_framebuffer_region])
        .bind_graphics_pipeline_pair(&self.merge_pipeline, &self.merge_pipeline_layout)
        .bind_graphics_descriptor_sets(0, &[self.merge_input_descriptor_set.0], &[])
        .push_graphics_constant(br::ShaderStage::VERTEX, 0, &[0.0f32, 0.0, 1.0, 1.0])
        .push_graphics_constant(br::ShaderStage::FRAGMENT, 16, &(1.0f32 / 6.0))
        .draw(4, 1, 0, 0)
        .end_render_pass()
    }
}

pub struct AutoExposureEffect {
    histogram_buffer: Rc<peridot_memory_manager::Buffer>,
    clear_pipeline_layout: br::PipelineLayoutObject<StdVkDevice>,
    clear_pipeline: br::PipelineObject<StdVkDevice>,
    histogram_pipeline_layout: br::PipelineLayoutObject<StdVkDevice>,
    histogram_pipeline: br::PipelineObject<StdVkDevice>,
    aggregate_pipeline_layout: br::PipelineLayoutObject<StdVkDevice>,
    aggregate_pipeline: br::PipelineObject<StdVkDevice>,
    _dp: br::DescriptorPoolObject<StdVkDevice>,
    clear_input_descriptor_set: br::DescriptorSet,
    histogram_input_descriptor_set: br::DescriptorSet,
    aggregate_input_descriptor_set: br::DescriptorSet,
    input_size: br::vk::VkExtent2D,
}
impl AutoExposureEffect {
    pub fn new(e: &mut MiniEngine, init_input_size: br::vk::VkExtent2D) -> br::Result<Self> {
        let histogram_buffer = match AppGlobalSharedInstances::get()
            .editor_window_histogram_buffer
            .as_ref()
            .and_then(|x| x.upgrade())
        {
            Some(x) => x,
            None => {
                let b = Rc::new(
                    e.alloc_device_local_buffer(br::BufferDesc::new_for_type::<[u32; 256]>(
                        br::BufferUsage::STORAGE_BUFFER
                            .uniform_buffer()
                            .transfer_src(),
                    ))?,
                );
                AppGlobalSharedInstances::get_mut().editor_window_histogram_buffer =
                    Some(Rc::downgrade(&b));
                b
            }
        };

        let clear_input_set = e.descriptor_set_layout(vec![DescriptorBinding::StorageBuffer(
            1,
            br::ShaderStage::COMPUTE,
        )])?;
        let histogram_input_set = e.descriptor_set_layout(vec![
            DescriptorBinding::StorageBuffer(1, br::ShaderStage::COMPUTE),
            DescriptorBinding::StorageImage(1, br::ShaderStage::COMPUTE),
        ])?;
        let aggregate_input_set = e.descriptor_set_layout(vec![
            DescriptorBinding::StorageBuffer(1, br::ShaderStage::COMPUTE),
            DescriptorBinding::StorageBuffer(1, br::ShaderStage::COMPUTE),
        ])?;

        let clear_sh = e.shader("shaders/posteffects/autoexposure/lum_clear.cspv")?;
        let clear_pipeline_layout = br::PipelineLayoutBuilder::new(vec![&clear_input_set], vec![])
            .create(e.device().clone())?;
        let clear_pipeline = br::ComputePipelineBuilder::new(
            &clear_pipeline_layout,
            br::PipelineShader2::new(&clear_sh, c"main"),
        );

        let histogram_sh = e.shader("shaders/posteffects/autoexposure/lum_histogram.cspv")?;
        let histogram_pipeline_layout = br::PipelineLayoutBuilder::new(
            vec![&histogram_input_set],
            vec![(br::ShaderStage::COMPUTE, 0..16)],
        )
        .create(e.device().clone())?;
        let histogram_pipeline = br::ComputePipelineBuilder::new(
            &histogram_pipeline_layout,
            br::PipelineShader2::new(&histogram_sh, c"main"),
        );

        let aggregate_sh = e.shader("shaders/posteffects/autoexposure/lum_avg.cspv")?;
        let aggregate_pipeline_layout = br::PipelineLayoutBuilder::new(
            vec![&aggregate_input_set],
            vec![(br::ShaderStage::COMPUTE, 0..32)],
        )
        .create(e.device().clone())?;
        let aggregate_pipeline = br::ComputePipelineBuilder::new(
            &aggregate_pipeline_layout,
            br::PipelineShader2::new(&aggregate_sh, c"main"),
        );

        let [clear_pipeline, histogram_pipeline, aggregate_pipeline] = e
            .create_compute_pipeline_array(&[
                clear_pipeline,
                histogram_pipeline,
                aggregate_pipeline,
            ])?;

        let mut dp = br::DescriptorPoolBuilder::new(3)
            .with_reservations(vec![
                br::DescriptorType::StorageBuffer.with_count(4),
                br::DescriptorType::StorageImage.with_count(1),
            ])
            .create(e.device().clone())?;
        let [clear_input_descriptor_set, histogram_input_descriptor_set, aggregate_input_descriptor_set] =
            dp.alloc_array(&[
                br::DescriptorSetLayoutObjectRef::new(&clear_input_set),
                br::DescriptorSetLayoutObjectRef::new(&histogram_input_set),
                br::DescriptorSetLayoutObjectRef::new(&aggregate_input_set),
            ])?;

        Ok(Self {
            histogram_buffer,
            clear_pipeline_layout,
            clear_pipeline,
            histogram_pipeline_layout,
            histogram_pipeline,
            aggregate_pipeline_layout,
            aggregate_pipeline,
            _dp: dp,
            clear_input_descriptor_set,
            histogram_input_descriptor_set,
            aggregate_input_descriptor_set,
            input_size: init_input_size,
        })
    }

    pub fn update_descriptor_sets<'s>(
        &'s self,
        writes: &mut Vec<br::DescriptorSetWriteInfo<'s>>,
        input: &'s (impl br::VkHandle<Handle = br::vk::VkImageView> + ?Sized),
        postfx_global_work_buffer: &'s (impl br::VkHandle<Handle = br::vk::VkBuffer> + ?Sized),
    ) {
        writes.extend([
            self.clear_input_descriptor_set.binding_at(0).write(
                br::DescriptorContents::storage_buffer(
                    &self.histogram_buffer,
                    0..core::mem::size_of::<[u32; 256]>() as u64,
                ),
            ),
            self.histogram_input_descriptor_set.binding_at(0).write(
                br::DescriptorContents::storage_buffer(
                    &self.histogram_buffer,
                    0..core::mem::size_of::<[u32; 256]>() as u64,
                ),
            ),
            self.histogram_input_descriptor_set.binding_at(1).write(
                br::DescriptorContents::storage_image(input, br::ImageLayout::General),
            ),
            self.aggregate_input_descriptor_set.binding_at(0).write(
                br::DescriptorContents::storage_buffer(
                    &self.histogram_buffer,
                    0..core::mem::size_of::<[u32; 256]>() as u64,
                ),
            ),
            self.aggregate_input_descriptor_set.binding_at(1).write(
                br::DescriptorContents::storage_buffer(
                    postfx_global_work_buffer,
                    0..core::mem::size_of::<PostEffectGlobalWorkBuffer>() as u64,
                ),
            ),
        ]);
    }

    pub fn set_input<'s>(
        &mut self,
        writes: &mut Vec<br::DescriptorSetWriteInfo<'s>>,
        input: &'s (impl br::ImageView + br::ImageChild + ?Sized),
    ) {
        self.input_size = *input.image().size().as_2d_ref();
        writes.push(self.histogram_input_descriptor_set.binding_at(1).write(
            br::DescriptorContents::storage_image(input, br::ImageLayout::General),
        ));
    }

    pub fn populate_commands<
        'r,
        CB: br::VkHandleMut<Handle = br::vk::VkCommandBuffer> + ?Sized,
        Device: br::Device + ?Sized,
    >(
        &'r self,
        rec: br::CmdRecord<'r, CB, Device>,
        input: &'r impl br::Image,
    ) -> br::CmdRecord<CB, Device> {
        let min_ev100 = 6.0f32;
        let ev100_range = 20.0f32 - min_ev100;
        let inverse_ev100_range = ev100_range.recip();
        let total_pixel_count = self.input_size.width * self.input_size.height;
        let filter_percentile_low = 0.5f32;
        let filter_percentile_high = 0.9f32;
        let exposure_min_ev = 12.0f32;
        let exposure_max_ev = 13.5f32;

        rec.bind_compute_pipeline_pair(&self.clear_pipeline, &self.clear_pipeline_layout)
            .bind_compute_descriptor_sets(0, &[self.clear_input_descriptor_set.0], &[])
            .dispatch(1, 1, 1)
            .pipeline_barrier_2(&br::DependencyInfo::new(
                &[br::MemoryBarrier2::new()
                    .from(
                        br::PipelineStageFlags2::COMPUTE_SHADER,
                        br::AccessFlags2::SHADER.write,
                    )
                    .to(
                        br::PipelineStageFlags2::COMPUTE_SHADER,
                        br::AccessFlags2::SHADER.read | br::AccessFlags2::SHADER.write,
                    )],
                &[],
                &[],
            ))
            .bind_compute_pipeline_pair(&self.histogram_pipeline, &self.histogram_pipeline_layout)
            .bind_compute_descriptor_sets(0, &[self.histogram_input_descriptor_set.0], &[])
            .push_compute_constant(
                br::ShaderStage::COMPUTE,
                0,
                &[min_ev100, inverse_ev100_range],
            )
            .push_compute_constant(
                br::ShaderStage::COMPUTE,
                8,
                &[self.input_size.width, self.input_size.height],
            )
            .dispatch(
                (self.input_size.width + 15) / 16,
                (self.input_size.height + 15) / 16,
                1,
            )
            .pipeline_barrier_2(&br::DependencyInfo::new(
                &[br::MemoryBarrier2::new()
                    .from(
                        br::PipelineStageFlags2::COMPUTE_SHADER,
                        br::AccessFlags2::SHADER.write,
                    )
                    .to(
                        br::PipelineStageFlags2::COMPUTE_SHADER
                            | br::PipelineStageFlags2::FRAGMENT_SHADER,
                        br::AccessFlags2::SHADER.read,
                    )],
                &[],
                &[input
                    .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                    .memory_barrier2()
                    .transferring_layout(
                        br::ImageLayout::General,
                        br::ImageLayout::ShaderReadOnlyOpt,
                    )],
            ))
            .bind_compute_pipeline_pair(&self.aggregate_pipeline, &self.aggregate_pipeline_layout)
            .bind_compute_descriptor_sets(0, &[self.aggregate_input_descriptor_set.0], &[])
            .push_compute_constant(br::ShaderStage::COMPUTE, 0, &total_pixel_count)
            .push_compute_constant(
                br::ShaderStage::COMPUTE,
                4,
                &[
                    min_ev100,
                    ev100_range,
                    0.1,
                    filter_percentile_low,
                    filter_percentile_high,
                    exposure_min_ev,
                    exposure_max_ev,
                ],
            )
            .dispatch(1, 1, 1)
            .pipeline_barrier_2(&br::DependencyInfo::new(
                &[br::MemoryBarrier2::new()
                    .of_memory(
                        br::AccessFlags2::SHADER.write,
                        br::AccessFlags2::UNIFORM_READ,
                    )
                    .of_execution(
                        br::PipelineStageFlags2::COMPUTE_SHADER,
                        br::PipelineStageFlags2::FRAGMENT_SHADER,
                    )],
                &[],
                &[],
            ))
    }

    pub fn readback_histogram(&self) -> [u32; 256] {
        let mut b = AppSubsystemInstances::get()
            .mini_engine
            .borrow_mut()
            .alloc_upload_buffer(br::BufferDesc::new_for_type::<[u32; 256]>(
                br::BufferUsage::TRANSFER_DEST,
            ))
            .unwrap();

        AppSubsystemInstances::get()
            .mini_engine
            .borrow_mut()
            .submit_transient_commands_and_wait(|r| {
                r.copy_buffer(
                    &self.histogram_buffer,
                    &b,
                    &[br::BufferCopy::mirror_data::<[u32; 256]>(0)],
                )
            })
            .unwrap();

        let mut sink = [0u32; 256];
        b.guard_map(peridot_memory_manager::BufferMapMode::Read, |ptr| {
            sink.copy_from_slice(unsafe { ptr.slice(0, 256) });
        })
        .unwrap();

        sink
    }
}

pub struct EditorStageRenderResources {
    utility_verts: UtilityVertices,
    skybox_renderer: SkyboxRenderer,
    _descriptor_pool: br::DescriptorPoolObject<StdVkDevice>,
    hdr_temp_rt: TempRT,
    depth_stencil_temp_rt: TempRT,
    hdr_main_render_pass: Rc<br::RenderPassObject<StdVkDevice>>,
    hdr_to_ldr_render_pass: Rc<br::RenderPassObject<StdVkDevice>>,
    hdr_final_pass_pipeline_layout: br::PipelineLayoutObject<StdVkDevice>,
    hdr_final_pass_pipeline: br::PipelineObject<StdVkDevice>,
    postfx_global_work_buffer: Rc<peridot_memory_manager::Buffer>,
    postfx_auto_exposure: AutoExposureEffect,
    postfx_bloom: HDRBloomPostEffect,
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
    hdr_main_framebuffer: br::FramebufferObject<'static, StdVkDevice>,
    hdr_bloom_merge_framebuffer: br::FramebufferObject<'static, StdVkDevice>,
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

    const BLOOM_FILTER_ITERATION_COUNT: usize = 6;

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
        let hdr_main_render_pass = e
            .render_pass(RenderPassDescription {
                attachments: vec![
                    br::AttachmentDescription2::new(HDR_COLOR_FORMAT)
                        .with_layout_to(br::ImageLayout::General.from_undefined())
                        .color_memory_op(br::LoadOp::Clear, br::StoreOp::Store),
                    br::AttachmentDescription2::new(br::vk::VK_FORMAT_D24_UNORM_S8_UINT)
                        .with_layout_to(br::ImageLayout::DepthStencilAttachmentOpt.from_undefined())
                        .color_memory_op(br::LoadOp::Clear, br::StoreOp::Store),
                ],
                subpasses: vec![SubpassDescription {
                    color_outputs: vec![br::AttachmentReference2::color_attachment_opt(0)],
                    depth_stencil: Some(br::AttachmentReference2::depth_stencil_attachment_opt(1)),
                    ..SubpassDescription::EMPTY
                }],
                dependencies: vec![br::SubpassDependency2::new(
                    br::SubpassIndex::Internal(0),
                    br::SubpassIndex::External,
                )
                .of_memory(
                    br::AccessFlags::COLOR_ATTACHMENT.write,
                    br::AccessFlags::SHADER.read,
                )
                .of_execution(
                    br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                    br::PipelineStageFlags::COMPUTE_SHADER,
                )],
            })
            .unwrap();
        let hdr_to_ldr_render_pass = e
            .render_pass(RenderPassDescription {
                attachments: vec![
                    // ldr color
                    br::AttachmentDescription2::new(br::vk::VK_FORMAT_R8G8B8A8_UNORM)
                        .with_layout_to(br::ImageLayout::General.from_undefined())
                        .color_memory_op(br::LoadOp::DontCare, br::StoreOp::Store),
                    // hdr color
                    br::AttachmentDescription2::new(HDR_COLOR_FORMAT)
                        .layout(br::ImageLayout::ShaderReadOnlyOpt)
                        .color_memory_op(br::LoadOp::Load, br::StoreOp::DontCare),
                    // depth
                    br::AttachmentDescription2::new(br::vk::VK_FORMAT_D24_UNORM_S8_UINT)
                        .layout(br::ImageLayout::DepthStencilAttachmentOpt)
                        .color_memory_op(br::LoadOp::Load, br::StoreOp::DontCare),
                ],
                subpasses: vec![SubpassDescription {
                    inputs: vec![br::AttachmentReference2::shader_color_readonly_opt(1)],
                    color_outputs: vec![br::AttachmentReference2::color_attachment_opt(0)],
                    depth_stencil: Some(br::AttachmentReference2::depth_stencil_attachment_opt(2)),
                    ..SubpassDescription::EMPTY
                }],
                dependencies: vec![br::SubpassDependency2::new(
                    br::SubpassIndex::Internal(0),
                    br::SubpassIndex::External,
                )
                .of_memory(
                    br::AccessFlags::COLOR_ATTACHMENT.write,
                    br::AccessFlags::MEMORY.read,
                )
                .of_execution(
                    br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                    br::PipelineStageFlags(0),
                )],
            })
            .unwrap();

        let hdr_temp_rt = TempRT::new(
            e,
            br::ImageDesc::new(init_size, HDR_COLOR_FORMAT)
                .as_color_attachment()
                .sampled()
                .as_input_attachment()
                .use_as_storage(),
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

        let descriptor_set_layout_camera_data = e
            .descriptor_set_layout(vec![DescriptorBinding::UniformBuffer(
                1,
                br::ShaderStage::VERTEX | br::ShaderStage::FRAGMENT,
            )])
            .unwrap();

        let full_blit_vsh = e
            .shader("shaders/full_blit.vspv")
            .expect("Failed to load final pass vertex shader");
        let hdr_final_pass_fsh = e
            .shader("shaders/simple2d_hdr_final_pass.fspv")
            .expect("Failed to load final pass fragment shader");
        let descriptor_set_layout_hdr_final_pass_input = e
            .descriptor_set_layout(vec![
                DescriptorBinding::InputAttachment(1, br::ShaderStage::FRAGMENT),
                DescriptorBinding::UniformBuffer(1, br::ShaderStage::FRAGMENT),
            ])
            .unwrap();
        let hdr_final_pass_pipeline_layout = br::PipelineLayoutBuilder::new(
            vec![&descriptor_set_layout_hdr_final_pass_input],
            vec![(br::ShaderStage::FRAGMENT, 0..12)],
        )
        .create(e.device().clone())
        .expect("Failed to create hdr final pass pipeline layout");
        let mut hdr_final_pass_pipeline = br::NonDerivedGraphicsPipelineBuilder::new(
            &hdr_final_pass_pipeline_layout,
            hdr_to_ldr_render_pass.subpass(0),
            br::VertexProcessingStages::new(
                br::VertexShaderStage::new(br::PipelineShader2::new(&full_blit_vsh, c"main"))
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

        let postfx_auto_exposure = AutoExposureEffect::new(e, init_size).unwrap();
        let postfx_bloom = HDRBloomPostEffect::new(
            e,
            br::vk::VkExtent2D {
                width: init_size.width / 2,
                height: init_size.height / 2,
            },
            Self::BLOOM_FILTER_ITERATION_COUNT,
        )
        .unwrap();

        let grid_vsh = e
            .shader("shaders/simple_transformed_static_pos.vspv")
            .expect("Failed to load vertex shader");
        let grid_fsh = e
            .shader("shaders/vertex_color.fspv")
            .expect("Failed to load fragment shader");
        let (grid_vbinds, grid_vattrs) = ColoredVertex::single_binding(0, 1);
        let grid_pipeline_layout = br::PipelineLayoutBuilder::new(
            vec![&descriptor_set_layout_camera_data],
            vec![(br::ShaderStage::VERTEX, 0..64)],
        )
        .create(e.device().clone())
        .expect("Failed to create grid pipeline layout");
        let mut grid_pipeline = br::NonDerivedGraphicsPipelineBuilder::new(
            &grid_pipeline_layout,
            hdr_to_ldr_render_pass.subpass(0),
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

        let default_material_sh = e.shader("shaders/default_material_test.spv").unwrap();
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
        let descriptor_set_layout_default_mat = e
            .descriptor_set_layout(vec![
                DescriptorBinding::UniformBufferDynamic(1, br::ShaderStage::VERTEX),
                DescriptorBinding::UniformBuffer(1, br::ShaderStage::FRAGMENT),
            ])
            .unwrap();
        let default_material_pipeline_layout = br::PipelineLayoutBuilder::new(
            vec![
                &descriptor_set_layout_camera_data,
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
            hdr_main_render_pass.subpass(0),
            br::VertexProcessingStages::new(
                br::VertexShaderStage::new(br::PipelineShader2::new(
                    &default_material_sh,
                    c"vertMain",
                ))
                .with_fragment_shader_stage(br::PipelineShader2::new(
                    &default_material_sh,
                    c"fragMain",
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
            .expect("Failed to create pipeline states");

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

        let postfx_global_work_buffer = match AppGlobalSharedInstances::get()
            .editor_window_postfx_global_work_buffer
            .as_ref()
            .and_then(|x| x.upgrade())
        {
            Some(x) => x,
            None => {
                let b = Rc::new(
                    e.alloc_device_local_buffer(br::BufferDesc::new_for_type::<
                        PostEffectGlobalWorkBuffer,
                    >(
                        br::BufferUsage::STORAGE_BUFFER.uniform_buffer()
                    ))
                    .unwrap(),
                );
                AppGlobalSharedInstances::get_mut().editor_window_postfx_global_work_buffer =
                    Some(Rc::downgrade(&b));
                b
            }
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
                br::DescriptorType::UniformBuffer.with_count(3),
                br::DescriptorType::UniformBufferDynamic.with_count(1),
                br::DescriptorType::InputAttachment.with_count(1),
            ])
            .create(e.device().clone())
            .expect("Failed to create descriptor pool");
        let [camera_descriptor_set, hdr_final_pass_descriptor_set, per_object_descriptor_set] = dp
            .alloc_array(&[
                br::DescriptorSetLayoutObjectRef::new(&descriptor_set_layout_camera_data),
                br::DescriptorSetLayoutObjectRef::new(&descriptor_set_layout_hdr_final_pass_input),
                br::DescriptorSetLayoutObjectRef::new(&descriptor_set_layout_default_mat),
            ])
            .expect("Failed to allocate camera descriptor set");
        let mut descriptor_init_writes = vec![
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
            hdr_final_pass_descriptor_set.binding_at(1).write(
                br::DescriptorContents::uniform_buffer(
                    &postfx_global_work_buffer,
                    0..core::mem::size_of::<PostEffectGlobalWorkBuffer>() as _,
                ),
            ),
            per_object_descriptor_set.binding_at(0).write(
                br::DescriptorContents::uniform_buffer_dynamic(
                    &per_object_uniform_data.array.buffer,
                    per_object_uniform_data.array.data_range(),
                ),
            ),
            per_object_descriptor_set
                .binding_at(1)
                .write(br::DescriptorContents::uniform_buffer(
                    &forward_light_buffer,
                    0..core::mem::size_of::<ForwardLightUniformData>() as _,
                )),
        ];
        postfx_auto_exposure.update_descriptor_sets(
            &mut descriptor_init_writes,
            &hdr_temp_rt.resource,
            &postfx_global_work_buffer,
        );
        postfx_bloom.update_descriptor_sets(
            &mut descriptor_init_writes,
            &hdr_temp_rt.resource,
            &postfx_global_work_buffer,
        );
        e.device()
            .update_descriptor_sets(&descriptor_init_writes, &[]);

        let hdr_main_framebuffer = br::FramebufferBuilder::new_with_attachments(
            &hdr_main_render_pass,
            vec![
                hdr_temp_rt.resource.clone(),
                depth_stencil_temp_rt.resource.clone(),
            ],
        )
        .create()
        .unwrap();
        let hdr_bloom_merge_framebuffer = br::FramebufferBuilder::new_with_attachments(
            &postfx_bloom.hdr_color_overwrite_pass,
            vec![hdr_temp_rt.resource.clone()],
        )
        .create()
        .unwrap();

        let skybox_renderer = SkyboxRenderer::new(
            e,
            &descriptor_set_layout_camera_data,
            hdr_main_render_pass.subpass(0),
            skybox_precomputed,
            init_light_data,
        )
        .unwrap();

        Self {
            utility_verts,
            skybox_renderer,
            _descriptor_pool: dp,
            hdr_temp_rt,
            depth_stencil_temp_rt,
            hdr_main_render_pass,
            hdr_to_ldr_render_pass,
            hdr_final_pass_pipeline_layout,
            hdr_final_pass_pipeline,
            postfx_global_work_buffer,
            postfx_auto_exposure,
            postfx_bloom,
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
            hdr_main_framebuffer,
            hdr_bloom_merge_framebuffer,
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

        rec.set_viewport(0, &[viewport.clone()])
            .set_scissor(0, &[rect])
            .begin_render_pass(
                &self.hdr_main_render_pass,
                &self.hdr_main_framebuffer,
                rect,
                &[
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
            .end_render_pass()
            .inject(|r| {
                self.postfx_auto_exposure
                    .populate_commands(r, self.hdr_temp_rt.resource.image())
            })
            .inject(|r| {
                self.postfx_bloom
                    .populate_commands(r, &self.hdr_bloom_merge_framebuffer, rect)
            })
            .begin_render_pass(&self.hdr_to_ldr_render_pass, fb, rect, &[], true)
            .bind_graphics_pipeline_pair(
                &self.hdr_final_pass_pipeline,
                &self.hdr_final_pass_pipeline_layout,
            )
            .bind_graphics_descriptor_sets(0, &[self.hdr_final_pass_descriptor_set.0], &[])
            .push_graphics_constant(br::ShaderStage::FRAGMENT, 0, &[100.0f32, 12.5, 0.65])
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
        self.postfx_bloom
            .resize(br::vk::VkExtent2D {
                width: new_size.width / 2,
                height: new_size.height / 2,
            })
            .unwrap();

        let mut descriptor_update_writes = Vec::new();
        self.postfx_auto_exposure
            .set_input(&mut descriptor_update_writes, &self.hdr_temp_rt.resource);
        self.postfx_bloom.update_descriptor_sets(
            &mut descriptor_update_writes,
            &self.hdr_temp_rt.resource,
            &self.postfx_global_work_buffer,
        );
        AppSubsystemInstances::get()
            .mini_engine
            .borrow()
            .device()
            .update_descriptor_sets(&descriptor_update_writes, &[]);

        self.hdr_main_framebuffer = br::FramebufferBuilder::new_with_attachments(
            &self.hdr_main_render_pass,
            vec![
                self.hdr_temp_rt.resource.clone(),
                self.depth_stencil_temp_rt.resource.clone(),
            ],
        )
        .create()
        .unwrap();
        self.hdr_bloom_merge_framebuffer = br::FramebufferBuilder::new_with_attachments(
            &self.postfx_bloom.hdr_color_overwrite_pass,
            vec![self.hdr_temp_rt.resource.clone()],
        )
        .create()
        .unwrap();
    }

    pub fn readback_postfx_global_work_buffer(&self) -> PostEffectGlobalWorkBuffer {
        let mut b = AppSubsystemInstances::get()
            .mini_engine
            .borrow_mut()
            .alloc_upload_buffer(br::BufferDesc::new_for_type::<PostEffectGlobalWorkBuffer>(
                br::BufferUsage::TRANSFER_DEST,
            ))
            .unwrap();

        AppSubsystemInstances::get()
            .mini_engine
            .borrow_mut()
            .submit_transient_commands_and_wait(|r| {
                r.copy_buffer(
                    &self.postfx_global_work_buffer,
                    &b,
                    &[br::BufferCopy::mirror_data::<PostEffectGlobalWorkBuffer>(0)],
                )
            })
            .unwrap();

        let mut sink = core::mem::MaybeUninit::<PostEffectGlobalWorkBuffer>::uninit();
        b.guard_map(peridot_memory_manager::BufferMapMode::Read, |ptr| unsafe {
            sink.write(ptr.get_at::<PostEffectGlobalWorkBuffer>(0).clone())
        })
        .unwrap();

        unsafe { sink.assume_init() }
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

            let vk_framebuffer =
                br::FramebufferBuilder::new(&render_resources.hdr_to_ldr_render_pass)
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

            let vk_framebuffer = br::FramebufferBuilder::new(&resources.hdr_to_ldr_render_pass)
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
impl MountableView for EditorStageView {
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

pub struct CompositionTabPresenter {}
impl PaneTabContentPresenter for CompositionTabPresenter {
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
impl PaneTabPresenter for CompositionTabPresenter {
    const INIT_TAB_NAME: &'static str = "Composition Graph";

    fn new(
        _tab_header_view: &SharedMut<PaneTabHeaderView>,
        _view_ctx: &(impl ViewContext + ?Sized),
        _app_state: &MTSharedMut<AppState>,
    ) -> Self {
        Self {}
    }
}

pub struct ObjectTreeElementRowViewSelectViewRefs {
    bound_object_id: Uuid,
    select_bg: SpriteVisual,
    select_animation: ScalarKeyFrameAnimation,
    unselect_animation: ScalarKeyFrameAnimation,
    is_selected: AtomicBool,
}

pub struct ObjectTreeElementRowViewState {
    root: ContainerVisual,
    ht: HitTestTree,
    bg: SpriteVisual,
    bg_hover_animation: ScalarKeyFrameAnimation,
    bg_hover_end_animation: ScalarKeyFrameAnimation,
    bound_object_id: Uuid,
    rendered_dpi: f32,
    app_state: MTSharedMut<AppState>,
    parent: ObjectTreeTabPresenterWeakRef,
    selection_changed_handler: Arc<ObjectTreeElementRowViewSelectViewRefs>,
}
#[derive(Clone)]
pub struct ObjectTreeElementRowView(SharedMut<ObjectTreeElementRowViewState>);
impl ObjectTreeElementRowView {
    #[inline(always)]
    pub fn make_weak_ref(&self) -> ObjectTreeElementRowViewWeakRef {
        ObjectTreeElementRowViewWeakRef(Rc::downgrade(&self.0))
    }
}
#[derive(Clone)]
pub struct ObjectTreeElementRowViewWeakRef(WeakMut<ObjectTreeElementRowViewState>);
impl ObjectTreeElementRowViewWeakRef {
    #[inline(always)]
    pub fn upgrade(&self) -> Option<ObjectTreeElementRowView> {
        self.0.upgrade().map(ObjectTreeElementRowView)
    }
}
impl ObjectTreeElementRowView {
    const PADDING_Y: f32 = 2.0;
    const PADDING_X: f32 = 8.0;
    const HOVER_ANIMATION_DURATION: TimeSpan = timespan_ms(50);
    const SELECT_ANIMATION_DURATION: TimeSpan = timespan_ms(100);
    const HOVER_COLOR: Color = Color {
        A: 16,
        R: 255,
        G: 255,
        B: 255,
    };
    const SELECT_BG_COLOR: Color = Color {
        A: 255,
        R: 64,
        G: 160,
        B: 255,
    };

    pub fn new(
        ref_dpi: f32,
        label_offset: f32,
        init_name: impl Into<Cow<'static, str>>,
        bound_object_id: Uuid,
        init_selected: bool,
        app_state: &MTSharedMut<AppState>,
        parent: &ObjectTreeTabPresenterWeakRef,
    ) -> windows::core::Result<Self> {
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

        let select_bg = AppSubsystemInstances::get()
            .compositor
            .CreateSpriteVisual()?;
        select_bg
            .set_properties()
            .brush(
                &AppSubsystemInstances::get()
                    .compositor
                    .CreateColorBrushWithColor(Self::SELECT_BG_COLOR)?,
            )?
            .expand_to_parent()?
            .opacity(if init_selected { 1.0 } else { 0.0 })?;
        root.Children()?.InsertAtTop(&select_bg)?;

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
                X: Self::PADDING_X + label_offset,
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

        let select_animation = AppSubsystemInstances::get()
            .compositor
            .CreateScalarKeyFrameAnimation()?;
        select_animation
            .keyframe(0.0, 0.0)?
            .interpolate(1.0, 1.0, &linear_easing)?
            .set_properties()
            .duration(Self::SELECT_ANIMATION_DURATION)?;
        let unselect_animation = AppSubsystemInstances::get()
            .compositor
            .CreateScalarKeyFrameAnimation()?;
        unselect_animation
            .keyframe(0.0, 1.0)?
            .interpolate(1.0, 0.0, &linear_easing)?
            .set_properties()
            .duration(Self::SELECT_ANIMATION_DURATION)?;

        Ok(Self(new_cyclic_shared_mut(|wthis| {
            let ht = HitTestTree::new(
                Some(ObjectTreeElementRowViewWeakRef(wthis.clone())),
                Rect::from_size(core::f32::MAX, label_surface.height + Self::PADDING_Y * 2.0),
                Rect::empty(),
            );

            ObjectTreeElementRowViewState {
                root,
                ht,
                bg,
                bg_hover_animation,
                bg_hover_end_animation,
                bound_object_id,
                rendered_dpi: ref_dpi,
                app_state: app_state.clone(),
                parent: parent.clone(),
                selection_changed_handler: Arc::new(ObjectTreeElementRowViewSelectViewRefs {
                    bound_object_id,
                    select_bg,
                    select_animation,
                    unselect_animation,
                    is_selected: AtomicBool::new(init_selected),
                }),
            }
        })))
    }

    pub fn height(&self) -> f32 {
        self.0.borrow().ht.rect().Height
    }

    pub fn reposition(&self, pos: Vector2) -> windows::core::Result<()> {
        self.0.borrow().root.SetOffset(pos.with_z(0.0))?;
        self.0.borrow().ht.set_offset(pos.X, pos.Y);

        Ok(())
    }
}
impl MountableView for ObjectTreeElementRowView {
    fn mount(&self, onto: &VisualCollection, onto_ht: &HitTestTree) -> windows::core::Result<()> {
        onto.InsertAtTop(&self.0.borrow().root)?;
        onto_ht.add_child(&self.0.borrow().ht);

        AppState::observe_current_selection_changes(
            &self.0.borrow().app_state,
            &self.0.borrow().selection_changed_handler,
            &ViewContext1 {
                current_dpi: self.0.borrow().rendered_dpi,
            },
        );

        Ok(())
    }

    fn unmount(&self) -> windows::core::Result<()> {
        self.0
            .borrow()
            .root
            .Parent()?
            .Children()?
            .Remove(&self.0.borrow().root)?;
        self.0.borrow().ht.unmount();

        self.0
            .borrow()
            .app_state
            .write()
            .unobserve_current_selection_changes(&Arc::downgrade(
                &self.0.borrow().selection_changed_handler,
            ));

        Ok(())
    }
}
impl InputEventHandler for ObjectTreeElementRowViewWeakRef {
    fn on_pointer_enter(&self, _ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };

        this.0
            .borrow()
            .bg
            .StartAnimation(h!("Opacity"), &this.0.borrow().bg_hover_animation)
            .expect("Failed to start hover animation");
    }

    fn on_pointer_leave(&self, _ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };

        this.0
            .borrow()
            .bg
            .StartAnimation(h!("Opacity"), &this.0.borrow().bg_hover_end_animation)
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
            Some(this.0.borrow().bound_object_id.clone()),
            &mut ctx,
        );
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
                                {
                                    let app_state = this.0.borrow().app_state.clone();
                                    let ref_dpi = ctx.current_dpi();
                                    let parent_w = this.0.borrow().parent.clone();
                                    let bound_object_id = this.0.borrow().bound_object_id;

                                    new_shared_mut(move || {
                                        let Some(parent) = parent_w.upgrade() else {
                                            return;
                                        };

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
                                            parent_id: Some(bound_object_id),
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
                                        app_state
                                            .write()
                                            .current_scene
                                            .add_object_under(bound_object_id, new_object);

                                        // refresh list
                                        for v in parent.0.borrow().rows.iter() {
                                            v.unmount()
                                                .expect("Failed to unmount old element rows");
                                        }

                                        parent.0.borrow_mut().rows.clear();
                                        parent.rebuild_views(ref_dpi, &app_state);

                                        let children = parent
                                            .0
                                            .borrow()
                                            .mounted_visual_root
                                            .as_ref()
                                            .unwrap()
                                            .Children()
                                            .unwrap();
                                        for v in parent.0.borrow().rows.iter() {
                                            v.mount(
                                                &children,
                                                parent.0.borrow().mounted_ht.as_ref().unwrap(),
                                            )
                                            .expect("Failed to mount new rows");
                                        }
                                    })
                                },
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
impl AppStateCurrentSelectionChangedHandler for ObjectTreeElementRowViewSelectViewRefs {
    fn on_changed(&self, app_state: &MTSharedMut<AppState>, _view_context: &dyn ViewContext) {
        let selected = app_state
            .read()
            .current_selection_object_id
            .is_some_and(|id| id == self.bound_object_id);

        if self
            .is_selected
            .swap(selected, std::sync::atomic::Ordering::AcqRel)
            == selected
        {
            // 状態が変わらなかった
            return;
        }

        if selected {
            self.select_bg
                .StartAnimation(h!("Opacity"), &self.select_animation)
                .unwrap();
        } else {
            self.select_bg
                .StartAnimation(h!("Opacity"), &self.unselect_animation)
                .unwrap();
        }
    }
}

struct ObjectTreeTabState {
    rows: Vec<ObjectTreeElementRowView>,
    app_state: MTSharedMut<AppState>,
    mounted_visual_root: Option<ContainerVisual>,
    mounted_ht: Option<HitTestTree>,
}
#[derive(Clone)]
pub struct ObjectTreeTabPresenter(SharedMut<ObjectTreeTabState>);
#[derive(Clone)]
pub struct ObjectTreeTabPresenterWeakRef(WeakMut<ObjectTreeTabState>);
impl ObjectTreeTabPresenter {
    pub fn make_weak_ref(&self) -> ObjectTreeTabPresenterWeakRef {
        ObjectTreeTabPresenterWeakRef(Rc::downgrade(&self.0))
    }
}
impl ObjectTreeTabPresenterWeakRef {
    pub fn upgrade(&self) -> Option<ObjectTreeTabPresenter> {
        self.0.upgrade().map(ObjectTreeTabPresenter)
    }
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
        for r in self.0.borrow().rows.iter() {
            r.mount(&children, onto_ht)?;
        }

        self.0.borrow_mut().mounted_visual_root = Some(onto.clone());
        self.0.borrow_mut().mounted_ht = Some(onto_ht.clone());

        Ok(())
    }

    fn on_hide_content_view(
        &mut self,
        _view_context: &dyn ViewContext,
        _app_state: &MTSharedMut<AppState>,
    ) -> windows::core::Result<()> {
        for r in self.0.borrow().rows.iter() {
            r.unmount()?;
        }

        self.0.borrow_mut().mounted_visual_root = None;
        self.0.borrow_mut().mounted_ht = None;

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
                                    let app_state = self.0.borrow().app_state.clone();
                                    let this_wref = self.make_weak_ref();
                                    let ref_dpi = input_context.current_dpi();

                                    new_shared_mut(move || {
                                        let Some(this) = this_wref.upgrade() else {
                                            return;
                                        };

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
                                        #[repr(C)]
                                        pub struct BufferInitializationContents {
                                            vertices: [GenericVertex; 24],
                                            indices: [u16; 36],
                                        }
                                        let mut init_buffer = AppSubsystemInstances::get()
                                            .mini_engine
                                            .borrow_mut()
                                            .alloc_upload_buffer(br::BufferDesc::new_for_type::<
                                                BufferInitializationContents,
                                            >(
                                                br::BufferUsage::TRANSFER_SRC
                                            ))
                                            .expect("Failed to allocate new cube stg buffers");
                                        let (v, i) = GenericVertex::unit_cube();
                                        init_buffer
                                            .write_content(BufferInitializationContents {
                                                vertices: v,
                                                indices: i,
                                            })
                                            .unwrap();
                                        AppSubsystemInstances::get()
                                            .mini_engine
                                            .borrow_mut()
                                            .submit_transient_commands_and_wait(|rec| {
                                                rec.copy_buffer(
                                                    &init_buffer,
                                                    &vertex_buffer,
                                                    &[br::BufferCopy::copy_data::<
                                                        [GenericVertex; 24],
                                                    >(
                                                        core::mem::offset_of!(
                                                            BufferInitializationContents,
                                                            vertices
                                                        )
                                                            as _,
                                                        0,
                                                    )],
                                                )
                                                .copy_buffer(
                                                    &init_buffer,
                                                    &index_buffer,
                                                    &[br::BufferCopy::copy_data::<[u16; 36]>(
                                                        core::mem::offset_of!(
                                                            BufferInitializationContents,
                                                            indices
                                                        )
                                                            as _,
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
                                                            br::PipelineStageFlags2::VERTEX_ATTRIBUTE_INPUT
                                                                | br::PipelineStageFlags2::INDEX_INPUT,
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
                                            parent_id: None,
                                            name: "New Cube".into(),
                                            order: app_state.read().current_scene.next_order(),
                                            is_dirty: true,
                                            details: ObjectDetails::Mesh {
                                                vertex_buffer,
                                                index_buffer: Some(index_buffer),
                                                vertex_count: 36,
                                                position: peridot_math::Vector3::ZERO,
                                                rotation: peridot_math::Quaternion::ONE,
                                                scale: peridot_math::Vector3::ONE,
                                            },
                                        };
                                        app_state.write().current_scene.add_object(new_object);

                                        // refresh list
                                        for v in this.0.borrow().rows.iter() {
                                            v.unmount()
                                                .expect("Failed to unmount old element rows");
                                        }

                                        this.0.borrow_mut().rows.clear();
                                        this.rebuild_views(ref_dpi, &app_state);

                                        let children = this.0.borrow().mounted_visual_root.as_ref().unwrap().Children().unwrap();
                                        for v in this.0.borrow().rows.iter() {
                                            v.mount(&children, this.0.borrow().mounted_ht.as_ref().unwrap())
                                                .expect("Failed to mount new rows");
                                        }
                                    })
                                },
                                true,
                            ),
                            MenuItem::Command(
                                "Plane".into(),
                                {
                                    let app_state = self.0.borrow().app_state.clone();
                                    let this_wref = self.make_weak_ref();
                                    let ref_dpi = input_context.current_dpi();

                                    new_shared_mut(move || {
                                        let Some(this) = this_wref.upgrade() else {
                                            return;
                                        };

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
                                            parent_id: None,
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
                                        for v in this.0.borrow().rows.iter() {
                                            v.unmount()
                                                .expect("Failed to unmount old element rows");
                                        }

                                        this.0.borrow_mut().rows.clear();
                                        this.rebuild_views(ref_dpi, &app_state);

                                        let children = this
                                            .0
                                            .borrow()
                                            .mounted_visual_root
                                            .as_ref()
                                            .unwrap()
                                            .Children()
                                            .unwrap();
                                        for v in this.0.borrow().rows.iter() {
                                            v.mount(
                                                &children,
                                                this.0.borrow().mounted_ht.as_ref().unwrap(),
                                            )
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
        let this = Self(new_shared_mut(ObjectTreeTabState {
            rows: Vec::new(),
            app_state: app_state.clone(),
            mounted_visual_root: None,
            mounted_ht: None,
        }));

        this.rebuild_views(view_ctx.current_dpi(), app_state);
        this
    }
}
impl ObjectTreeTabPresenter {
    pub fn rebuild_views(&self, ref_dpi: f32, app_state: &MTSharedMut<AppState>) {
        fn recursive(
            ref_dpi: f32,
            view_store: &mut Vec<ObjectTreeElementRowView>,
            id_list: Vec<Uuid>,
            objects: &HashMap<Uuid, ObjectEditState>,
            left_offset: f32,
            mut base_y: f32,
            app_state: &MTSharedMut<AppState>,
            parent_w: &ObjectTreeTabPresenterWeakRef,
        ) -> f32 {
            let mut target_objects = id_list.iter().map(|id| &objects[id]).collect::<Vec<_>>();
            target_objects.sort_by_key(|x| x.order);

            for x in target_objects {
                let p = ObjectTreeElementRowView::new(
                    ref_dpi,
                    left_offset,
                    x.name.to_owned(),
                    x.id.clone(),
                    app_state
                        .read()
                        .current_selection_object_id
                        .is_some_and(|id| id == x.id),
                    app_state,
                    parent_w,
                )
                .expect("Failed to create row view");
                p.reposition(Vector2 { X: 0.0, Y: base_y })
                    .expect("Failed to reposition row view");
                base_y += p.height();

                view_store.push(p);

                base_y = recursive(
                    ref_dpi,
                    view_store,
                    app_state
                        .read()
                        .current_scene
                        .object_tree
                        .get(&x.id)
                        .map_or_else(Vec::new, |x| x.clone()),
                    objects,
                    left_offset + 16.0,
                    base_y,
                    app_state,
                    parent_w,
                );
            }

            base_y
        }

        let first_object_id_list = app_state
            .read()
            .current_scene
            .root_objects
            .iter()
            .copied()
            .collect::<Vec<_>>();

        recursive(
            ref_dpi,
            &mut self.0.borrow_mut().rows,
            first_object_id_list,
            &app_state.read().current_scene.objects,
            0.0,
            0.0,
            app_state,
            &self.make_weak_ref(),
        );
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
    pub root_objects: HashSet<Uuid>,
    pub object_tree: HashMap<Uuid, Vec<Uuid>>,
    pub is_dirty: bool,
}
impl SceneEditState {
    pub fn new() -> Self {
        Self {
            objects: HashMap::new(),
            root_objects: HashSet::new(),
            object_tree: HashMap::new(),
            is_dirty: false,
        }
    }

    pub fn add_object(&mut self, mut state: ObjectEditState) {
        // 次のレンダリングサイクルでデータ載せてほしいのでdirtyフラグを立てておく
        state.is_dirty = true;
        self.is_dirty = true;
        self.object_tree.insert(state.id, Vec::new());
        self.root_objects.insert(state.id);
        self.objects.insert(state.id, state);
    }

    pub fn add_object_under(&mut self, parent_id: Uuid, mut state: ObjectEditState) {
        // 次のレンダリングサイクルでデータ載せてほしいのでdirtyフラグを立てておく
        state.is_dirty = true;
        self.is_dirty = true;
        self.object_tree
            .entry(parent_id)
            .or_insert_with(Vec::new)
            .push(state.id);
        self.objects.insert(state.id, state);
    }

    pub fn mark_dirty_recursive(&mut self, id: Uuid) {
        if let Some(x) = self.objects.get_mut(&id) {
            x.is_dirty = true;
        }

        let child_ids = self
            .object_tree
            .get(&id)
            .map_or(&[][..], |x| &x[..])
            .iter()
            .copied()
            .collect::<Vec<_>>();
        for cid in child_ids {
            self.mark_dirty_recursive(cid);
        }
    }

    pub fn next_order(&self) -> u32 {
        self.objects.values().map(|x| x.order).max().unwrap_or(0) + 1
    }
}

pub struct ObjectEditState {
    pub id: Uuid,
    pub parent_id: Option<Uuid>,
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

static mut APP_GLOBAL_SHARED_INSTANCES: *mut AppGlobalSharedInstances = core::ptr::null_mut();
pub struct AppGlobalSharedInstances {
    pub editor_window_postfx_global_work_buffer: Option<Weak<peridot_memory_manager::Buffer>>,
    pub editor_window_histogram_buffer: Option<Weak<peridot_memory_manager::Buffer>>,
}
impl AppGlobalSharedInstances {
    #[inline(always)]
    pub fn get<'a>() -> &'a Self {
        unsafe { &*APP_GLOBAL_SHARED_INSTANCES }
    }

    #[inline(always)]
    pub fn get_mut<'a>() -> &'a mut Self {
        unsafe { &mut *APP_GLOBAL_SHARED_INSTANCES }
    }

    #[inline]
    pub fn initialize() -> AppGlobalSharedInstancesFinalizer {
        unsafe {
            APP_GLOBAL_SHARED_INSTANCES = Box::into_raw(Box::new(Self {
                editor_window_postfx_global_work_buffer: None,
                editor_window_histogram_buffer: None,
            }));
        }

        AppGlobalSharedInstancesFinalizer
    }
}
pub struct AppGlobalSharedInstancesFinalizer;
impl Drop for AppGlobalSharedInstancesFinalizer {
    #[inline]
    fn drop(&mut self) {
        unsafe {
            drop(Box::from_raw(core::mem::replace(
                &mut *core::ptr::addr_of_mut!(APP_GLOBAL_SHARED_INSTANCES),
                core::ptr::null_mut(),
            )));
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

fn init_bg(root_visual: &ContainerVisual) -> windows::core::Result<()> {
    let bg = AppSubsystemInstances::get()
        .compositor
        .CreateSpriteVisual()?;
    bg.set_properties()
        .brush(&{
            let b = AppSubsystemInstances::get()
                .compositor
                .CreateLinearGradientBrush()
                .expect("Failed to create bg brush");
            let color_stops = b.ColorStops().expect("Failed to get color stop collection");
            color_stops
                .Append(
                    &AppSubsystemInstances::get()
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
                    &AppSubsystemInstances::get()
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
        })?
        .expand_to_parent()?;
    root_visual.Children()?.InsertAtBottom(&bg)?;

    Ok(())
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
        parent_id: None,
        name: "Camera".into(),
        order: 0,
        is_dirty: false,
        details: ObjectDetails::Camera {},
    };
    state.current_scene.add_object(obj);
    let obj = ObjectEditState {
        id: Uuid::new_v4(),
        parent_id: None,
        name: "Sun Light".into(),
        order: 1,
        is_dirty: false,
        details: ObjectDetails::SunLight {
            rotation: peridot_math::QuaternionF32::new(
                45.0f32.to_radians(),
                peridot_math::Vector3::right(),
            ),
            intensity: 120000.0,
        },
    };
    state.current_scene.add_object(obj);
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
    let app_global_shared_instances_finalizer = AppGlobalSharedInstances::initialize();
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

    if !MicaController::IsSupported().unwrap() {
        panic!("mica is not supported");
    }
    // enable mica background

    // どうやらMicaControllerか何かが生き残っていないとエラーになるっぽい？
    let mica = MicaController::new().unwrap();
    let system_backdrop_configuration = SystemBackdropConfiguration::new().unwrap();
    system_backdrop_configuration
        .SetIsInputActive(true)
        .unwrap();
    system_backdrop_configuration
        .SetTheme(SystemBackdropTheme::Default)
        .unwrap();
    mica.SetKind(MicaKind::BaseAlt).unwrap();
    mica.SetSystemBackdropConfiguration(&system_backdrop_configuration)
        .unwrap();

    mica.AddSystemBackdropTarget(
        &desktop_window_target
            .cast::<ICompositionSupportsSystemBackdrop>()
            .unwrap(),
    )
    .unwrap();

    init_bg(&composition_root).expect("Failed to initialize bg visual");

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

    let view_context = ViewContext1 {
        current_dpi: window_handle.current_dpi,
    };

    let pane_group_docking_manager = new_shared_mut(
        PaneGroupDockingManager::new(&hittest_tree_root)
            .expect("Failed to initialize docking manager"),
    );

    let sequences_pane = TabGroupPaneView::new(&pane_group_docking_manager)
        .expect("Failed to create TabGroupPaneView");
    TabGroupPaneView::add_tab::<TimelineTabPresenter>(&sequences_pane, &view_context, &state)
        .expect("Failed to create SceneViewPaneTabHeader");
    sequences_pane.borrow_mut().rearrange(&ResizeContext {
        current_dpi: window_handle.current_dpi,
    });

    let main_pane = TabGroupPaneView::new(&pane_group_docking_manager)
        .expect("Failed to create TabGroupPaneView");
    TabGroupPaneView::add_tab::<StageTabPresenter>(&main_pane, &view_context, &state)
        .expect("Failed to create StagePaneTab");
    TabGroupPaneView::add_tab::<PreviewTabPresenter>(&main_pane, &view_context, &state)
        .expect("Failed to create PreviewPaneTab");
    TabGroupPaneView::add_tab::<ProjectSettingsTabPresenter>(&main_pane, &view_context, &state)
        .expect("Failed to create ProjectSettingsPaneTab");
    TabGroupPaneView::add_tab::<CompositionTabPresenter>(&main_pane, &view_context, &state)
        .expect("Failed to create CompositionPaneTab");
    main_pane.borrow_mut().rearrange(&ResizeContext {
        current_dpi: window_handle.current_dpi,
    });

    let inspection_pane = TabGroupPaneView::new(&pane_group_docking_manager)
        .expect("Failed to create TabGroupPaneView");
    TabGroupPaneView::add_tab::<InspectorTabPresenter>(&inspection_pane, &view_context, &state)
        .expect("Failed to create InspectorPaneTabHeader");
    inspection_pane.borrow_mut().rearrange(&ResizeContext {
        current_dpi: window_handle.current_dpi,
    });
    inspection_pane
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
    TabGroupPaneView::add_tab::<AssetExplorerTabPresenter>(&explorers_pane, &view_context, &state)
        .expect("Failed to create AssetExplorerTab");
    TabGroupPaneView::add_tab::<MonitorTabPresenter>(&explorers_pane, &view_context, &state)
        .unwrap();
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
    TabGroupPaneView::add_tab::<ObjectTreeTabPresenter>(&scene_subinfo_pane, &view_context, &state)
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
                |parent, _| PaneDockLayer::new_filled(&inspection_pane, parent),
                |parent, ctx| {
                    PaneDockLayer::new_on(
                        DockDirection::Top,
                        parent,
                        |parent, _| PaneDockLayer::new_filled(&sequences_pane, parent),
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
                &view_context,
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
    drop(app_global_shared_instances_finalizer);
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
