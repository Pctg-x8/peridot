use std::{borrow::Cow, collections::HashMap};

use windows::{
    core::{h, s, w, Interface, PCSTR, PCWSTR},
    Foundation::{
        Numerics::{Vector2, Vector3},
        Size, TimeSpan,
    },
    Graphics::DirectX::{DirectXAlphaMode, DirectXPixelFormat},
    Win32::{
        Foundation::{HWND, LPARAM, LRESULT, POINT, WPARAM},
        Graphics::{
            Direct2D::{
                Common::{D2D1_COLOR_F, D2D_POINT_2F},
                D2D1CreateFactory, ID2D1DeviceContext, ID2D1Factory1, D2D1_DEBUG_LEVEL_WARNING,
                D2D1_DRAW_TEXT_OPTIONS_NONE, D2D1_FACTORY_OPTIONS,
                D2D1_FACTORY_TYPE_SINGLE_THREADED,
            },
            Direct3D::{D3D_DRIVER_TYPE_HARDWARE, D3D_FEATURE_LEVEL},
            Direct3D11::{
                D3D11CreateDevice, ID3D11Device, ID3D11DeviceContext,
                D3D11_CREATE_DEVICE_BGRA_SUPPORT, D3D11_SDK_VERSION,
            },
            DirectWrite::{
                DWriteCreateFactory, IDWriteFactory, IDWriteTextFormat, DWRITE_FACTORY_TYPE_SHARED,
                DWRITE_FONT_STRETCH_NORMAL, DWRITE_FONT_STYLE_NORMAL, DWRITE_FONT_WEIGHT,
                DWRITE_FONT_WEIGHT_NORMAL, DWRITE_FONT_WEIGHT_SEMI_BOLD, DWRITE_TEXT_METRICS,
            },
            Dxgi::IDXGIDevice,
            Gdi::HBRUSH,
        },
        System::{
            LibraryLoader::GetModuleHandleA,
            WinRT::{
                Composition::{
                    ICompositionDrawingSurfaceInterop, ICompositorDesktopInterop,
                    ICompositorInterop,
                },
                CreateDispatcherQueueController, DispatcherQueueOptions, DQTAT_COM_ASTA,
                DQTYPE_THREAD_CURRENT,
            },
        },
        UI::{
            HiDpi::GetDpiForWindow,
            WindowsAndMessaging::{
                CreateWindowExA, DefWindowProcA, DispatchMessageA, GetMessageA, GetWindowLongPtrA,
                LoadCursorA, LoadIconA, PostQuitMessage, RegisterClassExA, SetWindowLongPtrA,
                ShowWindow, TranslateMessage, CW_USEDEFAULT, IDC_ARROW, IDI_APPLICATION, MSG,
                SW_SHOWNORMAL, WINDOW_LONG_PTR_INDEX, WM_DESTROY, WM_LBUTTONDOWN, WM_LBUTTONUP,
                WM_MOUSEMOVE, WNDCLASSEXA, WNDCLASS_STYLES, WS_EX_APPWINDOW,
                WS_EX_NOREDIRECTIONBITMAP, WS_OVERLAPPEDWINDOW,
            },
        },
    },
    UI::{
        Color,
        Composition::{
            CompositionColorBrush, CompositionDrawingSurface, CompositionGraphicsDevice,
            CompositionLinearGradientBrush, CompositionSurfaceBrush, ContainerVisual, LayerVisual,
            ScalarKeyFrameAnimation, ShapeVisual,
        },
    },
};

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, PartialOrd)]
struct SafeF32(f32);
impl SafeF32 {
    #[inline(always)]
    pub fn new(value: f32) -> Self {
        assert!(!value.is_nan(), "NaN value is not allowed");

        Self(value)
    }

    #[inline(always)]
    pub const fn value(&self) -> f32 {
        self.0
    }
}
impl From<f32> for SafeF32 {
    fn from(value: f32) -> Self {
        Self::new(value)
    }
}
impl From<SafeF32> for f32 {
    fn from(value: SafeF32) -> Self {
        value.0
    }
}
impl core::cmp::Eq for SafeF32 {}
impl core::cmp::Ord for SafeF32 {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        unsafe { self.partial_cmp(other).unwrap_unchecked() }
    }
}
impl core::hash::Hash for SafeF32 {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.to_ne_bytes().hash(state)
    }
}

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
struct TextFormatStock {
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
struct TextSurface {
    surface: CompositionDrawingSurface,
    interop: ICompositionDrawingSurfaceInterop,
    width: f32,
    height: f32,
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
struct TextSurfaceStock {
    dwrite_factory: IDWriteFactory,
    composition_graphics_device: CompositionGraphicsDevice,
    target_window_dpi: f32,
    surfaces: HashMap<(*const IDWriteTextFormat, Cow<'static, str>), TextSurface>,
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
        match self.surfaces.entry((fmt as *const _, text.into())) {
            std::collections::hash_map::Entry::Occupied(e) => Ok(e.get().clone()),
            std::collections::hash_map::Entry::Vacant(e) => {
                let text_layout = unsafe {
                    self.dwrite_factory.CreateTextLayout(
                        &e.key().1.encode_utf16().collect::<Vec<_>>(),
                        &*e.key().0,
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
                    interop: surface_interop,
                    width: text_metrics.width,
                    height: text_metrics.height,
                })
                .clone())
            }
        }
    }
}

struct UICommonObjects {
    tab_base_brush: CompositionColorBrush,
    tab_active_overlay_brush: CompositionLinearGradientBrush,
    tab_title_font: IDWriteTextFormat,
    tab_active_title_font: IDWriteTextFormat,
    tab_hover_animation: ScalarKeyFrameAnimation,
    tab_hover_end_animation: ScalarKeyFrameAnimation,
    tab_active_overlay_enter_animation: ScalarKeyFrameAnimation,
    tab_active_overlay_leave_animation: ScalarKeyFrameAnimation,
}

pub struct ViewContext<'r> {
    compositor: &'r windows::UI::Composition::Compositor,
    common: &'r UICommonObjects,
    text_format_stock: &'r mut TextFormatStock,
    text_surface_stock: &'r mut TextSurfaceStock,
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

pub trait InputEventHandler {
    fn on_pointer_enter(&self, view_ctx: &mut ViewContext) {}
    fn on_pointer_leave(&self, view_ctx: &mut ViewContext) {}
    fn on_click(&self, view_ctx: &mut ViewContext) {}
}
impl<T: InputEventHandler> InputEventHandler for std::rc::Rc<T> {
    #[inline(always)]
    fn on_pointer_enter(&self, view_ctx: &mut ViewContext) {
        T::on_pointer_enter(&*self, view_ctx)
    }

    #[inline(always)]
    fn on_pointer_leave(&self, view_ctx: &mut ViewContext) {
        T::on_pointer_leave(&*self, view_ctx)
    }

    #[inline(always)]
    fn on_click(&self, view_ctx: &mut ViewContext) {
        T::on_click(&*self, view_ctx)
    }
}
impl InputEventHandler for () {}

struct PaneGroupView {
    root: ContainerVisual,
    ht_ref: std::rc::Rc<core::cell::RefCell<HitTestTree>>,
    current_active: usize,
    tabs: Vec<std::rc::Rc<core::cell::RefCell<PaneTabHeaderView>>>,
}
impl PaneGroupView {
    pub fn new(
        ctx: &mut ViewContext,
        ht_parent: &mut HitTestTree,
        ht_ctx: &mut HitTestTreeContext,
    ) -> windows::core::Result<std::rc::Rc<core::cell::RefCell<Self>>> {
        let root = ctx.compositor.CreateContainerVisual()?;

        let ht = std::rc::Rc::new(core::cell::RefCell::new(HitTestTree::new_unsized(
            Box::new(()),
            ht_ctx.new_id(),
            0.0,
            0.0,
        )));
        ht_parent.add_child(&ht);

        Ok(std::rc::Rc::new(core::cell::RefCell::new(Self {
            root,
            ht_ref: ht,
            current_active: 0,
            tabs: Vec::new(),
        })))
    }

    pub fn add_tab(
        this: &std::rc::Rc<core::cell::RefCell<Self>>,
        title: impl Into<Cow<'static, str>>,
        ctx: &mut ViewContext,
        ht_ctx: &mut HitTestTreeContext,
    ) -> windows::core::Result<std::rc::Rc<core::cell::RefCell<PaneTabHeaderView>>> {
        let mut thisref = this.borrow_mut();
        let header_view = PaneTabHeaderView::new(
            this,
            thisref.tabs.len(),
            title,
            thisref.tabs.is_empty(),
            ctx,
            &mut thisref.ht_ref.borrow_mut(),
            ht_ctx,
        )?;
        thisref.tabs.push(header_view.clone());
        thisref
            .root
            .Children()?
            .InsertAtTop(&header_view.borrow().visual)?;

        Ok(header_view)
    }

    pub fn rearrange(&mut self) {
        let mut offset = 0.0;
        for v in self.tabs.iter() {
            v.borrow()
                .set_offset(offset, 0.0)
                .expect("Failed to set tab offset");
            offset += v.borrow().width;
        }
    }

    pub fn set_offset(&self, left: f32, top: f32) -> windows::core::Result<()> {
        self.root.SetOffset(Vector3 {
            X: left,
            Y: top,
            Z: 0.0,
        })?;
        self.ht_ref.borrow_mut().left = left;
        self.ht_ref.borrow_mut().top = top;

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
            .borrow_mut()
            .set_active(false, view_ctx)?;
        self.current_active = new_active;
        self.tabs[self.current_active]
            .borrow_mut()
            .set_active(true, view_ctx)?;

        Ok(())
    }
}

struct PaneTabHeaderView {
    group_view: std::rc::Weak<core::cell::RefCell<PaneGroupView>>,
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
        ht_parent: &mut HitTestTree,
        ht_ctx: &mut HitTestTreeContext,
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
                let ht_id = ht_ctx.new_id();
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
                ht_parent.add_child(&ht_self);

                core::cell::RefCell::new(Self {
                    group_view: std::rc::Rc::downgrade(group_view),
                    index_in_group,
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
        self.hittest_tree_self.borrow_mut().left = left;
        self.hittest_tree_self.borrow_mut().top = top;

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

const DRAG_THRESHOLD_DIST2: f32 = 5.0 * 5.0;
struct InputState {
    ht_tree: std::rc::Rc<core::cell::RefCell<HitTestTree>>,
    mouse_current_enter_element: Option<std::rc::Weak<core::cell::RefCell<HitTestTree>>>,
    mouse_down_point: Option<(f32, f32)>,
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
        let mut left = false;
        if let Some(x) = self
            .mouse_current_enter_element
            .as_ref()
            .and_then(std::rc::Weak::upgrade)
        {
            if Some(x.borrow().id) != over_tree.as_ref().map(|x| x.borrow().id) {
                // leave
                x.borrow().eh.on_pointer_leave(view_ctx);
                left = true;
            }
        }
        self.mouse_current_enter_element = over_tree.as_ref().map(std::rc::Rc::downgrade);
        if left {
            if let Some(x) = self
                .mouse_current_enter_element
                .as_ref()
                .and_then(std::rc::Weak::upgrade)
            {
                x.borrow().eh.on_pointer_enter(view_ctx);
            }
        }
    }

    fn on_mouse_move(&mut self, x: f32, y: f32, view_ctx: &mut ViewContext) {
        self.update_mouse_pos(x, y, view_ctx);

        if let Some((dx, dy)) = self.mouse_down_point {
            if !self.is_mouse_dragging {
                // 閾値を超えた後は永続的にドラッグ状態になる
                let dist2 = (dx - x).powi(2) + (dy - y).powi(2);
                if dist2 >= DRAG_THRESHOLD_DIST2 {
                    self.is_mouse_dragging = true;
                }
            }
        }
    }

    fn on_mouse_down(&mut self, x: f32, y: f32, view_ctx: &mut ViewContext) {
        self.update_mouse_pos(x, y, view_ctx);
        self.mouse_down_point = Some((x, y));
        self.is_mouse_dragging = false;
    }

    fn on_mouse_up(&mut self, x: f32, y: f32, view_ctx: &mut ViewContext) {
        self.update_mouse_pos(x, y, view_ctx);

        if !self.is_mouse_dragging {
            if let Some(x) = self
                .mouse_current_enter_element
                .as_ref()
                .and_then(std::rc::Weak::upgrade)
            {
                x.borrow().eh.on_click(view_ctx);
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

fn main() {
    let instance_handle = unsafe { GetModuleHandleA(None).expect("Failed to get instance handle") };
    let wndclass = WNDCLASSEXA {
        cbSize: core::mem::size_of::<WNDCLASSEXA>() as _,
        cbClsExtra: 0,
        cbWndExtra: core::mem::size_of::<[usize; 2]>() as _,
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
    let atom = unsafe { RegisterClassExA(&wndclass) };
    if atom == 0 {
        panic!("Failed to register window class");
    }

    let window_handle = unsafe {
        CreateWindowExA(
            WS_EX_APPWINDOW | WS_EX_NOREDIRECTIONBITMAP,
            PCSTR(atom as _),
            s!("Peridot Marble Editor"),
            WS_OVERLAPPEDWINDOW,
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            None,
            None,
            instance_handle,
            None,
        )
    };
    if window_handle.0 == 0 {
        panic!("Failed to create main window");
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

    let ui_font = text_format_stock
        .get("system-ui", 12.0, DWRITE_FONT_WEIGHT_NORMAL)
        .expect("Failed to create default ui format");
    let title_text_surface = text_surface_stock
        .get(&ui_font, "New Project - Peridot Marble Editor v0.1.0")
        .expect("Failed to create title text surface");

    let title_label = compositor
        .CreateSpriteVisual()
        .expect("Failed to create title label visual");
    let title_label_brush = compositor
        .CreateSurfaceBrushWithSurface(&title_text_surface.surface)
        .expect("Failed to create surface brush");
    title_label
        .SetBrush(&title_label_brush)
        .expect("Failed to set surface brush");
    title_label
        .SetSize(Vector2 {
            X: title_text_surface.width as _,
            Y: title_text_surface.height as _,
        })
        .expect("Failed to set title label size");
    title_label
        .SetOffset(Vector3 {
            X: 28.0,
            Y: 8.0,
            Z: 0.0,
        })
        .expect("Failed to set title label offset");
    composition_root
        .Children()
        .expect("Failed to get children collection")
        .InsertAtTop(&title_label)
        .expect("Failed to insert title label visual");

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

    let mut view_context = ViewContext {
        compositor: &compositor,
        common: &common_objects,
        text_format_stock: &mut text_format_stock,
        text_surface_stock: &mut text_surface_stock,
    };

    let hittest_tree_root = std::rc::Rc::new(core::cell::RefCell::new(HitTestTree::new_unsized(
        Box::new(()),
        0,
        0.0,
        0.0,
    )));
    let mut hittest_context = HitTestTreeContext::new();

    let pane_group1 = PaneGroupView::new(
        &mut view_context,
        &mut hittest_tree_root.borrow_mut(),
        &mut hittest_context,
    )
    .expect("Failed to create PaneGroupView");
    PaneGroupView::add_tab(
        &pane_group1,
        "Inspector",
        &mut view_context,
        &mut hittest_context,
    )
    .expect("Failed to create InspectorPaneTabHeader");
    PaneGroupView::add_tab(
        &pane_group1,
        "Project Settings",
        &mut view_context,
        &mut hittest_context,
    )
    .expect("Failed to create ProjectSettingsPaneTabHeader");
    PaneGroupView::add_tab(
        &pane_group1,
        "Timeline",
        &mut view_context,
        &mut hittest_context,
    )
    .expect("Failed to create SceneViewPaneTabHeader");
    pane_group1.borrow_mut().rearrange();
    pane_group1
        .borrow()
        .set_offset(100.0, 100.0)
        .expect("Failed to set page group offset");
    composition_root
        .Children()
        .expect("Failed to get children collection")
        .InsertAtTop(&pane_group1.borrow().root)
        .expect("Failed to insert inspector pane visual");

    let mut input_state = InputState::new(&hittest_tree_root);
    unsafe {
        SetWindowLongPtrA(
            window_handle,
            WINDOW_LONG_PTR_INDEX(0),
            &mut input_state as *mut _ as _,
        );
        SetWindowLongPtrA(
            window_handle,
            WINDOW_LONG_PTR_INDEX(core::mem::size_of::<usize>() as _),
            &mut view_context as *mut _ as _,
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
            (GetWindowLongPtrA(hwnd, WINDOW_LONG_PTR_INDEX(0)) as *mut InputState).as_mut()
        }) else {
            return LRESULT(0);
        };
        let Some(view_context) = (unsafe {
            (GetWindowLongPtrA(
                hwnd,
                WINDOW_LONG_PTR_INDEX(core::mem::size_of::<usize>() as _),
            ) as *mut ViewContext)
                .as_mut()
        }) else {
            return LRESULT(0);
        };

        let dpi = unsafe { GetDpiForWindow(hwnd) as f32 };
        let (x, y) = ((lp.0 & 0xffff) as i16, ((lp.0 >> 16) & 0xffff) as i16);
        state.on_mouse_move(x as f32 * 96.0 / dpi, y as f32 * 96.0 / dpi, view_context);

        return LRESULT(0);
    }
    if msg == WM_LBUTTONDOWN {
        let Some(state) = (unsafe {
            (GetWindowLongPtrA(hwnd, WINDOW_LONG_PTR_INDEX(0)) as *mut InputState).as_mut()
        }) else {
            return LRESULT(0);
        };
        let Some(view_context) = (unsafe {
            (GetWindowLongPtrA(
                hwnd,
                WINDOW_LONG_PTR_INDEX(core::mem::size_of::<usize>() as _),
            ) as *mut ViewContext)
                .as_mut()
        }) else {
            return LRESULT(0);
        };

        let dpi = unsafe { GetDpiForWindow(hwnd) as f32 };
        let (x, y) = ((lp.0 & 0xffff) as i16, ((lp.0 >> 16) & 0xffff) as i16);
        state.on_mouse_down(x as f32 * 96.0 / dpi, y as f32 * 96.0 / dpi, view_context);

        return LRESULT(0);
    }
    if msg == WM_LBUTTONUP {
        let Some(state) = (unsafe {
            (GetWindowLongPtrA(hwnd, WINDOW_LONG_PTR_INDEX(0)) as *mut InputState).as_mut()
        }) else {
            return LRESULT(0);
        };
        let Some(view_context) = (unsafe {
            (GetWindowLongPtrA(
                hwnd,
                WINDOW_LONG_PTR_INDEX(core::mem::size_of::<usize>() as _),
            ) as *mut ViewContext)
                .as_mut()
        }) else {
            return LRESULT(0);
        };

        let dpi = unsafe { GetDpiForWindow(hwnd) as f32 };
        let (x, y) = ((lp.0 & 0xffff) as i16, ((lp.0 >> 16) & 0xffff) as i16);
        state.on_mouse_up(x as f32 * 96.0 / dpi, y as f32 * 96.0 / dpi, view_context);

        return LRESULT(0);
    }

    unsafe { DefWindowProcA(hwnd, msg, wp, lp) }
}
