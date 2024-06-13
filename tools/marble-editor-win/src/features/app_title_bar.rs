use std::rc::Rc;

use windows::{
    core::h,
    Foundation::{
        Numerics::{Vector2, Vector3},
        Rect,
    },
    Graphics::IGeometrySource2D,
    Win32::{
        Graphics::{
            Direct2D::Common::{
                D2D1_FIGURE_BEGIN_HOLLOW, D2D1_FIGURE_END_CLOSED, D2D1_FIGURE_END_OPEN,
                D2D_POINT_2F,
            },
            DirectWrite::{
                DWRITE_FONT_WEIGHT_NORMAL, DWRITE_FONT_WEIGHT_SEMI_BOLD, DWRITE_TEXT_RANGE,
            },
        },
        UI::WindowsAndMessaging::{HTCAPTION, HTCLOSE, HTMAXBUTTON, HTMINBUTTON, HTNOWHERE},
    },
    UI::{
        Color, Colors,
        Composition::{
            CompositionEffectSourceParameter, CompositionPath, ContainerVisual,
            ScalarKeyFrameAnimation, ShapeVisual, SpriteVisual, VisualCollection,
        },
    },
};

use crate::{
    bindgen::Graphics::Canvas::Effects::{EffectOptimization, GaussianBlurEffect},
    new_cyclic_shared_mut,
    uikit::{HitTestTree, InputContext, InputEventHandler, ViewContext},
    utils::RectExtensions,
    winapi_extras::{
        timespan_ms, GeometryInterop, KeyFrameAnimationExtension,
        KeyFrameAnimationPropertySetterExtension, VectorScalarConstructor, VisualExtensions,
    },
    SharedMut, WeakMut,
};

#[derive(Clone, Copy, PartialEq, Eq)]
enum AppTitleBarControlButtonType {
    Close,
    MaximizeRestore,
    Minimize,
}
struct AppTitleBarControlButtonView {
    ty: AppTitleBarControlButtonType,
    nth: usize,
    root: ContainerVisual,
    bg: SpriteVisual,
    icon: ShapeVisual,
    hover_animation: ScalarKeyFrameAnimation,
    hover_end_animation: ScalarKeyFrameAnimation,
    ht: SharedMut<HitTestTree>,
}
impl AppTitleBarControlButtonView {
    fn new(
        ctx: &mut (impl ViewContext + ?Sized),
        ty: AppTitleBarControlButtonType,
        nth: usize,
    ) -> windows::core::Result<SharedMut<Self>> {
        let root = ctx
            .app_subsystems()
            .borrow()
            .compositor
            .CreateContainerVisual()?;
        root.set_properties()
            .size(Vector2 {
                X: AppTitleBarView::BUTTON_WIDTH,
                Y: AppTitleBarView::HEIGHT,
            })?
            .anchor_point(Vector2::unit_x())?
            .offset(Vector3 {
                X: -(nth as f32 * AppTitleBarView::BUTTON_WIDTH),
                Y: 0.0,
                Z: 0.0,
            })?
            .relative_offset_adjustment(Vector3 {
                X: 1.0,
                Y: 0.0,
                Z: 0.0,
            })?;
        let bg = ctx
            .app_subsystems()
            .borrow()
            .compositor
            .CreateSpriteVisual()?;
        bg.set_properties()
            .brush(
                &ctx.app_subsystems()
                    .borrow()
                    .compositor
                    .CreateColorBrushWithColor(if ty == AppTitleBarControlButtonType::Close {
                        Colors::Red()?
                    } else {
                        Color {
                            A: 64,
                            R: 255,
                            G: 255,
                            B: 255,
                        }
                    })?,
            )?
            .expand_to_parent()?
            .opacity(0.0)?;
        let linear_fn = ctx
            .app_subsystems()
            .borrow()
            .compositor
            .CreateLinearEasingFunction()?;
        let hover_animation = ctx
            .app_subsystems()
            .borrow()
            .compositor
            .CreateScalarKeyFrameAnimation()?;
        hover_animation
            .keyframe(0.0, 0.0)?
            .interpolate(1.0, 1.0, &linear_fn)?
            .set_properties()
            .duration(timespan_ms(100))?;
        let hover_end_animation = ctx
            .app_subsystems()
            .borrow()
            .compositor
            .CreateScalarKeyFrameAnimation()?;
        hover_end_animation
            .keyframe(0.0, 1.0)?
            .interpolate(1.0, 0.0, &linear_fn)?
            .set_properties()
            .duration(timespan_ms(100))?;
        root.Children()?.InsertAtTop(&bg)?;

        let icon_geometry = unsafe {
            ctx.app_subsystems()
                .borrow()
                .d2d1_factory
                .CreatePathGeometry()?
        };
        let sink = unsafe { icon_geometry.Open()? };
        match ty {
            AppTitleBarControlButtonType::Close => unsafe {
                sink.BeginFigure(D2D_POINT_2F { x: 0.0, y: 0.0 }, D2D1_FIGURE_BEGIN_HOLLOW);
                sink.AddLine(D2D_POINT_2F {
                    x: AppTitleBarView::BUTTON_ICON_SIZE,
                    y: AppTitleBarView::BUTTON_ICON_SIZE,
                });
                sink.EndFigure(D2D1_FIGURE_END_OPEN);
                sink.BeginFigure(
                    D2D_POINT_2F {
                        x: AppTitleBarView::BUTTON_ICON_SIZE,
                        y: 0.0,
                    },
                    D2D1_FIGURE_BEGIN_HOLLOW,
                );
                sink.AddLine(D2D_POINT_2F {
                    x: 0.0,
                    y: AppTitleBarView::BUTTON_ICON_SIZE,
                });
                sink.EndFigure(D2D1_FIGURE_END_OPEN);
            },
            AppTitleBarControlButtonType::Minimize => unsafe {
                sink.BeginFigure(
                    D2D_POINT_2F {
                        x: 0.0,
                        y: AppTitleBarView::BUTTON_ICON_SIZE * 0.9,
                    },
                    D2D1_FIGURE_BEGIN_HOLLOW,
                );
                sink.AddLine(D2D_POINT_2F {
                    x: AppTitleBarView::BUTTON_ICON_SIZE,
                    y: AppTitleBarView::BUTTON_ICON_SIZE * 0.9,
                });
                sink.EndFigure(D2D1_FIGURE_END_OPEN);
            },
            AppTitleBarControlButtonType::MaximizeRestore => unsafe {
                sink.BeginFigure(
                    D2D_POINT_2F {
                        x: AppTitleBarView::BUTTON_ICON_SIZE * 0.1,
                        y: AppTitleBarView::BUTTON_ICON_SIZE * 0.1,
                    },
                    D2D1_FIGURE_BEGIN_HOLLOW,
                );
                sink.AddLines(&[
                    D2D_POINT_2F {
                        x: AppTitleBarView::BUTTON_ICON_SIZE * 0.9,
                        y: AppTitleBarView::BUTTON_ICON_SIZE * 0.1,
                    },
                    D2D_POINT_2F {
                        x: AppTitleBarView::BUTTON_ICON_SIZE * 0.9,
                        y: AppTitleBarView::BUTTON_ICON_SIZE * 0.9,
                    },
                    D2D_POINT_2F {
                        x: AppTitleBarView::BUTTON_ICON_SIZE * 0.1,
                        y: AppTitleBarView::BUTTON_ICON_SIZE * 0.9,
                    },
                ]);
                sink.EndFigure(D2D1_FIGURE_END_CLOSED);
            },
        }
        unsafe {
            sink.Close()?;
        }
        let icon_geometry: IGeometrySource2D = GeometryInterop(icon_geometry.into()).into();
        let icon_geometry = ctx
            .app_subsystems()
            .borrow()
            .compositor
            .CreatePathGeometryWithPath(&CompositionPath::Create(&icon_geometry)?)?;
        let icon_shape = ctx
            .app_subsystems()
            .borrow()
            .compositor
            .CreateSpriteShapeWithGeometry(&icon_geometry)?;
        icon_shape.SetStrokeBrush(
            &ctx.app_subsystems()
                .borrow()
                .compositor
                .CreateColorBrushWithColor(Colors::White()?)?,
        )?;
        icon_shape.SetStrokeThickness(1.5)?;
        let icon = ctx
            .app_subsystems()
            .borrow()
            .compositor
            .CreateShapeVisual()?;
        icon.Shapes()?.Append(&icon_shape)?;
        icon.set_properties()
            .size(Vector2::scalar(AppTitleBarView::BUTTON_ICON_SIZE))?
            .anchor_point(Vector2::scalar(0.5))?
            .relative_offset_adjustment(Vector3::scalar(0.5))?;
        root.Children()?.InsertAtTop(&icon)?;

        Ok(new_cyclic_shared_mut(|wthis| {
            let ht = HitTestTree::new(
                Some(&Rc::new(wthis.clone())),
                ctx.hittest_context_mut().new_id(),
                Rect::from_size(AppTitleBarView::BUTTON_WIDTH, AppTitleBarView::HEIGHT),
            );

            Self {
                ty,
                nth,
                root,
                bg,
                icon,
                hover_animation,
                hover_end_animation,
                ht,
            }
        }))
    }

    pub fn mount(
        &self,
        onto: &VisualCollection,
        onto_ht: &SharedMut<HitTestTree>,
    ) -> windows::core::Result<()> {
        onto.InsertAtTop(&self.root)?;
        HitTestTree::add_child(onto_ht, self.ht.clone());

        Ok(())
    }

    pub fn adjust_left(&self, parent_width: f32) {
        self.ht
            .borrow_mut()
            .set_right(parent_width - (self.nth as f32 * AppTitleBarView::BUTTON_WIDTH));
    }

    pub fn change_maximize_restore_icon(
        &self,
        is_maximized: bool,
        view_ctx: &mut (impl ViewContext + ?Sized),
    ) -> windows::core::Result<()> {
        if self.ty != AppTitleBarControlButtonType::MaximizeRestore {
            // Maximize/Restore以外のボタンはアイコン変更なし
            return Ok(());
        }

        let icon_geometry = unsafe {
            view_ctx
                .app_subsystems()
                .borrow()
                .d2d1_factory
                .CreatePathGeometry()?
        };
        let sink = unsafe { icon_geometry.Open()? };
        if !is_maximized {
            unsafe {
                sink.BeginFigure(
                    D2D_POINT_2F {
                        x: AppTitleBarView::BUTTON_ICON_SIZE * 0.1,
                        y: AppTitleBarView::BUTTON_ICON_SIZE * 0.1,
                    },
                    D2D1_FIGURE_BEGIN_HOLLOW,
                );
                sink.AddLines(&[
                    D2D_POINT_2F {
                        x: AppTitleBarView::BUTTON_ICON_SIZE * 0.9,
                        y: AppTitleBarView::BUTTON_ICON_SIZE * 0.1,
                    },
                    D2D_POINT_2F {
                        x: AppTitleBarView::BUTTON_ICON_SIZE * 0.9,
                        y: AppTitleBarView::BUTTON_ICON_SIZE * 0.9,
                    },
                    D2D_POINT_2F {
                        x: AppTitleBarView::BUTTON_ICON_SIZE * 0.1,
                        y: AppTitleBarView::BUTTON_ICON_SIZE * 0.9,
                    },
                ]);
                sink.EndFigure(D2D1_FIGURE_END_CLOSED);
            }
        } else {
            unsafe {
                sink.BeginFigure(
                    D2D_POINT_2F {
                        x: AppTitleBarView::BUTTON_ICON_SIZE * 0.1,
                        y: AppTitleBarView::BUTTON_ICON_SIZE * 0.3,
                    },
                    D2D1_FIGURE_BEGIN_HOLLOW,
                );
                sink.AddLines(&[
                    D2D_POINT_2F {
                        x: AppTitleBarView::BUTTON_ICON_SIZE * 0.7,
                        y: AppTitleBarView::BUTTON_ICON_SIZE * 0.3,
                    },
                    D2D_POINT_2F {
                        x: AppTitleBarView::BUTTON_ICON_SIZE * 0.7,
                        y: AppTitleBarView::BUTTON_ICON_SIZE * 0.9,
                    },
                    D2D_POINT_2F {
                        x: AppTitleBarView::BUTTON_ICON_SIZE * 0.1,
                        y: AppTitleBarView::BUTTON_ICON_SIZE * 0.9,
                    },
                ]);
                sink.EndFigure(D2D1_FIGURE_END_CLOSED);
                sink.BeginFigure(
                    D2D_POINT_2F {
                        x: AppTitleBarView::BUTTON_ICON_SIZE * 0.3,
                        y: AppTitleBarView::BUTTON_ICON_SIZE * 0.2,
                    },
                    D2D1_FIGURE_BEGIN_HOLLOW,
                );
                sink.AddLines(&[
                    D2D_POINT_2F {
                        x: AppTitleBarView::BUTTON_ICON_SIZE * 0.3,
                        y: AppTitleBarView::BUTTON_ICON_SIZE * 0.1,
                    },
                    D2D_POINT_2F {
                        x: AppTitleBarView::BUTTON_ICON_SIZE * 0.9,
                        y: AppTitleBarView::BUTTON_ICON_SIZE * 0.1,
                    },
                    D2D_POINT_2F {
                        x: AppTitleBarView::BUTTON_ICON_SIZE * 0.9,
                        y: AppTitleBarView::BUTTON_ICON_SIZE * 0.7,
                    },
                    D2D_POINT_2F {
                        x: AppTitleBarView::BUTTON_ICON_SIZE * 0.8,
                        y: AppTitleBarView::BUTTON_ICON_SIZE * 0.7,
                    },
                ]);
                sink.EndFigure(D2D1_FIGURE_END_OPEN);
            }
        }
        unsafe {
            sink.Close()?;
        }
        let icon_geometry: IGeometrySource2D = GeometryInterop(icon_geometry.into()).into();
        let icon_geometry = view_ctx
            .app_subsystems()
            .borrow()
            .compositor
            .CreatePathGeometryWithPath(&CompositionPath::Create(&icon_geometry)?)?;
        let icon_shape = view_ctx
            .app_subsystems()
            .borrow()
            .compositor
            .CreateSpriteShapeWithGeometry(&icon_geometry)?;
        icon_shape.SetStrokeBrush(
            &view_ctx
                .app_subsystems()
                .borrow()
                .compositor
                .CreateColorBrushWithColor(Colors::White()?)?,
        )?;
        icon_shape.SetStrokeThickness(1.5)?;
        self.icon.Shapes()?.SetAt(0, &icon_shape)?;

        Ok(())
    }
}
impl InputEventHandler for WeakMut<AppTitleBarControlButtonView> {
    fn nc_hittest(&self) -> u32 {
        let Some(this) = self.upgrade() else {
            return HTNOWHERE;
        };
        let ty = this.borrow().ty;

        match ty {
            AppTitleBarControlButtonType::Close => HTCLOSE,
            AppTitleBarControlButtonType::MaximizeRestore => HTMAXBUTTON,
            AppTitleBarControlButtonType::Minimize => HTMINBUTTON,
        }
    }

    fn on_pointer_enter(&self, _ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };

        this.borrow()
            .bg
            .StartAnimation(h!("Opacity"), &this.borrow().hover_animation)
            .expect("Failed to start hover animation");
    }

    fn on_pointer_leave(&self, _ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };

        this.borrow()
            .bg
            .StartAnimation(h!("Opacity"), &this.borrow().hover_end_animation)
            .expect("Failed to start hover end animation");
    }
}

pub struct AppTitleBarView {
    root: ContainerVisual,
    close_button: SharedMut<AppTitleBarControlButtonView>,
    maxres_button: SharedMut<AppTitleBarControlButtonView>,
    min_button: SharedMut<AppTitleBarControlButtonView>,
    ht: SharedMut<HitTestTree>,
}
impl AppTitleBarView {
    pub const HEIGHT: f32 = 32.0;
    const BUTTON_WIDTH: f32 = 40.0;
    const BUTTON_ICON_SIZE: f32 = 10.0;

    pub fn new(
        ctx: &mut (impl ViewContext + ?Sized),
        init_dpi: f32,
        global_scale: f64,
    ) -> windows::core::Result<SharedMut<Self>> {
        let root = ctx
            .app_subsystems()
            .borrow()
            .compositor
            .CreateContainerVisual()?;
        root.set_properties()
            .size(Vector2 {
                X: 0.0,
                Y: Self::HEIGHT,
            })?
            .relative_size_adjustment(Vector2 {
                // Note: グローバルスケールがかかるのでその分縮める
                X: global_scale.recip() as _,
                Y: 0.0,
            })?;

        let title_font = ctx.app_subsystems().borrow_mut().text_format_stock.get(
            "system-ui",
            10.0,
            DWRITE_FONT_WEIGHT_NORMAL,
        )?;
        let title_text = "New Project Peridot Marble Editor 0.1.0";
        let title_layout = unsafe {
            ctx.app_subsystems()
                .borrow()
                .dwrite_factory
                .CreateTextLayout(
                    &title_text.encode_utf16().collect::<Vec<_>>(),
                    &title_font,
                    f32::MAX,
                    f32::MAX,
                )?
        };
        unsafe {
            let project_name_range = DWRITE_TEXT_RANGE {
                startPosition: 0,
                length: "New Project".len() as _,
            };

            title_layout.SetFontWeight(DWRITE_FONT_WEIGHT_SEMI_BOLD, project_name_range)?;
            title_layout.SetFontSize(12.0, project_name_range)?;
        }
        let title_text = ctx
            .app_subsystems()
            .borrow_mut()
            .text_surface_stock
            .create_text_surface(&title_layout, init_dpi)?;
        let title = ctx
            .app_subsystems()
            .borrow()
            .compositor
            .CreateSpriteVisual()?;
        title
            .set_properties()
            .anchor_point(Vector2::scalar(0.5))?
            .center_point(Vector3::scalar(0.0))?
            .size(title_text.visual_size())?
            .relative_offset_adjustment(Vector3::scalar(0.5))?
            .brush(
                &ctx.app_subsystems()
                    .borrow()
                    .compositor
                    .CreateSurfaceBrushWithSurface(&title_text.surface)?,
            )?;

        let fx = GaussianBlurEffect::new()?;
        fx.SetSource(&CompositionEffectSourceParameter::Create(h!("source"))?)?;
        fx.SetBlurAmount(2.0)?;
        fx.SetOptimization(EffectOptimization::Balanced)?;
        let effect_factory = ctx
            .app_subsystems()
            .borrow()
            .compositor
            .CreateEffectFactory(&fx)?;
        let blur_brush = effect_factory.CreateBrush()?;
        blur_brush.SetSourceParameter(
            h!("source"),
            &ctx.app_subsystems()
                .borrow()
                .compositor
                .CreateSurfaceBrushWithSurface(&title_text.surface)?,
        )?;
        let title_fx = ctx
            .app_subsystems()
            .borrow()
            .compositor
            .CreateSpriteVisual()?;
        title_fx
            .set_properties()
            .anchor_point(Vector2::scalar(0.5))?
            .center_point(Vector3::scalar(0.0))?
            .size(title_text.visual_size())?
            .relative_offset_adjustment(Vector3::scalar(0.5))?
            .brush(&blur_brush)?;

        let close_button =
            AppTitleBarControlButtonView::new(ctx, AppTitleBarControlButtonType::Close, 0)?;
        let maxres_button = AppTitleBarControlButtonView::new(
            ctx,
            AppTitleBarControlButtonType::MaximizeRestore,
            1,
        )?;
        let min_button =
            AppTitleBarControlButtonView::new(ctx, AppTitleBarControlButtonType::Minimize, 2)?;

        let root_children = root.Children()?;
        root_children.InsertAtTop(&title_fx)?;
        root_children.InsertAtTop(&title)?;

        let this = new_cyclic_shared_mut(|wthis| {
            let ht = HitTestTree::new(
                Some(&Rc::new(wthis.clone())),
                ctx.hittest_context_mut().new_id(),
                Rect::from_size(128.0, Self::HEIGHT),
            );

            Self {
                root,
                close_button,
                maxres_button,
                min_button,
                ht,
            }
        });

        // mount children views
        this.borrow()
            .close_button
            .borrow()
            .mount(&root_children, &this.borrow().ht)?;
        this.borrow()
            .maxres_button
            .borrow()
            .mount(&root_children, &this.borrow().ht)?;
        this.borrow()
            .min_button
            .borrow()
            .mount(&root_children, &this.borrow().ht)?;

        Ok(this)
    }

    pub fn set_width(&self, width: f32) {
        self.ht.borrow_mut().set_width(width);
        self.close_button.borrow().adjust_left(width);
        self.maxres_button.borrow().adjust_left(width);
        self.min_button.borrow().adjust_left(width);
    }

    pub fn mount(
        &self,
        onto: &VisualCollection,
        onto_ht: &SharedMut<HitTestTree>,
    ) -> windows::core::Result<()> {
        onto.InsertAtTop(&self.root)?;
        HitTestTree::add_child(onto_ht, self.ht.clone());

        Ok(())
    }

    pub fn change_maximize_restore_icon(
        &self,
        is_maximized: bool,
        view_ctx: &mut (impl ViewContext + ?Sized),
    ) -> windows::core::Result<()> {
        self.maxres_button
            .borrow()
            .change_maximize_restore_icon(is_maximized, view_ctx)
    }
}
impl InputEventHandler for WeakMut<AppTitleBarView> {
    fn nc_hittest(&self) -> u32 {
        HTCAPTION
    }
}
