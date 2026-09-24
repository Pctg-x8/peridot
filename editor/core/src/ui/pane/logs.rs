use crate::{
    ui::dock::PaneContentPresenter,
    uicore::{TypedViewIdentifier, ViewInitContext, ViewRegisterable},
    uikit::{ContainerView, ContainerViewInit},
};

pub struct Presenter {
    root_view: TypedViewIdentifier<ContainerView>,
}
impl Presenter {
    pub const ID: &str = internal_pane_identifier!("Logs");

    pub fn new(ctx: &mut ViewInitContext) -> Self {
        let root_view = ctx.construct_view(ContainerViewInit, |_| []);

        Self { root_view }
    }
}
impl PaneContentPresenter for Presenter {
    fn id(&self) -> String {
        Self::ID.into()
    }

    fn name(&self) -> String {
        "Logs".into()
    }

    fn root_view_id(&self) -> crate::uicore::ViewIdentifier {
        self.root_view.into_untyped()
    }
}
