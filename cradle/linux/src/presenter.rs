use std::os::fd::BorrowedFd;

pub mod wayland;
pub mod xcb;

pub trait WindowBackend {
    fn show(&mut self);
    fn geometry(&self) -> peridot::math::Vector2<usize>;
}

pub trait BorrowFd {
    fn borrow_fd<'fd>(&'fd self) -> BorrowedFd<'fd>;
}

pub trait EventProcessor {
    type ReadinessGuard: BorrowFd;

    fn readiness_guard(&mut self) -> Self::ReadinessGuard;
    fn process_all_events(&mut self, readiness_guard: Self::ReadinessGuard);
    fn has_close_requested(&self) -> bool;
}

pub trait PresenterProvider {
    type Presenter: peridot::PlatformPresenter;
    const SURFACE_EXT_NAME: &'static str;

    fn create(&self, g: &peridot::Graphics) -> Self::Presenter;
}
