use bedrock as br;

pub trait AwaitableFence {
    fn is_ready(&self) -> br::Result<bool>;
}

pub struct FenceReactorThread {
    pending_fences: std::sync::Arc<
        parking_lot::Mutex<
            Vec<(
                std::task::Waker,
                std::sync::Weak<dyn AwaitableFence + Send + Sync>,
            )>,
        >,
    >,
    shutdown: std::sync::Arc<std::sync::atomic::AtomicBool>,
    thread_handle: Option<std::thread::JoinHandle<()>>,
    thread_waker: std::sync::Arc<parking_lot::Condvar>,
}
impl FenceReactorThread {
    pub fn new() -> Self {
        let pending_fences = std::sync::Arc::new(parking_lot::Mutex::new(Vec::new()));
        let shutdown = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
        let thread_waker = std::sync::Arc::new(parking_lot::Condvar::new());

        let pf2 = pending_fences.clone();
        let s2 = shutdown.clone();
        let tw2 = thread_waker.clone();
        let thread_handle = std::thread::Builder::new()
            .name(String::from("Peridot Fence Reactor"))
            .spawn(move || {
                let mut managed_fences = Vec::<(
                    std::task::Waker,
                    std::sync::Weak<dyn AwaitableFence + Send + Sync>,
                )>::new();
                let mut signaled_indexes = Vec::new();

                loop {
                    {
                        let mut pf = pf2.lock();
                        if managed_fences.is_empty() {
                            tw2.wait(&mut pf);
                        }

                        if s2.load(std::sync::atomic::Ordering::Acquire) {
                            break;
                        }

                        managed_fences.extend(pf.drain(..));
                    }

                    if !managed_fences.is_empty() {
                        for (n, (_, f)) in managed_fences.iter().enumerate().rev() {
                            if let Some(f) = f.upgrade() {
                                if f.is_ready().expect("Failed to get fence status") {
                                    signaled_indexes.push((n, true));
                                }
                            } else {
                                // observing fence was dropped externally
                                signaled_indexes.push((n, false));
                            }
                        }
                        // signaled_indexes is sorted larger to smaller
                        for (dx, wake) in signaled_indexes.drain(..) {
                            let (wk, _) = managed_fences.remove(dx);
                            if wake {
                                wk.wake();
                            }
                        }
                    }
                }
            })
            .expect("Failed to spawn Fence Reactor Thread");

        Self {
            pending_fences,
            shutdown,
            thread_handle: Some(thread_handle),
            thread_waker,
        }
    }

    pub fn register(
        &self,
        fence: &std::sync::Arc<dyn AwaitableFence + Send + Sync>,
        waker: std::task::Waker,
    ) {
        self.pending_fences
            .lock()
            .push((waker, std::sync::Arc::downgrade(fence)));
        self.thread_waker.notify_all();
    }
}
impl Drop for FenceReactorThread {
    fn drop(&mut self) {
        if let Some(th) = self.thread_handle.take() {
            self.shutdown
                .store(true, std::sync::atomic::Ordering::Release);
            self.thread_waker.notify_all();
            th.join().expect("Joining Fence Reactor Thread failed");
        }
    }
}

pub(crate) struct FenceWaitFuture<'d> {
    pub(crate) reactor: &'d FenceReactorThread,
    pub(crate) object: std::sync::Arc<dyn AwaitableFence + Send + Sync>,
    pub(crate) registered: bool,
}
impl std::future::Future for FenceWaitFuture<'_> {
    type Output = br::Result<()>;

    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        match self.object.is_ready() {
            Err(e) => std::task::Poll::Ready(Err(e)),
            Ok(true) => std::task::Poll::Ready(Ok(())),
            Ok(false) => {
                if !self.registered {
                    self.reactor.register(&self.object, cx.waker().clone());
                    self.get_mut().registered = true;
                }

                std::task::Poll::Pending
            }
        }
    }
}
