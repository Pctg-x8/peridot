use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
};

use windows::Win32::{
    Foundation::{HANDLE, WAIT_OBJECT_0},
    System::Threading::INFINITE,
    UI::WindowsAndMessaging::{MsgWaitForMultipleObjects, QS_ALLEVENTS},
};

use crate::uikit::ViewContext;

pub trait SignalEventReceiver {
    fn on_signal(&self, arg: usize, view_ctx: &dyn ViewContext);
}

pub enum SignalEventType {
    Receiver(Rc<dyn SignalEventReceiver>, usize),
    Message,
    Unknown,
}

static mut APP_GLOBAL_SIGNALS: *mut RefCell<AppGlobalSignals> = core::ptr::null_mut();

pub struct AppGlobalSignalsFinalizer;
impl Drop for AppGlobalSignalsFinalizer {
    fn drop(&mut self) {
        AppGlobalSignals::finalize();
    }
}

pub struct AppGlobalSignals {
    pub entries: Vec<(Rc<dyn SignalEventReceiver>, usize)>,
    pub raw_events: Vec<HANDLE>,
}
impl AppGlobalSignals {
    pub fn initialize() -> AppGlobalSignalsFinalizer {
        unsafe {
            APP_GLOBAL_SIGNALS = Box::into_raw(Box::new(RefCell::new(Self::new())));
        }

        AppGlobalSignalsFinalizer
    }

    pub fn finalize() {
        unsafe {
            if APP_GLOBAL_SIGNALS.is_null() {
                return;
            }

            drop(Box::from_raw(core::mem::replace(
                &mut *core::ptr::addr_of_mut!(APP_GLOBAL_SIGNALS),
                core::ptr::null_mut(),
            )));
        }
    }

    pub fn get<'a>() -> Ref<'a, Self> {
        unsafe { (&*APP_GLOBAL_SIGNALS).borrow() }
    }

    pub fn get_mut<'a>() -> RefMut<'a, Self> {
        unsafe { (&*APP_GLOBAL_SIGNALS).borrow_mut() }
    }

    fn new() -> Self {
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
