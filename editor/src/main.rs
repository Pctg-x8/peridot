use core::pin::Pin;
use windows::{
    Win32::{
        Foundation::{HINSTANCE, HWND, LPARAM, LRESULT, WPARAM},
        Graphics::Gdi::HBRUSH,
        System::LibraryLoader::GetModuleHandleW,
        UI::WindowsAndMessaging::{
            CW_USEDEFAULT, CreateWindowExW, DefWindowProcW, DispatchMessageW, GetMessageW,
            GetWindowLongPtrW, HCURSOR, IDI_APPLICATION, LoadIconW, PostQuitMessage,
            RegisterClassExW, SW_SHOWNORMAL, SetWindowLongPtrW, ShowWindow, WINDOW_LONG_PTR_INDEX,
            WM_DESTROY, WNDCLASS_STYLES, WNDCLASSEXW, WS_EX_APPWINDOW, WS_OVERLAPPEDWINDOW,
        },
    },
    core::{BOOL, PCWSTR, w},
};

static APP_WAKER_VTABLE: core::task::RawWakerVTable = core::task::RawWakerVTable::new(
    |data| core::task::RawWaker::new(data, &APP_WAKER_VTABLE),
    |_| {},
    |_| {},
    |_| {},
);

fn main() {
    let mut event_store = core::pin::pin!(None::<u32>);
    let mut app = core::pin::pin!(run(EventQueue {
        event_store: event_store.as_mut().get_mut(),
    }));
    main_wrapper(app.as_mut(), event_store);
}

fn main_wrapper<AppFuture: core::future::Future<Output = ()>>(
    mut app: Pin<&mut AppFuture>,
    mut event_store: Pin<&mut Option<u32>>,
) {
    let _ = app
        .as_mut()
        .poll(&mut core::task::Context::from_waker(&unsafe {
            core::task::Waker::new(&(), &APP_WAKER_VTABLE)
        }));

    let hinstance: HINSTANCE = unsafe { GetModuleHandleW(None).expect("GetModuleHandleW").into() };
    let atom = unsafe {
        RegisterClassExW(&WNDCLASSEXW {
            cbSize: core::mem::size_of::<WNDCLASSEXW>() as _,
            style: WNDCLASS_STYLES(0),
            cbClsExtra: 0,
            cbWndExtra: core::mem::size_of::<[usize; 2]>() as _,
            lpfnWndProc: Some(wndproc::<AppFuture>),
            hInstance: hinstance,
            hIcon: LoadIconW(None, IDI_APPLICATION).expect("LoadIconW"),
            hCursor: HCURSOR(core::ptr::null_mut()),
            hbrBackground: HBRUSH(core::ptr::null_mut()),
            lpszMenuName: PCWSTR::null(),
            lpszClassName: w!("MainWindow"),
            hIconSm: LoadIconW(None, IDI_APPLICATION).expect("LoadIconW"),
        })
    };
    if atom == 0 {
        Err::<(), _>(std::io::Error::last_os_error()).expect("RegisterClassExW");
    }

    let w = unsafe {
        CreateWindowExW(
            WS_EX_APPWINDOW,
            PCWSTR(core::ptr::without_provenance(atom as _)),
            w!("Peridot Marble Editor"),
            WS_OVERLAPPEDWINDOW,
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            None,
            None,
            Some(hinstance),
            None,
        )
        .expect("CreateWindowExW")
    };

    unsafe {
        SetWindowLongPtrW(
            w,
            WINDOW_LONG_PTR_INDEX(0),
            app.as_mut().get_unchecked_mut() as *mut _ as _,
        );
        SetWindowLongPtrW(
            w,
            WINDOW_LONG_PTR_INDEX(core::mem::size_of::<usize>() as _),
            event_store.as_mut().get_mut() as *mut _ as _,
        );
        let _ = ShowWindow(w, SW_SHOWNORMAL);
    }

    let mut msg = core::mem::MaybeUninit::uninit();
    'app: loop {
        match unsafe { GetMessageW(msg.as_mut_ptr(), None, 0, 0) } {
            BOOL(0) => break 'app,
            BOOL(-1) => Err::<(), _>(std::io::Error::last_os_error()).expect("GetMessageW"),
            _ => unsafe {
                let msg = msg.assume_init_ref();
                DispatchMessageW(msg);
            },
        }
    }

    *event_store = Some(0);
    while app
        .as_mut()
        .poll(&mut core::task::Context::from_waker(&unsafe {
            core::task::Waker::new(&(), &APP_WAKER_VTABLE)
        }))
        .is_pending()
    {}
}

struct EventQueue {
    event_store: *mut Option<u32>,
}
impl EventQueue {
    pub async fn next_event(&self) -> u32 {
        EventQueueNextEventAwaiter { q: self }.await
    }
}

struct EventQueueNextEventAwaiter<'e> {
    q: &'e EventQueue,
}
impl<'e> core::future::Future for EventQueueNextEventAwaiter<'e> {
    type Output = u32;

    fn poll(
        self: std::pin::Pin<&mut Self>,
        _cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        match unsafe { (&mut *self.get_mut().q.event_store).take() } {
            None => core::task::Poll::Pending,
            Some(x) => core::task::Poll::Ready(x),
        }
    }
}

async fn run(event_queue: EventQueue) {
    loop {
        match event_queue.next_event().await {
            r if r == 0 => {
                println!("app finish");
                break;
            }
            _ => (),
        }
    }
}

extern "system" fn wndproc<AppFuture: core::future::Future<Output = ()>>(
    hwnd: HWND,
    msg: u32,
    wparam: WPARAM,
    lparam: LPARAM,
) -> LRESULT {
    let app_future = unsafe { GetWindowLongPtrW(hwnd, WINDOW_LONG_PTR_INDEX(0)) };
    let event_store = unsafe {
        GetWindowLongPtrW(
            hwnd,
            WINDOW_LONG_PTR_INDEX(core::mem::size_of::<usize>() as _),
        )
    };

    if msg == WM_DESTROY {
        unsafe {
            PostQuitMessage(0);
        }

        return LRESULT(0);
    }

    unsafe { DefWindowProcW(hwnd, msg, wparam, lparam) }
}
