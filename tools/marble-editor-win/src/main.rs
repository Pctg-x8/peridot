use windows::{
    core::{s, Interface, PCSTR},
    Foundation::Numerics::{Vector2, Vector3},
    Win32::{
        Foundation::{HWND, LPARAM, LRESULT, WPARAM},
        Graphics::Gdi::HBRUSH,
        System::{
            LibraryLoader::GetModuleHandleA,
            WinRT::{
                Composition::ICompositorDesktopInterop, CreateDispatcherQueueController,
                DispatcherQueueOptions, DQTAT_COM_ASTA, DQTYPE_THREAD_CURRENT,
            },
        },
        UI::WindowsAndMessaging::{
            CreateWindowExA, DefWindowProcA, DispatchMessageA, GetMessageA, LoadCursorA, LoadIconA,
            PostQuitMessage, RegisterClassExA, ShowWindow, TranslateMessage, CW_USEDEFAULT,
            IDC_ARROW, IDI_APPLICATION, MSG, SW_SHOWNORMAL, WM_DESTROY, WNDCLASSEXA,
            WNDCLASS_STYLES, WS_EX_APPWINDOW, WS_EX_NOREDIRECTIONBITMAP, WS_OVERLAPPEDWINDOW,
        },
    },
    UI::Color,
};

fn main() {
    let instance_handle = unsafe { GetModuleHandleA(None).expect("Failed to get instance handle") };
    let wndclass = WNDCLASSEXA {
        cbSize: core::mem::size_of::<WNDCLASSEXA>() as _,
        cbClsExtra: 0,
        cbWndExtra: 0,
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

    let dispatcher_queue_controller = unsafe {
        CreateDispatcherQueueController(DispatcherQueueOptions {
            dwSize: core::mem::size_of::<DispatcherQueueOptions>() as _,
            threadType: DQTYPE_THREAD_CURRENT,
            apartmentType: DQTAT_COM_ASTA,
        })
        .expect("Failed to create dispatcher queue controller")
    };
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
    let composition_root = compositor
        .CreateContainerVisual()
        .expect("Failed to create root visual");
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

    unsafe {
        let _ = ShowWindow(window_handle, SW_SHOWNORMAL);
    }

    let mut msg = core::mem::MaybeUninit::<MSG>::uninit();
    while unsafe { GetMessageA(msg.as_mut_ptr(), None, 0, 0).0 > 0 } {
        unsafe {
            let _ = TranslateMessage(msg.as_ptr());
            DispatchMessageA(msg.as_ptr());
        }
    }

    std::process::exit(unsafe { msg.assume_init().wParam.0 as _ });
}

extern "system" fn window_proc(hwnd: HWND, msg: u32, wp: WPARAM, lp: LPARAM) -> LRESULT {
    if msg == WM_DESTROY {
        unsafe { PostQuitMessage(0) };
        return LRESULT(0);
    }

    unsafe { DefWindowProcA(hwnd, msg, wp, lp) }
}
