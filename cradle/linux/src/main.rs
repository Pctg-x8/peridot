
mod x11;

// TODO AssetLoaderつくる
pub struct PlatformAssetLoader {

}
impl peridot::AssetLoader for PlatformAssetLoader {

}

fn main() {
    let mut con = x11::Connection::connect_local().expect("Could not connect Unix Domain Socket of Xorg server");
    let &x11::ScreenStruct { root_window_id, root_depth, .. } = con.screen_iter().next().expect("No root screen");

    let (wm_protocols, wm_delete_window) =
        match con.intern_atoms(&[b"WM_PROTOCOLS", b"WM_DELETE_WINDOW"]).unwrap()[..] {
            [a, b] => (a, b), _ => unreachable!()
        };
    let mainwnd_id = con.next_resource_id();
    x11::RequestCreateWindowFixedFields {
        target_id: mainwnd_id, parent_id: root_window_id, depth: root_depth, width: 640, height: 480,
        .. Default::default()
    }.send_with(&[], &mut con).unwrap();
    let mut bp = x11::BufferedPacket::new();
    bp.change_property(mainwnd_id, x11::WM_NAME, x11::STRING, b"Peridot Engine");
    bp.append_property(mainwnd_id, wm_protocols, x11::ATOM, &[wm_delete_window]);
    bp.map(mainwnd_id);
    bp.send(&mut con).unwrap();

    con.start_nonblocking();
    'outer: loop {
        while let Some(ev) = con.get_event().unwrap() {
            if ev[0] & 0x7f == x11::ClientMessageEvent::CODE {
                let e = unsafe { x11::ClientMessageEvent::as_ref(&ev) };
                if unsafe { e.u32_data()[0] == wm_delete_window } { break 'outer; }
            }
            else { println!("Generic Event: {:?}", ev); }
        }
        // RenderLoop here...
    }
    println!("Terminating Program...");
}
