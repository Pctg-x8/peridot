use core::cell::{Cell, OnceCell, UnsafeCell};

use shared::{LogicalUnit, Point, Rect};
use windows::Win32::{
    Foundation::{
        DRAGDROP_S_CANCEL, DRAGDROP_S_DROP, DRAGDROP_S_USEDEFAULTCURSORS, DV_E_FORMATETC, HWND,
        POINT, POINTL, RECT, S_FALSE, S_OK,
    },
    Graphics::Gdi::{MONITOR_DEFAULTTONEAREST, MapWindowPoints, MonitorFromRect},
    System::{
        Com::{
            DATADIR_GET, DVASPECT_CONTENT, FORMATETC, IAdviseSink, IDataObject, IDataObject_Impl,
            IEnumFORMATETC, IEnumFORMATETC_Impl, IEnumSTATDATA, STGMEDIUM, TYMED_HGLOBAL,
            TYMED_NULL,
        },
        Memory::{GlobalLock, GlobalUnlock},
        Ole::{
            CF_HDROP, CF_PRIVATEFIRST, CLIPBOARD_FORMAT, DROPEFFECT, DROPEFFECT_COPY,
            DROPEFFECT_NONE, IDropSource, IDropSource_Impl, IDropTarget, IDropTarget_Impl,
            ReleaseStgMedium,
        },
        SystemServices::{
            MK_LBUTTON, MK_MBUTTON, MK_RBUTTON, MK_XBUTTON1, MK_XBUTTON2, MODIFIERKEYS_FLAGS,
        },
    },
    UI::{
        HiDpi::{GetDpiForMonitor, MDT_EFFECTIVE_DPI},
        Shell::DROPFILES,
        WindowsAndMessaging::{GCW_ATOM, GetClassLongPtrW, GetCursorPos, WindowFromPoint},
    },
};
use windows_core::{BOOL, HRESULT, implement};

use crate::{
    input::hittest::DragDropFlags,
    platform::{
        WindowHandleBase,
        windows::{
            ApplicationContext,
            toplevel::{WindowEventHandler, WindowHandle},
        },
    },
    utils::platform::windows::point_from_win32,
};

pub struct DragData {
    obj: IDataObject,
    is_file_drop: OnceCell<bool>,
}
impl DragData {
    pub fn is_file_drop(&self) -> bool {
        *self.is_file_drop.get_or_init(|| {
            let enumformatetc = unsafe {
                self.obj
                    .EnumFormatEtc(DATADIR_GET.0 as _)
                    .expect("pdataobj.enum_format_etc")
            };
            let mut elements = Vec::<FORMATETC>::with_capacity(8);
            let mut fetched_elements = core::mem::MaybeUninit::uninit();
            match unsafe {
                enumformatetc.Next(
                    core::mem::transmute(elements.spare_capacity_mut()),
                    Some(fetched_elements.as_mut_ptr()),
                )
            } {
                S_OK => {
                    let fetched = unsafe { fetched_elements.assume_init() };
                    unsafe {
                        elements.set_len(fetched as usize);
                    }
                }
                _ => {
                    elements = Vec::with_capacity(unsafe { fetched_elements.assume_init() } as _);
                    unsafe {
                        enumformatetc
                            .Next(
                                core::mem::transmute(elements.spare_capacity_mut()),
                                Some(fetched_elements.as_mut_ptr()),
                            )
                            .ok()
                            .expect("enumformatetc.next");
                    }
                    unsafe {
                        elements.set_len(fetched_elements.assume_init() as _);
                    }
                }
            }
            for e in elements {
                tracing::trace!(?e, is_file_drop = e.cfFormat == CF_HDROP.0, "enumformatetc");

                if e.cfFormat == CF_HDROP.0 {
                    return true;
                }
            }

            false
        })
    }

    pub fn query_file_list(&self) -> std::io::Result<Vec<String>> {
        let mut data = unsafe {
            self.obj.GetData(&FORMATETC {
                cfFormat: CF_HDROP.0,
                ptd: core::ptr::null_mut(),
                dwAspect: DVASPECT_CONTENT.0,
                lindex: -1,
                tymed: TYMED_HGLOBAL.0.cast_unsigned(),
            })?
        };
        let ptr = unsafe { GlobalLock(data.u.hGlobal) };
        let DROPFILES { pFiles, fWide, .. } = unsafe { ptr.cast::<DROPFILES>().read() };

        let mut files = Vec::new();
        if fWide.as_bool() {
            let mut o = 0;
            let words = unsafe { ptr.byte_add(pFiles as _).cast::<u16>() };
            'a: loop {
                let r0 = o;
                while unsafe { words.add(o).read() } != 0 {
                    o += 1;
                }
                if o == r0 {
                    break 'a;
                }
                files.push(
                    String::from_utf16(unsafe {
                        core::slice::from_raw_parts(words.add(r0), o - r0)
                    })
                    .expect("invalid utf16"),
                );
                o += 1;
            }
        } else {
            let mut o = 0;
            let bytes = unsafe { ptr.byte_add(pFiles as _).cast::<u8>() };
            'a: loop {
                let r0 = o;
                while unsafe { bytes.add(o).read() } != 0 {
                    o += 1;
                }
                if o == r0 {
                    break 'a;
                }
                files.push(unsafe {
                    str::from_utf8_unchecked(core::slice::from_raw_parts(bytes.add(r0), o - r0))
                        .to_string()
                });
                o += 1;
            }
        }

        unsafe {
            let _ = GlobalUnlock(data.u.hGlobal);
            ReleaseStgMedium(&mut data);
        }

        Ok(files)
    }
}

#[implement(IDropTarget)]
pub(super) struct DropTarget {
    pub hwnd: HWND,
    pub active_data: UnsafeCell<Option<IDataObject>>,
}
impl IDropTarget_Impl for DropTarget_Impl {
    fn DragEnter(
        &self,
        pdataobj: windows_core::Ref<IDataObject>,
        grfkeystate: MODIFIERKEYS_FLAGS,
        pt: &POINTL,
        pdweffect: *mut DROPEFFECT,
    ) -> windows_core::Result<()> {
        tracing::debug!(?pt, ?grfkeystate, "drag enter");
        let dataobj = pdataobj.as_ref().expect("no dataobj");
        unsafe {
            *self.active_data.get() = Some(dataobj.clone());
        }

        let mut client_pos = [POINT { x: pt.x, y: pt.y }];
        unsafe { MapWindowPoints(None, Some(self.hwnd), &mut client_pos) };
        let [client_pos] = client_pos;

        let state = WindowEventHandler::get_for_window(self.hwnd);
        let coreloop = state.coreloop();

        let offerred_flags = coreloop.pointer_input_manager.offer_accepting_drop(
            &DragData {
                obj: dataobj.clone(),
                is_file_drop: OnceCell::new(),
            },
            point_from_win32(client_pos).to_logical(WindowHandle(self.hwnd).ui_scale_factor()),
            WindowHandle(self.hwnd).state().ht_root,
            WindowHandle(self.hwnd).client_size(),
            &coreloop.ht_manager,
        );
        let mut effects = DROPEFFECT_NONE;
        if offerred_flags.contains(DragDropFlags::COPY) {
            effects |= DROPEFFECT_COPY;
        }
        unsafe {
            pdweffect.write(effects);
        }

        Ok(())
    }

    fn DragOver(
        &self,
        grfkeystate: MODIFIERKEYS_FLAGS,
        pt: &POINTL,
        pdweffect: *mut DROPEFFECT,
    ) -> windows_core::Result<()> {
        tracing::debug!(?pt, ?grfkeystate, "drag over");
        let Some(dataobj) = (unsafe { &*self.active_data.get() }) else {
            unsafe {
                pdweffect.write(DROPEFFECT_NONE);
            }
            return Ok(());
        };

        let mut client_pos = [POINT { x: pt.x, y: pt.y }];
        unsafe { MapWindowPoints(None, Some(self.hwnd), &mut client_pos) };
        let [client_pos] = client_pos;

        let state = WindowEventHandler::get_for_window(self.hwnd);
        let coreloop = state.coreloop();

        let offerred_flags = coreloop.pointer_input_manager.offer_accepting_drop(
            &DragData {
                obj: dataobj.clone(),
                is_file_drop: OnceCell::new(),
            },
            point_from_win32(client_pos).to_logical(WindowHandle(self.hwnd).ui_scale_factor()),
            WindowHandle(self.hwnd).state().ht_root,
            WindowHandle(self.hwnd).client_size(),
            &coreloop.ht_manager,
        );
        let mut effects = DROPEFFECT_NONE;
        if offerred_flags.contains(DragDropFlags::COPY) {
            effects |= DROPEFFECT_COPY;
        }
        unsafe {
            pdweffect.write(effects);
        }

        Ok(())
    }

    fn DragLeave(&self) -> windows_core::Result<()> {
        unsafe {
            *self.active_data.get() = None;
        }
        Ok(())
    }

    fn Drop(
        &self,
        pdataobj: windows_core::Ref<IDataObject>,
        grfkeystate: MODIFIERKEYS_FLAGS,
        pt: &POINTL,
        pdweffect: *mut DROPEFFECT,
    ) -> windows_core::Result<()> {
        tracing::debug!(?pt, ?grfkeystate, "drop");
        let dataobj = pdataobj.as_ref().expect("no dataobj");

        let mut client_pos = [POINT { x: pt.x, y: pt.y }];
        unsafe { MapWindowPoints(None, Some(self.hwnd), &mut client_pos) };
        let [client_pos] = client_pos;

        WindowEventHandler::get_for_window(self.hwnd)
            .coreloop()
            .perform_drop(
                DragData {
                    obj: dataobj.clone(),
                    is_file_drop: OnceCell::new(),
                }
                .into(),
                WindowHandle(self.hwnd),
                point_from_win32(client_pos).to_logical(WindowHandle(self.hwnd).ui_scale_factor()),
            );
        WindowEventHandler::get_for_window(self.hwnd)
            .coreloop()
            .update_view_all();

        Ok(())
    }
}

const CF_DOCK_PANE: CLIPBOARD_FORMAT = CLIPBOARD_FORMAT(CF_PRIVATEFIRST.0 + 0);

#[implement(IEnumFORMATETC)]
#[derive(Clone)]
struct DockPaneDataObjectFormatEtcEnumerator {
    datadir: u32,
    ptr: Cell<usize>,
}
impl IEnumFORMATETC_Impl for DockPaneDataObjectFormatEtcEnumerator_Impl {
    fn Clone(&self) -> windows_core::Result<IEnumFORMATETC> {
        Ok(IEnumFORMATETC::from((&**self).clone()))
    }

    fn Next(
        &self,
        celt: u32,
        rgelt: *mut FORMATETC,
        pceltfetched: *mut u32,
    ) -> windows_core::HRESULT {
        if self.datadir == DATADIR_GET.0 as _ {
            let returning_count = 1usize.saturating_sub(self.ptr.get()).min(celt as usize);
            if returning_count == 0 {
                // no items
                if let Some(x) = unsafe { pceltfetched.as_mut() } {
                    *x = 0;
                }

                return S_FALSE;
            }

            // 今のところ1種類しかないのでそれを返す
            assert_eq!(returning_count, 1);
            let sink = unsafe { &mut *rgelt };
            sink.cfFormat = CF_DOCK_PANE.0 as _;
            sink.ptd = core::ptr::null_mut();
            sink.dwAspect = DVASPECT_CONTENT.0;
            sink.lindex = -1;
            sink.tymed = TYMED_NULL.0 as _;

            self.ptr.update(|x| x + returning_count);
            if let Some(x) = unsafe { pceltfetched.as_mut() } {
                *x = returning_count as _;
            }

            S_OK
        } else {
            // Set方向は今のところない
            if let Some(x) = unsafe { pceltfetched.as_mut() } {
                *x = 0;
            }

            S_FALSE
        }
    }

    fn Reset(&self) -> windows_core::Result<()> {
        self.ptr.set(0);
        Ok(())
    }

    fn Skip(&self, celt: u32) -> windows_core::Result<()> {
        self.ptr.update(|x| x + celt as usize);
        Ok(())
    }
}

#[implement(IDataObject)]
pub(super) struct DockPaneDataObject {}
impl IDataObject_Impl for DockPaneDataObject_Impl {
    fn DAdvise(
        &self,
        _pformatetc: *const FORMATETC,
        advf: u32,
        _padvsink: windows_core::Ref<IAdviseSink>,
    ) -> windows_core::Result<u32> {
        tracing::debug!(advf, "DockPaneDataObject::DAdvise");
        Err(windows::Win32::Foundation::E_NOTIMPL.into())
    }

    fn DUnadvise(&self, dwconnection: u32) -> windows_core::Result<()> {
        tracing::debug!(dwconnection, "DockPaneDataObject::DUnadvise");
        Err(windows::Win32::Foundation::E_NOTIMPL.into())
    }

    fn EnumDAdvise(&self) -> windows_core::Result<IEnumSTATDATA> {
        tracing::debug!("DockPaneDataObject::EnumDAdvise");
        Err(windows::Win32::Foundation::E_NOTIMPL.into())
    }

    fn EnumFormatEtc(&self, dwdirection: u32) -> windows_core::Result<IEnumFORMATETC> {
        Ok(IEnumFORMATETC::from(
            DockPaneDataObjectFormatEtcEnumerator {
                datadir: dwdirection,
                ptr: Cell::new(0),
            },
        ))
    }

    fn GetCanonicalFormatEtc(
        &self,
        _pformatectin: *const FORMATETC,
        _pformatetcout: *mut FORMATETC,
    ) -> HRESULT {
        tracing::debug!("DockPaneDataObject::GetCanonicalFormatEtc");
        windows::Win32::Foundation::E_NOTIMPL
    }

    fn GetData(&self, pformatetcin: *const FORMATETC) -> windows_core::Result<STGMEDIUM> {
        let format = unsafe { &*pformatetcin };
        tracing::debug!(?format, "DockPaneDataObject::GetData");
        Err(DV_E_FORMATETC.into())
    }

    fn GetDataHere(
        &self,
        pformatetc: *const FORMATETC,
        _pmedium: *mut STGMEDIUM,
    ) -> windows_core::Result<()> {
        let format = unsafe { &*pformatetc };
        tracing::debug!(?format, "DockPaneDataObject::GetDataHere");
        Err(DV_E_FORMATETC.into())
    }

    fn QueryGetData(&self, pformatetc: *const FORMATETC) -> HRESULT {
        let format = unsafe { &*pformatetc };
        tracing::debug!(?format, "DockPaneDataObject::QueryGetData");
        DV_E_FORMATETC
    }

    fn SetData(
        &self,
        _pformatetc: *const FORMATETC,
        _pmedium: *const STGMEDIUM,
        frelease: BOOL,
    ) -> windows_core::Result<()> {
        tracing::debug!(?frelease, "DockPaneDataObject::SetData");
        Err(windows::Win32::Foundation::E_NOTIMPL.into())
    }
}

#[implement(IDropSource)]
pub(super) struct DockPaneDropSource {
    pub app_context: *const ApplicationContext,
    pub source_rect: Rect<LogicalUnit>,
    pub active_dpi_scale: Cell<f32>,
    pub dest_window: Cell<HWND>,
}
impl IDropSource_Impl for DockPaneDropSource_Impl {
    fn GiveFeedback(&self, dweffect: DROPEFFECT) -> HRESULT {
        tracing::debug!(?dweffect, "DockPaneDropSource::GiveFeedback");

        let mut cursor_pos = core::mem::MaybeUninit::uninit();
        if let Err(e) = unsafe { GetCursorPos(cursor_pos.as_mut_ptr()) } {
            tracing::error!(reason = %e, "win32.get_cursor_pos");
            return DRAGDROP_S_USEDEFAULTCURSORS;
        }
        let cursor_pos = unsafe { cursor_pos.assume_init() };

        let dest_window = unsafe { WindowFromPoint(cursor_pos) };
        self.dest_window.set(dest_window);
        if !dest_window.is_invalid()
            && unsafe { GetClassLongPtrW(dest_window, GCW_ATOM) }
                == unsafe { &*self.app_context }.wc_set.main as usize
        {
            // どれかのEditorWindowに入ってる場合はそっちで計算する
            let mut ps = [cursor_pos];
            unsafe {
                MapWindowPoints(None, Some(dest_window), &mut ps);
            }
            let [client_pos] = ps;

            tracing::debug!(?client_pos, ?dest_window, "give feedback via event");
            WindowEventHandler::get_for_window(dest_window)
                .coreloop()
                .move_redock_preview(
                    WindowHandle(dest_window),
                    point_from_win32(client_pos)
                        .to_logical(WindowHandle(dest_window).ui_scale_factor()),
                );

            return S_OK;
        }

        // 入ってなければカーソルに追従
        let o = self
            .source_rect
            .left_top()
            .to_pixels_round(self.active_dpi_scale.get());
        let s = self
            .source_rect
            .size()
            .to_pixels_ceil(self.active_dpi_scale.get());
        let mut new_dpi = core::mem::MaybeUninit::uninit();
        let mut new_dpi_y = core::mem::MaybeUninit::uninit();
        unsafe {
            GetDpiForMonitor(
                MonitorFromRect(
                    &RECT {
                        left: cursor_pos.x + o.x,
                        top: cursor_pos.y + o.y,
                        right: cursor_pos.x + o.x + s.width.cast_signed(),
                        bottom: cursor_pos.y + o.y + s.height.cast_signed(),
                    },
                    MONITOR_DEFAULTTONEAREST,
                ),
                MDT_EFFECTIVE_DPI,
                new_dpi.as_mut_ptr(),
                new_dpi_y.as_mut_ptr(),
            )
            .expect("win32.get_dpi_for_window")
        };
        self.active_dpi_scale
            .set(unsafe { new_dpi.assume_init() } as f32 / 96.0f32);

        unsafe { &*self.app_context }
            .drag_preview_popover
            .set_rect_screen_coord(&Rect::from_lt_size(
                Point::new_pixels(cursor_pos.x, cursor_pos.y).with_offset(
                    self.source_rect
                        .left_top()
                        .to_pixels_round(self.active_dpi_scale.get()),
                ),
                self.source_rect
                    .size()
                    .to_pixels_ceil(self.active_dpi_scale.get()),
            ));

        DRAGDROP_S_USEDEFAULTCURSORS
    }

    fn QueryContinueDrag(&self, fescapepressed: BOOL, grfkeystate: MODIFIERKEYS_FLAGS) -> HRESULT {
        if fescapepressed.as_bool() {
            return DRAGDROP_S_CANCEL;
        }

        // Note: containsだとすべてのビットフラグを持ってるかのチェックになってしまう（!= 0での比較だといずれかにできる）
        if grfkeystate & (MK_LBUTTON | MK_RBUTTON | MK_MBUTTON | MK_XBUTTON1 | MK_XBUTTON2)
            != MODIFIERKEYS_FLAGS(0)
        {
            // continue while pressing mouse button
            return S_OK;
        }

        return DRAGDROP_S_DROP;
    }
}
