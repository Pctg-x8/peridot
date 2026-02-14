use std::collections::HashMap;

use bedrock::{self as br, ImageChild, MemoryBound, VkHandle, VkObject};

use crate::graphics::VulkanDevice;

#[derive(Debug, Clone)]
pub struct GlyphRect {
    pub left: u32,
    pub top: u32,
    pub width: u32,
    pub height: u32,
}

pub struct GlyphAtlas {
    res: br::vk::VkImage,
    mem: br::vk::VkDeviceMemory,
    view: br::vk::VkImageView,
    acquired_rects: HashMap<(usize, u16), GlyphRect>,
    space_mgr: GlyphAtlasSpaceManager,
}
impl GlyphAtlas {
    pub const MULTISAMPLE_LEVEL: u32 = 4;

    pub unsafe fn drop(&mut self, gfx: &VulkanDevice) {
        unsafe {
            br::vkfn_wrapper::destroy_image_view(gfx.native_ptr(), self.view, None);
            br::vkfn_wrapper::destroy_image(gfx.native_ptr(), self.res, None);
            br::vkfn_wrapper::free_memory(gfx.native_ptr(), self.mem, None);
        }
    }

    pub fn new(gfx: &VulkanDevice) -> Self {
        let size = br::Extent2D::spread1(4096);

        let mut res = br::ImageObject::new(
            gfx,
            &br::ImageCreateInfo::new(size, br::vk::VK_FORMAT_R8_UNORM).set_usage(
                br::ImageUsageFlags::SAMPLED
                    | br::ImageUsageFlags::COLOR_ATTACHMENT
                    | br::ImageUsageFlags::TRANSFER_DEST,
            ),
        )
        .expect("res create");
        let memory_requirements = res.requirements();
        let mem = br::DeviceMemoryObject::new(
            gfx,
            &br::MemoryAllocateInfo::new(
                memory_requirements.size,
                gfx.find_device_local_memory_index(memory_requirements.memoryTypeBits)
                    .expect("no suitable memory"),
            ),
        )
        .expect("res malloc");
        res.bind(&mem, 0).expect("res mem bind");
        let view = br::ImageViewBuilder::new(
            res,
            br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
        )
        .create()
        .expect("res view create");

        view.image()
            .set_name(Some(c"Glyph Atlas"))
            .expect("res set name");
        mem.set_name(Some(c"Glyph Atlas [Backing]"))
            .expect("mem set name");
        view.set_name(Some(c"Glyph Atlas [View]"))
            .expect("view set name");

        let (view, res) = view.unmanage();
        let (res, _, _, _, _) = res.unmanage();
        let (mem, _) = mem.unmanage();
        Self {
            res,
            mem,
            view,
            acquired_rects: HashMap::new(),
            space_mgr: GlyphAtlasSpaceManager::new(size),
        }
    }

    pub fn clear(&mut self) {
        self.space_mgr.clear();
        self.acquired_rects.clear();
        // TODO: clear atlas content?
    }

    pub fn acquire(&mut self, key: (usize, u16), width: u32, height: u32) -> (GlyphRect, bool) {
        match self.acquired_rects.entry(key) {
            std::collections::hash_map::Entry::Vacant(x) => (
                x.insert(
                    self.space_mgr
                        .acquire(width, height)
                        .expect("no space left"),
                )
                .clone(),
                true,
            ),
            std::collections::hash_map::Entry::Occupied(x) => (x.get().clone(), false),
        }
    }

    #[inline(always)]
    pub const fn size(&self) -> &br::Extent2D {
        &self.space_mgr.max
    }

    #[inline(always)]
    pub const fn image<'s>(&'s self) -> br::VkHandleRef<'s, br::vk::VkImage> {
        unsafe { br::VkHandleRef::dangling(self.res) }
    }

    #[inline(always)]
    pub const fn image_range_entire(&self) -> br::ImageSubresourceRange {
        br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1)
    }

    #[inline(always)]
    pub const fn view<'s>(&'s self) -> br::VkHandleRef<'s, br::vk::VkImageView> {
        unsafe { br::VkHandleRef::dangling(self.view) }
    }
}

struct Skyline {
    pub y: u32,
    pub width: u32,
}

struct GlyphAtlasSpaceManager {
    // skyline method
    max: br::Extent2D,
    skylines: Vec<Skyline>,
}
impl GlyphAtlasSpaceManager {
    const SPACING: u32 = 1;

    pub fn new(max: br::Extent2D) -> Self {
        Self {
            skylines: vec![Skyline {
                y: 0,
                width: max.width,
            }],
            max,
        }
    }

    pub fn clear(&mut self) {
        self.skylines.clear();
        self.skylines.push(Skyline {
            y: 0,
            width: self.max.width,
        });
    }

    pub fn acquire(&mut self, width: u32, height: u32) -> Option<GlyphRect> {
        let cons_width = width + Self::SPACING;
        let cons_height = height + Self::SPACING;

        let mut fit_left_top = None;
        let mut left = 0;
        let mut n = 0;
        while n < self.skylines.len() && left + cons_width <= self.max.width {
            let skyline = &self.skylines[n];
            let skyline_height = self.max.height - skyline.y;
            if skyline_height >= cons_height && fit_left_top.is_none_or(|(_, t, _)| skyline.y < t) {
                let mut y = skyline.y;

                // potentially overlapping skylines at right
                let mut l1 = left + skyline.width;
                let mut m = n + 1;
                while m < self.skylines.len() && l1 <= left + cons_width {
                    let skyline2 = &self.skylines[m];

                    y = y.max(skyline2.y);
                    l1 += skyline2.width;
                    m += 1;
                }

                // recompute whether it fits
                let skyline_height = self.max.height - y;
                if skyline_height >= cons_height && fit_left_top.is_none_or(|(_, t, _)| y < t) {
                    fit_left_top = Some((left, y, n));
                }
            }

            left += skyline.width;
            n += 1;
        }

        let Some((left, top, left_skyline_point)) = fit_left_top else {
            // no available rects
            return None;
        };

        // update skyline
        let mut left_w = cons_width;
        let mut skyline_point_index = left_skyline_point;
        while left_w > 0 {
            let skyline = &self.skylines[skyline_point_index];

            if skyline.width > left_w {
                // needs splitting(and finishes at this step)
                if skyline_point_index > 0
                    && self.skylines[skyline_point_index - 1].y == top + cons_height
                {
                    // fuse with previous
                    self.skylines[skyline_point_index - 1].width += left_w;
                    self.skylines[skyline_point_index].width -= left_w;
                } else {
                    let org_skyline_y = skyline.y;
                    let right_skyline_width = skyline.width - left_w;
                    self.skylines[skyline_point_index] = Skyline {
                        y: top + cons_height,
                        width: left_w,
                    };
                    self.skylines.insert(
                        skyline_point_index + 1,
                        Skyline {
                            y: org_skyline_y,
                            width: right_skyline_width,
                        },
                    );
                }

                break;
            }

            let sw = skyline.width;
            if skyline_point_index > 0
                && self.skylines[skyline_point_index - 1].y == top + cons_height
            {
                // fuse with previous
                self.skylines[skyline_point_index - 1].width += sw;
                self.skylines.remove(skyline_point_index);
                skyline_point_index -= 1;
            } else {
                // just move this skyline
                self.skylines[left_skyline_point].y = top + cons_height;
            }

            left_w -= sw.min(left_w);
            skyline_point_index += 1;
        }

        Some(GlyphRect {
            left,
            top,
            width,
            height,
        })
    }
}

#[cfg(feature = "freetype")]
pub struct FreeType(ft::Library);
#[cfg(feature = "freetype")]
impl Drop for FreeType {
    #[inline(always)]
    fn drop(&mut self) {
        if let Err(e) = unsafe { ft::done_freetype(self.0) } {
            tracing::error!(reason = ?e, "FreeType.done");
        }
    }
}
#[cfg(feature = "freetype")]
unsafe impl Sync for FreeType {}
#[cfg(feature = "freetype")]
unsafe impl Send for FreeType {}
#[cfg(feature = "freetype")]
impl FreeType {
    #[inline(always)]
    pub fn init() -> ft::Result<Self> {
        ft::init_freetype().map(Self)
    }
}

#[derive(Default, Clone, Copy, Debug)]
#[repr(usize)]
pub enum FontID {
    #[default]
    UIDefault,
    UITitleProjectName,
}

pub struct FontSet {
    #[cfg(target_os = "macos")]
    ui_default: apple_sdk_port::Owned<apple_sdk_port::text::Font>,
    #[cfg(target_os = "macos")]
    ui_title_project_name: apple_sdk_port::Owned<apple_sdk_port::text::Font>,
    #[cfg(feature = "freetype")]
    ui_default: ft::Face,
    #[cfg(feature = "freetype")]
    ui_title_project_name: ft::Face,
    #[cfg(feature = "harfbuzz")]
    ui_default_shaping: core::ptr::NonNull<peridot_tp_harfbuzz::ffi::hb_font_t>,
    #[cfg(feature = "harfbuzz")]
    ui_title_project_name_shaping: core::ptr::NonNull<peridot_tp_harfbuzz::ffi::hb_font_t>,
    #[cfg(windows)]
    dw_factory: windows::Win32::Graphics::DirectWrite::IDWriteFactory,
    #[cfg(windows)]
    ui_default: windows::Win32::Graphics::DirectWrite::IDWriteTextFormat,
    #[cfg(windows)]
    ui_title_project_name: windows::Win32::Graphics::DirectWrite::IDWriteTextFormat,
}
#[cfg(not(windows))]
impl Drop for FontSet {
    fn drop(&mut self) {
        #[cfg(feature = "harfbuzz")]
        unsafe {
            peridot_tp_harfbuzz::ffi::hb_font_destroy(self.ui_default_shaping.as_ptr());
            peridot_tp_harfbuzz::ffi::hb_font_destroy(self.ui_title_project_name_shaping.as_ptr());
        }
        #[cfg(feature = "freetype")]
        {
            if let Err(e) = unsafe { ft::done_face(self.ui_title_project_name) } {
                tracing::error!(reason = %e, "ui_title_project_name.done_face");
            }
            if let Err(e) = unsafe { ft::done_face(self.ui_default) } {
                tracing::error!(reason = %e, "ui_default.done_face");
            }
        }
    }
}
impl FontSet {
    #[cfg(windows)]
    pub fn new(dw: windows::Win32::Graphics::DirectWrite::IDWriteFactory) -> Self {
        use windows::Win32::Globalization::GetUserDefaultLocaleName;

        let mut locale_name = [const { core::mem::MaybeUninit::uninit() }; 32];
        let len = unsafe {
            GetUserDefaultLocaleName(core::mem::transmute::<
                &mut [core::mem::MaybeUninit<u16>; 32],
                &mut [u16; 32],
            >(&mut locale_name))
        };
        let locale_name = if len == 0 {
            // fallback to en_US
            let e = std::io::Error::last_os_error();
            tracing::warn!(reason = ?e, "GetUserDefaultLocaleName.fallback");

            &[b'e' as u16, b'n' as _, b'_' as _, b'U' as _, b'S' as _, 0]
        } else {
            unsafe {
                core::mem::transmute::<&[core::mem::MaybeUninit<u16>], &[u16]>(
                    &locale_name[..len as usize],
                )
            }
        };

        let ui_default = unsafe {
            dw.CreateTextFormat(
                windows_core::w!("Inter Display"),
                None,
                windows::Win32::Graphics::DirectWrite::DWRITE_FONT_WEIGHT_NORMAL,
                windows::Win32::Graphics::DirectWrite::DWRITE_FONT_STYLE_NORMAL,
                windows::Win32::Graphics::DirectWrite::DWRITE_FONT_STRETCH_NORMAL,
                12.0,
                windows_core::PCWSTR(locale_name.as_ptr()),
            )
            .expect("dwrite.textformat.create.ui_default")
        };
        let ui_title_project_name = unsafe {
            dw.CreateTextFormat(
                windows_core::w!("Inter Display"),
                None,
                windows::Win32::Graphics::DirectWrite::DWRITE_FONT_WEIGHT_NORMAL,
                windows::Win32::Graphics::DirectWrite::DWRITE_FONT_STYLE_NORMAL,
                windows::Win32::Graphics::DirectWrite::DWRITE_FONT_STRETCH_NORMAL,
                10.0,
                windows_core::PCWSTR(locale_name.as_ptr()),
            )
            .expect("dwrite.textformat.create.ui_title_project_name")
        };

        Self {
            dw_factory: dw,
            ui_default,
            ui_title_project_name,
        }
    }

    #[cfg(feature = "freetype")]
    pub fn new(lib: &FreeType, dpi: u32) -> Self {
        use peridot_tp_freetype::FractionalExt;

        #[cfg(feature = "fontconfig")]
        let (font_file_path, face_index) = unsafe {
            fc::init().expect("FontConfig.init");
            let mut pat = fc::Pattern::new().expect("FcPattern.create");
            pat.as_mut()
                .add(fc::Pattern::KEY_FAMILY, c"Inter Display")
                .expect("FcPattern.add.family");
            pat.as_mut()
                .add(fc::Pattern::KEY_WEIGHT, &fc::raw::FC_WEIGHT_REGULAR)
                .expect("FcPattern.add.weight");
            pat.as_mut()
                .add(fc::Pattern::KEY_SIZE, &(12.0 as core::ffi::c_double))
                .expect("FcPattern.add.size");
            fc::Config::current()
                .unwrap_unchecked()
                .as_mut()
                .substitute(pat.as_mut(), fc::MatchKind::Pattern)
                .expect("FcConfig.substitute");
            pat.as_mut().default_substitute();
            let fonts = fc::sort(
                fc::Config::current().unwrap_unchecked().as_mut(),
                pat.as_mut(),
                false,
                None,
            )
            .expect("FontConfig.sort");
            // for n in 0..(*fonts).nfont {
            //     let f = *(*fonts).fonts.add(n as usize);
            //     fontconfig::FcPatternPrint(f);
            // }

            let mut font = fonts.as_ref().fonts_slice()[0];
            let file: &core::ffi::CStr = font
                .as_mut()
                .get(fc::Pattern::KEY_FILE)
                .expect("FcPattern.get.file")
                .expect("FcPattern.get.not_exist.file");
            let file = file.to_owned();
            let index: core::ffi::c_int = font
                .as_mut()
                .get(fc::Pattern::KEY_INDEX)
                .expect("FcPattern.get.index")
                .expect("FcPattern.get.not_exist.index");

            (file, index)
        };

        let ui_default = unsafe {
            ft::new_face(lib.0, &font_file_path, face_index as _)
                .expect("FreeType.new_face.ui_default")
        };
        unsafe {
            ft::set_char_size(ui_default, 0, 12.0f32.to_f26dot6_lossy(), 0, dpi)
                .expect("FreeType.set_char_size.ui_default")
        }
        let ui_title_project_name = unsafe {
            ft::new_face(lib.0, &font_file_path, face_index as _)
                .expect("FreeType.Face.new.ui_title_project_name")
        };
        unsafe {
            ft::set_char_size(ui_title_project_name, 0, 10.0f32.to_f26dot6_lossy(), 0, dpi)
                .expect("FreeType.set_char_size.ui_title_project_name")
        }

        #[cfg(feature = "harfbuzz")]
        let ui_default_shaping = core::ptr::NonNull::new(unsafe {
            peridot_tp_harfbuzz::ffi::hb_ft_font_create_referenced(ui_default)
        })
        .expect("hb_ft_font_create_referenced.ui_default");
        #[cfg(feature = "harfbuzz")]
        let ui_title_project_name_shaping = core::ptr::NonNull::new(unsafe {
            peridot_tp_harfbuzz::ffi::hb_ft_font_create_referenced(ui_title_project_name)
        })
        .expect("hb_ft_font_create_referenced.ui_title_project_name");

        Self {
            ui_default,
            ui_title_project_name,
            #[cfg(feature = "harfbuzz")]
            ui_default_shaping,
            #[cfg(feature = "harfbuzz")]
            ui_title_project_name_shaping,
        }
    }

    #[cfg(target_os = "macos")]
    pub fn new() -> Self {
        let ui_default = apple_sdk_port::text::Font::new_ui(
            apple_sdk_port::text::UIFontType::System,
            12.0,
            None,
        );
        let ui_title_project_name = apple_sdk_port::text::Font::new_ui(
            apple_sdk_port::text::UIFontType::System,
            10.0,
            None,
        );

        Self {
            ui_default,
            ui_title_project_name,
        }
    }

    #[cfg(target_os = "macos")]
    #[inline]
    pub fn select(&self, category: FontID) -> &apple_sdk_port::text::Font {
        match category {
            FontID::UIDefault => &self.ui_default,
            FontID::UITitleProjectName => &self.ui_title_project_name,
        }
    }

    #[cfg(feature = "freetype")]
    #[inline]
    pub fn select(&self, category: FontID) -> ft::Face {
        match category {
            FontID::UIDefault => self.ui_default,
            FontID::UITitleProjectName => self.ui_title_project_name,
        }
    }

    #[cfg(feature = "harfbuzz")]
    #[inline]
    pub fn select_shaping(&self, category: FontID) -> *mut peridot_tp_harfbuzz::ffi::hb_font_t {
        match category {
            FontID::UIDefault => self.ui_default_shaping.as_ptr(),
            FontID::UITitleProjectName => self.ui_title_project_name_shaping.as_ptr(),
        }
    }

    #[cfg(windows)]
    #[inline(always)]
    pub const fn native_factory(&self) -> &windows::Win32::Graphics::DirectWrite::IDWriteFactory {
        &self.dw_factory
    }

    #[cfg(windows)]
    #[inline]
    pub fn select(
        &self,
        category: FontID,
    ) -> &windows::Win32::Graphics::DirectWrite::IDWriteTextFormat {
        match category {
            FontID::UIDefault => &self.ui_default,
            FontID::UITitleProjectName => &self.ui_title_project_name,
        }
    }
}
