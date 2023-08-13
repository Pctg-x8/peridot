//! fontconfig Font Provider impl

use fontconfig::FcRange;

use crate::{Font, FontConstructionError, FontProvider, TTFBlob};

use super::super::ft_drivers;
use fontconfig::*;

pub struct FontconfigFontProvider {
    ft: ft_drivers::System,
    fc: Config,
}
impl FontProvider for FontconfigFontProvider {
    fn new() -> Result<Self, FontConstructionError> {
        Ok(Self {
            ft: ft_drivers::System::new(),
            fc: Config::init(),
        })
    }

    fn best_match(
        &self,
        family_name: &str,
        properties: &crate::FontProperties,
        size: f32,
    ) -> Result<crate::Font, crate::FontConstructionError> {
        let c_family_name = std::ffi::CString::new(family_name).expect("FFI Conversion failure");
        let mut pat = Pattern::with_name_weight_style_size(
            c_family_name.as_ptr() as *const _,
            properties.weight as _,
            properties.italic,
            size,
        )
        .ok_or(FontConstructionError::SysAPICallError("FcPatternBuild"))?;
        self.fc.substitute_pattern(&mut pat);
        pat.default_substitute();
        let fonts = self
            .fc
            .sort_fonts(&pat)
            .ok_or(FontConstructionError::SysAPICallError("FcFontSort"))?;

        let group_desc = fonts
            .iter()
            .map(|f| {
                let font_path = f
                    .get_filepath()
                    .ok_or(FontConstructionError::SysAPICallError("FcPatternGetString"))?;
                let face_index =
                    f.get_face_index()
                        .ok_or(FontConstructionError::SysAPICallError(
                            "FcPatternGetInteger",
                        ))?;

                Ok(ft_drivers::FaceGroupEntry::unloaded(
                    font_path,
                    face_index as _,
                ))
            })
            .collect::<Result<_, FontConstructionError>>()?;
        let face = self.ft.new_face_group(group_desc);
        face.set_size(size);

        Ok(Font(face, size))
    }

    fn load<NL: peridot::NativeLinker>(
        &self,
        e: &peridot::Engine<NL>,
        asset_path: &str,
        size: f32,
    ) -> Result<crate::Font, crate::FontConstructionError> {
        let a: TTFBlob = e.load(asset_path)?;
        let f = self
            .ft
            .new_face_from_mem(&a.0, 0)
            .map_err(FontConstructionError::FT2)?;
        let face = self
            .ft
            .new_face_group(vec![ft_drivers::FaceGroupEntry::LoadedMem(f, a.0.into())]);
        face.set_size(size);

        Ok(Font(face, size))
    }
}

struct Config {
    ptr: std::ptr::NonNull<FcConfig>,
}
impl Config {
    fn init() -> Self {
        unsafe {
            FcInit();

            Config {
                ptr: std::ptr::NonNull::new_unchecked(FcConfigGetCurrent()),
            }
        }
    }

    fn substitute_pattern(&self, pat: &mut Pattern) {
        unsafe {
            FcConfigSubstitute(self.ptr.as_ptr(), pat.ptr.as_ptr(), FcMatchPattern);
        }
    }

    fn match_font(&self, pat: &Pattern) -> Option<Pattern> {
        let mut _res = 0;
        let ptr = unsafe { FcFontMatch(self.ptr.as_ptr(), pat.ptr.as_ptr(), &mut _res) };

        std::ptr::NonNull::new(ptr).map(|ptr| Pattern { ptr })
    }

    fn sort_fonts(&self, pat: &Pattern) -> Option<FontSet> {
        let mut _res = 0;
        let ptr = unsafe {
            FcFontSort(
                self.ptr.as_ptr(),
                pat.ptr.as_ptr(),
                FcTrue,
                std::ptr::null_mut(),
                &mut _res,
            )
        };

        std::ptr::NonNull::new(ptr).map(|ptr| FontSet { ptr })
    }
}

struct PatternRef {
    ptr: std::ptr::NonNull<FcPattern>,
}
impl PatternRef {
    fn get_filepath(&self) -> Option<&std::ffi::CStr> {
        let mut file = std::mem::MaybeUninit::uninit();
        let res = unsafe {
            FcPatternGetString(
                self.ptr.as_ptr(),
                FC_FILE.as_ptr() as _,
                0,
                file.as_mut_ptr(),
            )
        };
        if res == FcResultMatch {
            Some(unsafe { std::ffi::CStr::from_ptr(file.assume_init() as *const _) })
        } else {
            None
        }
    }

    fn get_face_index(&self) -> Option<u32> {
        let mut index = std::mem::MaybeUninit::uninit();
        let res = unsafe {
            FcPatternGetInteger(
                self.ptr.as_ptr(),
                FC_INDEX.as_ptr() as _,
                0,
                index.as_mut_ptr(),
            )
        };
        if res == FcResultMatch {
            Some(unsafe { index.assume_init() as _ })
        } else {
            None
        }
    }
}

struct Pattern {
    ptr: std::ptr::NonNull<FcPattern>,
}
impl Pattern {
    /*pub fn parse_name(name: *const u8) -> Option<Self>
    {
        NonNull::new(unsafe { FcNameParse(name) }).map(|ptr| Pattern { ptr })
    }*/

    fn with_name_weight_style_size(
        name: *const u8,
        ot_weight: u32,
        italic: bool,
        size: f32,
    ) -> Option<Self> {
        let size_range = Range::new_double(size as _, size as _).expect("Range creation failed");
        let ptr = unsafe {
            FcPatternBuild(
                std::ptr::null_mut(),
                FC_FAMILY.as_ptr(),
                FcTypeString,
                name,
                FC_WEIGHT.as_ptr(),
                FcTypeInteger,
                FcWeightFromOpenType(ot_weight as _) as FcChar32,
                FC_SLANT.as_ptr(),
                FcTypeInteger,
                if italic { FC_SLANT_ITALIC } else { 0 } as FcChar32,
                FC_SIZE.as_ptr(),
                FcTypeRange,
                size_range.0.as_ptr(),
                std::ptr::null::<FcChar8>(),
            )
        };

        std::ptr::NonNull::new(ptr).map(|ptr| Pattern { ptr })
    }

    fn default_substitute(&mut self) {
        unsafe {
            FcDefaultSubstitute(self.ptr.as_ptr());
        }
    }

    fn dump(&self) {
        unsafe {
            FcPatternPrint(self.ptr.as_ptr());
        }
    }
}
impl Drop for Pattern {
    fn drop(&mut self) {
        unsafe {
            FcPatternDestroy(self.ptr.as_ptr());
        }
    }
}
impl std::ops::Deref for Pattern {
    type Target = PatternRef;
    fn deref(&self) -> &PatternRef {
        unsafe { std::mem::transmute(self) }
    }
}

struct FontSet {
    ptr: std::ptr::NonNull<FcFontSet>,
}
impl FontSet {
    fn dump(&self) {
        unsafe {
            FcFontSetPrint(self.ptr.as_ptr());
        }
    }

    fn iter(&self) -> impl Iterator<Item = PatternRef> {
        let sliced = unsafe {
            std::slice::from_raw_parts(self.ptr.as_ref().fonts, self.ptr.as_ref().nfont as _)
        };

        sliced.into_iter().map(|&p| unsafe {
            PatternRef {
                ptr: std::ptr::NonNull::new_unchecked(p),
            }
        })
    }
}
impl Drop for FontSet {
    fn drop(&mut self) {
        unsafe {
            FcFontSetDestroy(self.ptr.as_ptr());
        }
    }
}

#[repr(transparent)]
struct Range(std::ptr::NonNull<FcRange>);
impl Range {
    fn new_double(begin: f64, end: f64) -> Option<Self> {
        std::ptr::NonNull::new(unsafe { FcRangeCreateDouble(begin as _, end as _) }).map(Self)
    }
}
impl Drop for Range {
    fn drop(&mut self) {
        unsafe {
            FcRangeDestroy(self.0.as_ptr());
        }
    }
}
