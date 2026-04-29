use crate::{
    Object, Owned,
    foundation::{Array, AttributedString, Data, Dictionary, Range, String},
    graphics::Path,
    raw::*,
};

pub const fn font_symbolic_trait() -> &'static String {
    unsafe { &*kCTFontSymbolicTrait.cast::<String>() }
}

pub const fn font_weight_trait() -> &'static String {
    unsafe { &*kCTFontWeightTrait.cast::<String>() }
}

pub const fn font_width_trait() -> &'static String {
    unsafe { &*kCTFontWidthTrait.cast::<String>() }
}

pub const fn font_slant_trait() -> &'static String {
    unsafe { &*kCTFontSlantTrait.cast::<String>() }
}

#[repr(transparent)]
pub struct FontDescriptor(__CTFontDescriptor);
impl Object for FontDescriptor {
    #[inline(always)]
    fn as_typeref(&self) -> CFTypeRef {
        &self.0 as *const _ as _
    }
}
impl FontDescriptor {
    #[inline(always)]
    pub fn from_attributes(attributes: &Dictionary<String, dyn Object>) -> Option<Owned<Self>> {
        unsafe {
            Owned::from_ptr(
                CTFontDescriptorCreateWithAttributes(attributes as *const _ as _) as *mut Self,
            )
        }
    }

    #[inline(always)]
    pub fn from_data(data: &Data) -> Option<Owned<Self>> {
        unsafe {
            Owned::from_ptr(
                CTFontManagerCreateFontDescriptorFromData(data as *const _ as _) as *mut Self,
            )
        }
    }

    pub const fn family_name_attribute() -> &'static String {
        unsafe { &*kCTFontFamilyNameAttribute.cast::<String>() }
    }

    pub const fn traits_attribute() -> &'static String {
        unsafe { &*kCTFontTraitsAttribute.cast::<String>() }
    }
}

#[repr(transparent)]
pub struct Font(__CTFont);
impl Object for Font {
    #[inline(always)]
    fn as_typeref(&self) -> CFTypeRef {
        &self.0 as *const _ as _
    }
}
impl Font {
    #[inline(always)]
    pub unsafe fn ref_from_untyped_ptr<'a>(p: *const core::ffi::c_void) -> &'a Self {
        unsafe { &*p.cast::<Self>() }
    }

    #[inline(always)]
    pub fn from_font_descriptor(
        descriptor: &FontDescriptor,
        size: CGFloat,
        matrix: Option<&CGAffineTransform>,
    ) -> Option<Owned<Self>> {
        unsafe {
            Owned::from_ptr(CTFontCreateWithFontDescriptor(
                descriptor as *const _ as _,
                size,
                matrix.map_or_else(core::ptr::null, |x| x as *const _),
            ) as *mut Self)
        }
    }

    #[inline(always)]
    pub fn new_ui(r#type: UIFontType, size: CGFloat, language: Option<&String>) -> Owned<Self> {
        unsafe {
            Owned::from_ptr_unchecked(CTFontCreateUIFontForLanguage(
                r#type as _,
                size,
                language.map_or(core::ptr::null(), |x| x as *const _ as _),
            ) as *mut Self)
        }
    }

    #[inline(always)]
    pub fn clone_with_attributes(
        &self,
        size: CGFloat,
        matrix: Option<&CGAffineTransform>,
        attributes: Option<&FontDescriptor>,
    ) -> Option<Owned<Self>> {
        unsafe {
            Owned::from_ptr(CTFontCreateCopyWithAttributes(
                self as *const _ as _,
                size,
                matrix.map_or_else(core::ptr::null, |x| x as *const _),
                attributes.map_or_else(core::ptr::null, |x| x as *const _ as _),
            ) as *mut Self)
        }
    }

    #[inline(always)]
    pub fn size(&self) -> CGFloat {
        unsafe { CTFontGetSize(&self.0) }
    }

    #[inline(always)]
    pub fn matrix(&self) -> CGAffineTransform {
        unsafe { CTFontGetMatrix(&self.0) }
    }

    #[inline(always)]
    pub fn ascent(&self) -> CGFloat {
        unsafe { CTFontGetAscent(&self.0) }
    }

    #[inline(always)]
    pub fn descent(&self) -> CGFloat {
        unsafe { CTFontGetDescent(&self.0) }
    }

    #[inline(always)]
    pub fn leading(&self) -> CGFloat {
        unsafe { CTFontGetLeading(&self.0) }
    }

    #[inline(always)]
    pub fn units_per_em(&self) -> core::ffi::c_uint {
        unsafe { CTFontGetUnitsPerEm(&self.0) }
    }

    #[inline(always)]
    pub fn glyph_for_character(&self, character: UniChar) -> Option<core::num::NonZero<CGGlyph>> {
        let mut glyph = core::mem::MaybeUninit::uninit();
        let r = unsafe { CTFontGetGlyphsForCharacters(&self.0, &character, glyph.as_mut_ptr(), 1) };
        if !r {
            None
        } else {
            Some(unsafe { core::num::NonZero::new_unchecked(glyph.assume_init()) })
        }
    }

    #[inline(always)]
    pub fn glyphs_for_characters(
        &self,
        characters: &[UniChar],
        glyphs: &mut [core::mem::MaybeUninit<CGGlyph>],
    ) -> bool {
        debug_assert!(glyphs.len() >= characters.len());

        unsafe {
            CTFontGetGlyphsForCharacters(
                &self.0,
                characters.as_ptr(),
                glyphs.as_mut_ptr() as _,
                characters.len() as _,
            )
        }
    }

    #[inline(always)]
    pub fn advance_for_glyph(&self, orientation: FontOrientation, glyph: CGGlyph) -> CGSize {
        let mut adv = core::mem::MaybeUninit::uninit();
        unsafe {
            CTFontGetAdvancesForGlyphs(&self.0, orientation as _, &glyph, adv.as_mut_ptr(), 1);
        }

        unsafe { adv.assume_init() }
    }

    #[inline(always)]
    pub fn advances_for_glyphs(
        &self,
        orientation: FontOrientation,
        glyphs: &[CGGlyph],
        advances: &mut [core::mem::MaybeUninit<CGSize>],
    ) -> core::ffi::c_double {
        debug_assert!(advances.len() >= glyphs.len());

        unsafe {
            CTFontGetAdvancesForGlyphs(
                &self.0,
                orientation as _,
                glyphs.as_ptr(),
                advances.as_mut_ptr() as _,
                glyphs.len() as _,
            )
        }
    }

    #[inline(always)]
    pub fn bounding_rects_for_glyphs(
        &self,
        orientation: FontOrientation,
        glyphs: &[CGGlyph],
        bounding_rects: &mut [core::mem::MaybeUninit<CGRect>],
    ) {
        debug_assert!(bounding_rects.len() >= glyphs.len());
        unsafe {
            CTFontGetBoundingRectsForGlyphs(
                &self.0,
                orientation as _,
                glyphs.as_ptr(),
                bounding_rects.as_mut_ptr().cast(),
                glyphs.len() as _,
            );
        }
    }

    #[inline(always)]
    pub fn bounding_rect_for_glyph(&self, orientation: FontOrientation, glyph: CGGlyph) -> CGRect {
        let mut rect = core::mem::MaybeUninit::uninit();
        unsafe {
            CTFontGetBoundingRectsForGlyphs(
                &self.0,
                orientation as _,
                &glyph,
                rect.as_mut_ptr(),
                1,
            );
        }

        unsafe { rect.assume_init() }
    }

    #[inline(always)]
    pub fn create_path_for_glyph(
        &self,
        glyph: CGGlyph,
        matrix: Option<&CGAffineTransform>,
    ) -> Option<Owned<Path>> {
        unsafe {
            Owned::from_ptr(CTFontCreatePathForGlyph(
                &self.0,
                glyph,
                matrix.map_or_else(core::ptr::null, |x| x as *const _),
            ) as *mut Path)
        }
    }
}

#[repr(u32)]
pub enum UIFontType {
    User = kCTFontUIFontUser,
    UserFixedPitch = kCTFontUIFontUserFixedPitch,
    System = kCTFontUIFontSystem,
    EmphasizedSystem = kCTFontUIFontEmphasizedSystem,
    SmallSystem = kCTFontUIFontSmallSystem,
    SmallEmphasizedSystem = kCTFontUIFontSmallEmphasizedSystem,
    MiniSystem = kCTFontUIFontMiniSystem,
    MiniEmphasizedSystem = kCTFontUIFontMiniEmphasizedSystem,
    Views = kCTFontUIFontViews,
    Application = kCTFontUIFontApplication,
    Label = kCTFontUIFontLabel,
    MenuTitle = kCTFontUIFontMenuTitle,
    MenuItem = kCTFontUIFontMenuItem,
    MenuItemMark = kCTFontUIFontMenuItemMark,
    MenuItemCmdKey = kCTFontUIFontMenuItemCmdKey,
    WindowTitle = kCTFontUIFontWindowTitle,
    PushButton = kCTFontUIFontPushButton,
    UtilityWindowTitle = kCTFontUIFontUtilityWindowTitle,
    AlertHeader = kCTFontUIFontAlertHeader,
    SystemDetail = kCTFontUIFontSystemDetail,
    EmphasizedSystemDetail = kCTFontUIFontEmphasizedSystemDetail,
    Toolbar = kCTFontUIFontToolbar,
    SmallToolbar = kCTFontUIFontSmallToolbar,
    Message = kCTFontUIFontMessage,
    Palette = kCTFontUIFontPalette,
    ToolTip = kCTFontUIFontToolTip,
    ControlContent = kCTFontUIFontControlContent,
}

#[repr(u32)]
pub enum FontOrientation {
    Default = kCTFontOrientationDefault,
    Horizontal = kCTFontOrientationHorizontal,
    Vertical = kCTFontOrientationVertical,
}

#[repr(transparent)]
pub struct Run(__CTRun);
impl Object for Run {
    #[inline(always)]
    fn as_typeref(&self) -> crate::raw::CFTypeRef {
        &self.0 as *const _ as _
    }
}
impl Run {
    #[inline(always)]
    pub fn attributes(&self) -> &Dictionary<String, dyn Object> {
        unsafe { &*CTRunGetAttributes(&self.0).cast::<Dictionary<String, dyn Object>>() }
    }

    #[inline(always)]
    pub fn glyph_count(&self) -> CFIndex {
        unsafe { CTRunGetGlyphCount(&self.0) }
    }

    #[inline(always)]
    pub fn glyphs_ptr(&self) -> *const CGGlyph {
        unsafe { CTRunGetGlyphsPtr(&self.0) }
    }

    #[inline(always)]
    pub fn positions(&self) -> *const CGPoint {
        unsafe { CTRunGetPositionsPtr(&self.0) }
    }

    #[inline(always)]
    pub fn advances(&self) -> *const CGSize {
        unsafe { CTRunGetAdvancesPtr(&self.0) }
    }

    #[inline(always)]
    pub fn string_indices(&self) -> *const CFIndex {
        unsafe { CTRunGetStringIndicesPtr(&self.0) }
    }

    #[inline(always)]
    pub fn typographic_bounds(
        &self,
        range: Range,
        ascent: Option<&mut core::mem::MaybeUninit<CGFloat>>,
        descent: Option<&mut core::mem::MaybeUninit<CGFloat>>,
        leading: Option<&mut core::mem::MaybeUninit<CGFloat>>,
    ) -> core::ffi::c_double {
        unsafe {
            CTRunGetTypographicBounds(
                &self.0,
                range,
                ascent.map_or(core::ptr::null_mut(), |x| x.as_mut_ptr()),
                descent.map_or(core::ptr::null_mut(), |x| x.as_mut_ptr()),
                leading.map_or(core::ptr::null_mut(), |x| x.as_mut_ptr()),
            )
        }
    }
}

#[repr(transparent)]
pub struct Line(__CTLine);
impl Object for Line {
    #[inline(always)]
    fn as_typeref(&self) -> crate::raw::CFTypeRef {
        &self.0 as *const _ as _
    }
}
impl Line {
    #[inline(always)]
    pub fn glyph_runs(&self) -> &Array<Run> {
        unsafe { &*CTLineGetGlyphRuns(&self.0).cast::<Array<Run>>() }
    }

    #[inline(always)]
    pub fn bound(&self, options: CTLineBoundsOptions) -> CGRect {
        unsafe { CTLineGetBoundsWithOptions(&self.0, options) }
    }

    #[inline(always)]
    pub fn typographic_bounds(
        &self,
        ascent: Option<&mut core::mem::MaybeUninit<CGFloat>>,
        descent: Option<&mut core::mem::MaybeUninit<CGFloat>>,
        leading: Option<&mut core::mem::MaybeUninit<CGFloat>>,
    ) -> core::ffi::c_double {
        unsafe {
            CTLineGetTypographicBounds(
                &self.0,
                ascent.map_or(core::ptr::null_mut(), |x| x.as_mut_ptr()),
                descent.map_or(core::ptr::null_mut(), |x| x.as_mut_ptr()),
                leading.map_or(core::ptr::null_mut(), |x| x.as_mut_ptr()),
            )
        }
    }

    #[inline(always)]
    pub fn string_index_for_position(&self, pos: CGPoint) -> Option<CFIndex> {
        match unsafe { CTLineGetStringIndexForPosition(&self.0, pos) } {
            v if v == kCFNotFound => None,
            v => Some(v),
        }
    }
}

#[repr(transparent)]
pub struct Frame(__CTFrame);
impl Object for Frame {
    #[inline(always)]
    fn as_typeref(&self) -> crate::raw::CFTypeRef {
        &self.0 as *const _ as _
    }
}
impl Frame {
    #[inline(always)]
    pub fn lines(&self) -> &Array<Line> {
        unsafe { &*CTFrameGetLines(&self.0).cast::<Array<Line>>() }
    }

    #[inline(always)]
    pub fn line_origins(&self, offset: CFIndex, sink: &mut [core::mem::MaybeUninit<CGPoint>]) {
        unsafe {
            CTFrameGetLineOrigins(
                &self.0,
                CFRange {
                    location: offset,
                    length: sink.len() as _,
                },
                sink.as_mut_ptr().cast(),
            );
        }
    }
}

#[repr(transparent)]
pub struct Framesetter(__CTFramesetter);
impl Object for Framesetter {
    #[inline(always)]
    fn as_typeref(&self) -> crate::raw::CFTypeRef {
        &self.0 as *const _ as _
    }
}
impl Framesetter {
    #[inline(always)]
    pub fn from_attributed_string(s: &AttributedString) -> Option<Owned<Self>> {
        unsafe {
            Owned::from_ptr(CTFramesetterCreateWithAttributedString(s as *const _ as _) as *mut Self)
        }
    }

    #[inline(always)]
    pub fn create_frame(
        &self,
        string_range: Range,
        path: &Path,
        frame_attributes: Option<&Dictionary<String, dyn Object>>,
    ) -> Option<Owned<Frame>> {
        unsafe {
            Owned::from_ptr(CTFramesetterCreateFrame(
                &self.0,
                string_range,
                path as *const _ as _,
                frame_attributes.map_or(core::ptr::null(), |x| x as *const _ as _),
            ) as *mut Frame)
        }
    }
}
