use peridot_tp_dbus::{self as dbus, MessageIterAppendLike};

pub const INTERFACE_NAME: &core::ffi::CStr = c"com.canonical.dbusmenu";

#[derive(Debug)]
pub struct GetLayoutRequest {
    pub parent_id: i32,
    pub recursion_depth: i32,
    pub property_names: Vec<std::ffi::CString>,
}
impl GetLayoutRequest {
    pub fn deserialize(iter: &mut dbus::MessageIter) -> Self {
        let parent_id = iter.try_get_i32().expect("parent:i");
        iter.next();
        let recursion_depth = iter.try_get_i32().expect("recursionDepth:i");
        iter.next();
        let mut property_names_iter = iter
            .try_begin_iter_array_content()
            .expect("propertyName:as");
        let mut property_names = Vec::new();
        while property_names_iter.has_next() {
            property_names.push(
                property_names_iter
                    .try_get_cstr()
                    .expect("propertyName[]:s")
                    .to_owned(),
            );
            property_names_iter.next();
        }

        Self {
            parent_id,
            recursion_depth,
            property_names,
        }
    }
}

pub struct GetLayoutReply<'s> {
    pub revision: u32,
    pub layout: Layout<'s>,
}
impl GetLayoutReply<'_> {
    pub fn serialize(
        &self,
        iter: &mut dbus::MessageIterAppend,
    ) -> Result<(), dbus::NotEnoughMemory> {
        iter.append_u32(self.revision)?;
        self.layout.serialize(iter)?;

        Ok(())
    }
}

pub struct AboutToShowReply {
    pub need_update: bool,
}
impl AboutToShowReply {
    pub fn serialize(
        &self,
        iter: &mut dbus::MessageIterAppend,
    ) -> Result<(), dbus::NotEnoughMemory> {
        iter.append_bool(self.need_update)?;

        Ok(())
    }
}

pub struct Layout<'s> {
    pub id: i32,
    pub properties: LayoutProperties<'s>,
    pub children: &'s [Layout<'s>],
}
impl Layout<'_> {
    pub fn serialize(
        &self,
        iter: &mut (impl dbus::MessageIterAppendLike + ?Sized),
    ) -> Result<(), dbus::NotEnoughMemory> {
        let mut iter = iter.open_struct_container()?;
        iter.append_i32(self.id)?;
        self.properties.serialize(&mut iter)?;
        let mut array_iter = iter.open_array_container(c"v")?;
        for child in self.children {
            child.serialize_as_variant(&mut array_iter.as_ref())?;
        }
        array_iter.close()?;
        iter.close()?;

        Ok(())
    }

    pub fn serialize_as_variant(
        &self,
        iter: &mut (impl dbus::MessageIterAppendLike + ?Sized),
    ) -> Result<(), dbus::NotEnoughMemory> {
        let mut iter = iter.open_variant_container(c"(ia{sv}av)")?;
        self.serialize(&mut iter)?;
        iter.close()?;

        Ok(())
    }
}

#[derive(Default, Debug)]
pub struct LayoutProperties<'s> {
    pub children_display: Option<&'s core::ffi::CStr>,
    pub label: Option<&'s core::ffi::CStr>,
    pub enabled: Option<bool>,
    pub visible: Option<bool>,
    pub icon_name: Option<&'s core::ffi::CStr>,
    pub shortcut: Option<&'s [&'s [&'s core::ffi::CStr]]>,
}
impl LayoutProperties<'_> {
    pub fn serialize(
        &self,
        iter: &mut (impl dbus::MessageIterAppendLike + ?Sized),
    ) -> Result<(), dbus::NotEnoughMemory> {
        let mut child_iter = iter.open_array_container(c"{sv}")?;
        if let Some(x) = self.children_display {
            let mut iter = child_iter.open_dict_entry_container()?;
            iter.append_cstr(c"children-display")?;
            let mut value_iter = iter.open_variant_container(c"s")?;
            value_iter.append_cstr(x)?;
            value_iter.close()?;
            iter.close()?;
        }
        if let Some(x) = self.label {
            let mut iter = child_iter.open_dict_entry_container()?;
            iter.append_cstr(c"label")?;
            let mut value_iter = iter.open_variant_container(c"s")?;
            value_iter.append_cstr(x)?;
            value_iter.close()?;
            iter.close()?;
        }
        if let Some(x) = self.enabled {
            let mut iter = child_iter.open_dict_entry_container()?;
            iter.append_cstr(c"enabled")?;
            let mut value_iter = iter.open_variant_container(c"b")?;
            value_iter.append_bool(x)?;
            value_iter.close()?;
            iter.close()?;
        }
        if let Some(x) = self.visible {
            let mut iter = child_iter.open_dict_entry_container()?;
            iter.append_cstr(c"visible")?;
            let mut value_iter = iter.open_variant_container(c"b")?;
            value_iter.append_bool(x)?;
            value_iter.close()?;
            iter.close()?;
        }
        if let Some(x) = self.icon_name {
            let mut iter = child_iter.open_dict_entry_container()?;
            iter.append_cstr(c"icon-name")?;
            let mut value_iter = iter.open_variant_container(c"s")?;
            value_iter.append_cstr(x)?;
            value_iter.close()?;
            iter.close()?;
        }
        if let Some(x) = self.shortcut {
            let mut iter = child_iter.open_dict_entry_container()?;
            iter.append_cstr(c"shortcut")?;
            let mut value_iter = iter.open_variant_container(c"aas")?;
            let mut value_array_iter = value_iter.open_array_container(c"as")?;
            for &es in x {
                let mut array_iter = value_array_iter.open_array_container(c"s")?;
                for e in es {
                    array_iter.append_cstr(e)?;
                }
                array_iter.close()?;
            }
            value_array_iter.close()?;
            value_iter.close()?;
            iter.close()?;
        }
        child_iter.close()?;

        Ok(())
    }
}
