use objc::{runtime::Object, *};

use crate::{NSCopying, NSObject, NSUInteger, ObjcObject, Owned};

pub unsafe trait NSDictionary<KeyType: 'static, ObjectType: 'static>: NSObject {
    #[inline(always)]
    fn count(&self) -> NSUInteger {
        unsafe { msg_send![self.as_id(), count] }
    }

    #[inline(always)]
    fn object_for_key(&self, key: KeyType) -> ObjectType {
        unsafe { msg_send![self.as_id(), objectForKey: key] }
    }
}

#[repr(transparent)]
pub struct NSDictionaryObject<KeyType: 'static, ObjectType: 'static>(
    Object,
    core::marker::PhantomData<*const (KeyType, ObjectType)>,
);
unsafe impl<KeyType: 'static, ObjectType: 'static> ObjcObject
    for NSDictionaryObject<KeyType, ObjectType>
{
    #[inline(always)]
    fn as_id(&self) -> *mut Object {
        &self.0 as *const Object as *mut Object
    }

    #[inline(always)]
    fn as_id_mut(&mut self) -> *mut Object {
        &mut self.0 as *mut Object
    }
}
unsafe impl<KeyType: 'static, ObjectType: 'static> NSObject
    for NSDictionaryObject<KeyType, ObjectType>
{
}
unsafe impl<KeyType: 'static, ObjectType: 'static> NSCopying
    for NSDictionaryObject<KeyType, ObjectType>
where
    KeyType: NSCopying,
    ObjectType: NSCopying,
{
}
unsafe impl<KeyType: 'static, ObjectType: 'static> NSDictionary<KeyType, ObjectType>
    for NSDictionaryObject<KeyType, ObjectType>
{
}
impl<KeyType: 'static, ObjectType: 'static> NSDictionaryObject<KeyType, ObjectType> {
    #[inline(always)]
    pub fn new() -> Owned<Self> {
        unsafe { Owned::from_id_unchecked(msg_send![class!(NSDictionary), dictionary]) }
    }

    #[inline(always)]
    pub fn with_objects_and_keys(
        objects: &[ObjectType],
        keys: &[KeyType],
        count: NSUInteger,
    ) -> Owned<Self> {
        unsafe {
            Owned::from_id_unchecked(msg_send![
                class!(NSDictionary),
                dictionaryWithObjects: objects.as_ptr()
                forKeys: keys.as_ptr()
                count: count
            ])
        }
    }
}

pub unsafe trait NSMutableDictionary<KeyType: 'static, ObjectType: 'static>:
    NSDictionary<KeyType, ObjectType>
{
    #[inline(always)]
    fn remove_object_for_key(&mut self, key: KeyType) {
        unsafe { msg_send![self.as_id_mut(), removeObjectForKey: key] }
    }

    #[inline(always)]
    fn set_object_for_key(&mut self, object: ObjectType, key: KeyType) {
        unsafe { msg_send![self.as_id_mut(), setObject: object forKey: key] }
    }
}

#[repr(transparent)]
pub struct NSMutableDictionaryObject<KeyType: 'static, ObjectType: 'static>(
    Object,
    core::marker::PhantomData<*const (KeyType, ObjectType)>,
);
unsafe impl<KeyType: 'static, ObjectType: 'static> ObjcObject
    for NSMutableDictionaryObject<KeyType, ObjectType>
{
    #[inline(always)]
    fn as_id(&self) -> *mut Object {
        &self.0 as *const Object as *mut Object
    }

    #[inline(always)]
    fn as_id_mut(&mut self) -> *mut Object {
        &mut self.0 as *mut Object
    }
}
unsafe impl<KeyType: 'static, ObjectType: 'static> NSObject
    for NSMutableDictionaryObject<KeyType, ObjectType>
{
}
unsafe impl<KeyType: 'static, ObjectType: 'static> NSCopying
    for NSMutableDictionaryObject<KeyType, ObjectType>
where
    KeyType: NSCopying,
    ObjectType: NSCopying,
{
}
unsafe impl<KeyType: 'static, ObjectType: 'static> NSDictionary<KeyType, ObjectType>
    for NSMutableDictionaryObject<KeyType, ObjectType>
{
}
unsafe impl<KeyType: 'static, ObjectType: 'static> NSMutableDictionary<KeyType, ObjectType>
    for NSMutableDictionaryObject<KeyType, ObjectType>
{
}
impl<KeyType: 'static, ObjectType: 'static> NSMutableDictionaryObject<KeyType, ObjectType> {
    #[inline(always)]
    pub fn new() -> Owned<Self> {
        unsafe { Owned::from_id_unchecked(msg_send![class!(NSMutableDictionary), dictionary]) }
    }

    #[inline(always)]
    pub fn with_objects_and_keys(
        objects: &[ObjectType],
        keys: &[KeyType],
        count: NSUInteger,
    ) -> Owned<Self>
    where
        KeyType: NSCopying,
    {
        unsafe {
            Owned::from_id_unchecked(msg_send![
                class!(NSMutableDictionary),
                dictionaryWithObjects: objects.as_ptr()
                forKeys: keys.as_ptr()
                count: count
            ])
        }
    }

    #[inline(always)]
    pub fn with_capacity(capacity: NSUInteger) -> Owned<Self> {
        unsafe {
            Owned::from_id_unchecked(msg_send![
                class!(NSMutableDictionary),
                dictionaryWithCapacity: capacity
            ])
        }
    }
}
