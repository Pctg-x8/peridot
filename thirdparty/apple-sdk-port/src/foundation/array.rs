use crate::{NSCopying, NSObject, NSUInteger, ObjcObject, Owned};
use objc::{runtime::Object, *};

pub unsafe trait NSArray<ObjectType: 'static>: NSObject {
    #[inline(always)]
    fn count(&self) -> NSUInteger {
        unsafe { msg_send![self.as_id(), count] }
    }

    #[inline(always)]
    fn object_at_index(&self, index: NSUInteger) -> ObjectType {
        unsafe { msg_send![self.as_id(), objectAtIndex: index] }
    }
}

#[repr(transparent)]
pub struct NSArrayObject<ObjectType: 'static>(Object, core::marker::PhantomData<*const ObjectType>);
unsafe impl<ObjectType: 'static> ObjcObject for NSArrayObject<ObjectType> {
    #[inline(always)]
    fn as_id(&self) -> *mut Object {
        &self.0 as *const Object as *mut Object
    }

    #[inline(always)]
    fn as_id_mut(&mut self) -> *mut Object {
        &mut self.0 as *mut Object
    }
}
unsafe impl<ObjectType: 'static> NSObject for NSArrayObject<ObjectType> {}
unsafe impl<ObjectType: 'static> NSCopying for NSArrayObject<ObjectType> where ObjectType: NSCopying {}
unsafe impl<ObjectType: 'static> NSArray<ObjectType> for NSArrayObject<ObjectType> {}
impl<ObjectType: 'static> NSArrayObject<ObjectType> {
    #[inline(always)]
    pub fn new() -> Owned<Self> {
        unsafe { Owned::from_id_unchecked(msg_send![class!(NSArray), array]) }
    }

    #[inline(always)]
    pub fn from_objects(objects: &[ObjectType]) -> Owned<Self> {
        unsafe {
            Owned::from_id_unchecked(
                msg_send![class!(NSArray), arrayWithObjects: objects.as_ptr() count: objects.len() as NSUInteger],
            )
        }
    }
}

pub unsafe trait NSMutableArray<ObjectType: 'static>: NSArray<ObjectType> {
    #[inline(always)]
    fn add_object(&mut self, object: ObjectType) {
        unsafe { msg_send![self.as_id_mut(), addObject: object] }
    }

    #[inline(always)]
    fn insert_object(&mut self, object: ObjectType, index: NSUInteger) {
        unsafe { msg_send![self.as_id_mut(), insertObject: object atIndex: index] }
    }

    #[inline(always)]
    fn remove_last_object(&mut self) {
        unsafe { msg_send![self.as_id_mut(), removeLastObject] }
    }

    #[inline(always)]
    fn remove_object(&mut self, index: NSUInteger) {
        unsafe { msg_send![self.as_id_mut(), removeObjectAtIndex: index] }
    }

    #[inline(always)]
    fn replace_object(&mut self, index: NSUInteger, object: ObjectType) {
        unsafe { msg_send![self.as_id_mut(), replaceObjectAtIndex: index withObject: object] }
    }

    #[inline(always)]
    fn set_object(&mut self, obj: ObjectType, index: NSUInteger) {
        unsafe { msg_send![self.as_id_mut(), setObject: obj atIndexedSubscript: index] }
    }
}

#[repr(transparent)]
pub struct NSMutableArrayObject<ObjectType: 'static>(
    Object,
    core::marker::PhantomData<*const ObjectType>,
);
unsafe impl<ObjectType: 'static> ObjcObject for NSMutableArrayObject<ObjectType> {
    #[inline(always)]
    fn as_id(&self) -> *mut Object {
        &self.0 as *const Object as *mut Object
    }

    #[inline(always)]
    fn as_id_mut(&mut self) -> *mut Object {
        &mut self.0 as *mut Object
    }
}
unsafe impl<ObjectType: 'static> NSObject for NSMutableArrayObject<ObjectType> {}
unsafe impl<ObjectType: 'static> NSCopying for NSMutableArrayObject<ObjectType> where
    ObjectType: NSCopying
{
}
unsafe impl<ObjectType: 'static> NSArray<ObjectType> for NSMutableArrayObject<ObjectType> {}
unsafe impl<ObjectType: 'static> NSMutableArray<ObjectType> for NSMutableArrayObject<ObjectType> {}
impl<ObjectType: 'static> NSMutableArrayObject<ObjectType> {
    #[inline(always)]
    pub fn new() -> Owned<Self> {
        unsafe { Owned::from_id_unchecked(msg_send![class!(NSMutableArray), array]) }
    }

    #[inline(always)]
    pub fn from_objects(objects: &[ObjectType]) -> Owned<Self> {
        unsafe {
            Owned::from_id_unchecked(
                msg_send![class!(NSMutableArray), arrayWithObjects: objects.as_ptr() count: objects.len() as NSUInteger],
            )
        }
    }

    #[inline(always)]
    pub fn with_capacity(capacity: NSUInteger) -> Owned<Self> {
        unsafe {
            Owned::from_id_unchecked(msg_send![class!(NSMutableArray), arrayWithCapacity: capacity])
        }
    }
}
