use super::*;

pub type FT_List_Iterator =
    extern "system" fn(node: FT_ListNode, user: *mut core::ffi::c_void) -> FT_Error;
pub type FT_List_Destructor = extern "system" fn(
    memory: FT_Memory,
    data: *mut core::ffi::c_void,
    user: *mut core::ffi::c_void,
) -> FT_Error;

unsafe extern "system" {
    pub fn FT_List_Find(list: FT_List, data: *mut core::ffi::c_void) -> FT_ListNode;
    pub fn FT_List_Add(list: FT_List, node: FT_ListNode);
    pub fn FT_List_Insert(list: FT_List, node: FT_ListNode);
    pub fn FT_List_Remove(list: FT_List, node: FT_ListNode);
    pub fn FT_List_Up(list: FT_List, node: FT_ListNode);
    pub fn FT_List_Iterate(
        list: FT_List,
        iterator: FT_List_Iterator,
        user: *mut core::ffi::c_void,
    ) -> FT_Error;
    pub fn FT_List_Finalize(
        list: FT_List,
        destroy: FT_List_Destructor,
        memory: FT_Memory,
        user: *mut core::ffi::c_void,
    ) -> FT_Error;
}
