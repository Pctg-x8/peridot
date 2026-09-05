use core::any::TypeId;

pub struct NonDropAnyTypeQueue {
    bytes: Vec<u8>,
    types: Vec<(&'static TypeId, usize)>,
}
impl NonDropAnyTypeQueue {
    pub fn new() -> Self {
        Self {
            bytes: Vec::new(),
            types: Vec::new(),
        }
    }

    pub const fn is_empty(&self) -> bool {
        self.types.is_empty()
    }

    pub fn clear(&mut self) {
        self.types.clear();
        self.bytes.clear();
    }

    pub fn push<T: 'static>(&mut self, feedback: T) {
        self.types
            .push((&const { TypeId::of::<T>() }, size_of::<T>()));
        let bytes_head = self.bytes.len();
        self.bytes
            .try_reserve(size_of::<T>())
            .expect("view_feedback_queue.push");
        unsafe {
            self.bytes.set_len(bytes_head + size_of::<T>());
            self.bytes
                .as_mut_ptr()
                .byte_add(bytes_head)
                .cast::<T>()
                .write_unaligned(feedback);
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&'static TypeId, *const ())> + '_ {
        self.types.iter().scan(0, |offset, type_id| {
            let bytes = unsafe { self.bytes.as_ptr().byte_add(*offset) };
            *offset += type_id.1;
            Some((type_id.0, bytes.cast()))
        })
    }
}
