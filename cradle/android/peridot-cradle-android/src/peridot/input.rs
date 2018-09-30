
use std::rc::Rc;

pub struct InputProcess {
    
}
pub trait InputProcessPlugin {
    fn on_start_handle(&mut self, processor: &Rc<InputProcess>);
}
impl InputProcess {
    pub fn new() -> Self {
        InputProcess {}
    }
    pub fn dispatch_message<M: InputMessage>(&self, msg: M) { msg.process(self); }
}

pub trait InputMessage: Sized {
    fn process(self, processor: &InputProcess);
}
