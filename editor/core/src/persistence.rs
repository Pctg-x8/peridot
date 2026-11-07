//! State Persistence

use std::io::{Read, Write};

use shared::{LogicalUnit, PixelsUnit, Point, Rect, Size};

use crate::WindowPersistentStateNativeGeometryUnit;

#[derive(Debug)]
pub struct PersistStateWindowData {
    pub main: WindowState,
    pub sub: Vec<WindowState>,
}
impl PersistStateWindowData {
    pub fn serialize(&self, w: &mut (impl Write + ?Sized)) -> Result<(), SerializeError> {
        self.main.serialize(w)?;
        w.write_all(&usize::to_ne_bytes(self.sub.len()))?;
        for sub in &self.sub {
            sub.serialize(w)?;
        }

        Ok(())
    }

    pub fn deserialize(r: &mut (impl Read + ?Sized)) -> Result<Self, DeserializeError> {
        let main = WindowState::deserialize(r)?;
        let mut sub_len = 0usize;
        r.read_exact(unsafe {
            core::mem::transmute::<_, &mut [u8; size_of::<usize>()]>(&mut sub_len)
        })?;
        let mut sub = Vec::with_capacity(sub_len);
        for _ in 0..sub_len {
            sub.push(WindowState::deserialize(r)?);
        }
        Ok(Self { main, sub })
    }
}

#[derive(Debug)]
pub struct WindowState {
    pub geometry: WindowGeometryState,
    pub dock: DockState,
}
impl WindowState {
    fn serialize(&self, w: &mut (impl Write + ?Sized)) -> Result<(), SerializeError> {
        self.geometry.serialize(w)?;
        self.dock.serialize(w)?;

        Ok(())
    }

    fn deserialize(r: &mut (impl Read + ?Sized)) -> Result<Self, DeserializeError> {
        let geometry = WindowGeometryState::deserialize(r)?;
        let dock = DockState::deserialize(r)?;

        Ok(Self { geometry, dock })
    }
}

#[derive(Debug, Clone)]
pub enum WindowGeometryState {
    Maximized {
        monitor_index: usize,
    },
    Restored {
        rect: Rect<WindowPersistentStateNativeGeometryUnit>,
    },
}
impl WindowGeometryState {
    fn serialize(&self, w: &mut (impl Write + ?Sized)) -> Result<(), SerializeError> {
        match self {
            Self::Maximized { monitor_index } => {
                w.write_all(&[0x01])?;
                w.write_all(&usize::to_ne_bytes(*monitor_index))?;
            }
            Self::Restored { rect } => {
                w.write_all(&[0x02])?;
                rect.serialize(w)?;
            }
        }

        Ok(())
    }

    fn deserialize(r: &mut (impl Read + ?Sized)) -> Result<Self, DeserializeError> {
        let mut buf = [0u8; 1];
        r.read_exact(&mut buf)?;
        match buf[0] {
            0x01 => {
                let mut monitor_index = 0usize;
                r.read_exact(unsafe {
                    core::mem::transmute::<_, &mut [u8; size_of::<usize>()]>(&mut monitor_index)
                })?;
                Ok(Self::Maximized { monitor_index })
            }
            0x02 => {
                let rect = PersistStateSerializable::deserialize(r)?;

                Ok(Self::Restored { rect })
            }
            _ => Err(DeserializeError::InvalidFormat),
        }
    }
}

#[derive(Debug)]
pub enum DockState {
    Filled {
        content_ids: Vec<String>,
        active_index: usize,
    },
    Splitted {
        direction: DockDirection,
        content: Box<DockState>,
        rest: Box<DockState>,
    },
}
impl DockState {
    fn serialize(&self, w: &mut (impl Write + ?Sized)) -> Result<(), SerializeError> {
        match self {
            Self::Filled {
                content_ids,
                active_index,
            } => {
                w.write_all(&[0x01])?;
                w.write_all(&usize::to_ne_bytes(content_ids.len()))?;
                for id in content_ids {
                    w.write_all(&usize::to_ne_bytes(id.len()))?;
                    w.write_all(id.as_bytes())?;
                }
                w.write_all(&usize::to_ne_bytes(*active_index))?;
            }
            Self::Splitted {
                direction,
                content,
                rest,
            } => {
                w.write_all(&[0x02])?;
                direction.serialize(w)?;
                content.serialize(w)?;
                rest.serialize(w)?;
            }
        }

        Ok(())
    }

    fn deserialize(r: &mut (impl Read + ?Sized)) -> Result<Self, DeserializeError> {
        let mut buf = [0u8; 1];
        r.read_exact(&mut buf)?;
        match buf[0] {
            0x01 => {
                let mut content_count = 0usize;
                r.read_exact(unsafe {
                    core::mem::transmute::<_, &mut [u8; size_of::<usize>()]>(&mut content_count)
                })?;
                let mut content_ids = Vec::with_capacity(content_count);
                for _ in 0..content_count {
                    let mut id_length = 0usize;
                    r.read_exact(unsafe {
                        core::mem::transmute::<_, &mut [u8; size_of::<usize>()]>(&mut id_length)
                    })?;
                    let mut id = Vec::with_capacity(id_length);
                    r.read_exact(unsafe { core::mem::transmute(id.spare_capacity_mut()) })?;
                    unsafe {
                        id.set_len(id_length);
                    }
                    content_ids.push(unsafe { String::from_utf8_unchecked(id) });
                }
                let mut active_index = 0usize;
                r.read_exact(unsafe {
                    core::mem::transmute::<_, &mut [u8; size_of::<usize>()]>(&mut active_index)
                })?;

                Ok(Self::Filled {
                    content_ids,
                    active_index,
                })
            }
            0x02 => {
                let direction = DockDirection::deserialize(r)?;
                let content = Self::deserialize(r)?;
                let rest = Self::deserialize(r)?;

                Ok(Self::Splitted {
                    direction,
                    content: Box::new(content),
                    rest: Box::new(rest),
                })
            }
            _ => Err(DeserializeError::InvalidFormat),
        }
    }
}

#[derive(Debug)]
pub enum DockDirection {
    Left(f32),
    Right(f32),
    Top(f32),
    Bottom(f32),
}
impl DockDirection {
    fn serialize(&self, w: &mut (impl Write + ?Sized)) -> Result<(), SerializeError> {
        match self {
            &Self::Left(x) => {
                w.write_all(&[0x01])?;
                w.write_all(&f32::to_ne_bytes(x))?;
            }
            &Self::Right(x) => {
                w.write_all(&[0x02])?;
                w.write_all(&f32::to_ne_bytes(x))?;
            }
            &Self::Top(x) => {
                w.write_all(&[0x03])?;
                w.write_all(&f32::to_ne_bytes(x))?;
            }
            &Self::Bottom(x) => {
                w.write_all(&[0x04])?;
                w.write_all(&f32::to_ne_bytes(x))?;
            }
        }

        Ok(())
    }

    fn deserialize(r: &mut (impl Read + ?Sized)) -> Result<Self, DeserializeError> {
        let mut buf = [0u8; 1];
        r.read_exact(&mut buf)?;
        match buf[0] {
            0x01 => {
                let mut buf = [0u8; size_of::<f32>()];
                r.read_exact(&mut buf)?;
                Ok(Self::Left(f32::from_ne_bytes(buf)))
            }
            0x02 => {
                let mut buf = [0u8; size_of::<f32>()];
                r.read_exact(&mut buf)?;
                Ok(Self::Right(f32::from_ne_bytes(buf)))
            }
            0x03 => {
                let mut buf = [0u8; size_of::<f32>()];
                r.read_exact(&mut buf)?;
                Ok(Self::Top(f32::from_ne_bytes(buf)))
            }
            0x04 => {
                let mut buf = [0u8; size_of::<f32>()];
                r.read_exact(&mut buf)?;
                Ok(Self::Bottom(f32::from_ne_bytes(buf)))
            }
            _ => Err(DeserializeError::InvalidFormat),
        }
    }
}

#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum SerializeError {
    #[error(transparent)]
    IO(#[from] std::io::Error),
}

#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum DeserializeError {
    #[error(transparent)]
    IO(#[from] std::io::Error),
    #[error("persist_state_deserialize_error.invalid_format")]
    InvalidFormat,
}

trait PersistStateSerializable: Sized {
    fn serialize(&self, w: &mut (impl Write + ?Sized)) -> Result<(), SerializeError>;
    fn deserialize(r: &mut (impl Read + ?Sized)) -> Result<Self, DeserializeError>;
}
impl PersistStateSerializable for Rect<LogicalUnit> {
    fn serialize(&self, w: &mut (impl Write + ?Sized)) -> Result<(), SerializeError> {
        w.write_all(&f32::to_ne_bytes(self.left))?;
        w.write_all(&f32::to_ne_bytes(self.top))?;
        w.write_all(&f32::to_ne_bytes(self.width))?;
        w.write_all(&f32::to_ne_bytes(self.height))?;
        Ok(())
    }

    fn deserialize(r: &mut (impl Read + ?Sized)) -> Result<Self, DeserializeError> {
        let mut x = 0f32;
        let mut y = 0f32;
        let mut width = 0f32;
        let mut height = 0f32;
        r.read_exact(unsafe { core::mem::transmute::<_, &mut [u8; size_of::<f32>()]>(&mut x) })?;
        r.read_exact(unsafe { core::mem::transmute::<_, &mut [u8; size_of::<f32>()]>(&mut y) })?;
        r.read_exact(unsafe {
            core::mem::transmute::<_, &mut [u8; size_of::<f32>()]>(&mut width)
        })?;
        r.read_exact(unsafe {
            core::mem::transmute::<_, &mut [u8; size_of::<f32>()]>(&mut height)
        })?;

        Ok(Self::from_lt_size(
            Point::new_logical(x, y),
            Size::new_logical(width, height),
        ))
    }
}
impl PersistStateSerializable for Rect<PixelsUnit> {
    fn serialize(&self, w: &mut (impl Write + ?Sized)) -> Result<(), SerializeError> {
        w.write_all(&i32::to_ne_bytes(self.left))?;
        w.write_all(&i32::to_ne_bytes(self.top))?;
        w.write_all(&u32::to_ne_bytes(self.width))?;
        w.write_all(&u32::to_ne_bytes(self.height))?;
        Ok(())
    }

    fn deserialize(r: &mut (impl Read + ?Sized)) -> Result<Self, DeserializeError> {
        let mut left = 0i32;
        let mut top = 0i32;
        let mut width = 0u32;
        let mut height = 0u32;
        r.read_exact(unsafe { core::mem::transmute::<_, &mut [u8; size_of::<i32>()]>(&mut left) })?;
        r.read_exact(unsafe { core::mem::transmute::<_, &mut [u8; size_of::<i32>()]>(&mut top) })?;
        r.read_exact(unsafe {
            core::mem::transmute::<_, &mut [u8; size_of::<u32>()]>(&mut width)
        })?;
        r.read_exact(unsafe {
            core::mem::transmute::<_, &mut [u8; size_of::<u32>()]>(&mut height)
        })?;

        Ok(Self::from_lt_size(
            Point::new_pixels(left, top),
            Size::new_pixels(width, height),
        ))
    }
}
