//! Asset Interface

use std::io::Error as IOError;
use std::io::prelude::{Read, Seek};
use peridot_math as math;
use bedrock as br;
use peridot_coreutils as coreutils;

/// Asset Data Description in Filesystem.
pub trait LogicalAssetData: Sized
{
    /// Extension
    const EXT: &'static str;
}

/// Construct Asset Object from Readers.
pub trait FromAsset: LogicalAssetData
{
    /// Reading Error
    type Error: From<IOError>;
    /// Construct Asset from Reader
    fn from_asset<Asset: Read + Seek + 'static>(asset: Asset) -> Result<Self, Self::Error>;
}
/// Construct Streaming Asset Object with Readers.
pub trait FromStreamingAsset: LogicalAssetData
{
    /// Reading Error
    type Error: From<IOError>;
    /// Construct Streaming Asset with Reader.
    fn from_asset<Asset: Read + 'static>(asset: Asset) -> Result<Self, Self::Error>;
}
