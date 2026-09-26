use core::mem::MaybeUninit;

use peridot::{
    AssetBlob, AssetBlobAsync, LogicalAssetData,
    native_io::{RandomBlobAsyncReadSeekAdapter, RandomBlobReadSeekAdapter},
};

use crate::{
    Attribute, AttributeData, Header, IndexStream, IndexType, VertexStream, try_validate_signature,
    try_validate_signature_async,
};

/// アセット共通データ（これはインスタンス化できない [`Asset`]もしくは[`AssetAsync`]を使うこと）
pub struct AssetCore;
impl LogicalAssetData for AssetCore {
    const EXT: &'static str = "pa1-mesh";
}

/// メッシュアセット
pub struct Asset<Internal: AssetBlob> {
    /// ヘッダ情報
    pub header: Header,
    /// 頂点インデックス
    pub index_stream: IndexStream,
    /// 頂点データ
    pub vertex_streams: Vec<(VertexStream, Vec<(Attribute, AttributeData)>)>,
    /// バッファ読み取り用のファイルポインタ
    pub internal: Internal,
}
impl<Internal: AssetBlob> LogicalAssetData for Asset<Internal> {
    const EXT: &'static str = "pa1-mesh";
}
impl<Internal: AssetBlob> Asset<Internal> {
    /// アセットを開いて読み込み
    pub fn open(asset: Internal) -> Result<Self, std::io::Error> {
        let mut reader = RandomBlobReadSeekAdapter::new(asset);
        let needs_swap = try_validate_signature(&mut reader)?
            .expect("invalid asset")
            .needs_swap();
        let header = Header::deserialize(&mut reader)?;
        let index_stream = IndexStream::deserialize(&mut reader, needs_swap)?;
        let mut vertex_streams = Vec::with_capacity(header.vertex_stream_count as _);
        for _ in 0..header.vertex_stream_count {
            let vertex_stream = VertexStream::deserialize(&mut reader, needs_swap)?;
            let mut attributes = Vec::with_capacity(vertex_stream.attribute_count as _);
            for _ in 0..vertex_stream.attribute_count {
                let attribute = Attribute::deserialize(&mut reader)?;
                let attribute_data = AttributeData::deserialize(&mut reader, needs_swap)?;
                attributes.push((attribute, attribute_data));
            }
            vertex_streams.push((vertex_stream, attributes));
        }

        Ok(Self {
            header,
            index_stream,
            vertex_streams,
            internal: reader.into_inner(),
        })
    }

    /// インデックス情報を持つか？
    pub const fn has_index(&self) -> bool {
        !matches!(self.index_stream, IndexStream::None)
    }

    /// インデックスの数
    pub const fn index_count(&self) -> u32 {
        match self.index_stream {
            IndexStream::None => 0,
            IndexStream::Stream {
                ref buffer,
                index_type: IndexType::UInt16,
            } => buffer.byte_length / 2,
            IndexStream::Stream {
                ref buffer,
                index_type: IndexType::UInt32,
            } => buffer.byte_length / 4,
        }
    }

    /// インデックスストリームのバイト長
    pub const fn index_stream_byte_length(&self) -> u64 {
        match self.index_stream {
            IndexStream::None => 0,
            IndexStream::Stream { ref buffer, .. } => buffer.byte_length as u64,
        }
    }

    /// 頂点ストリームのバイト長
    pub fn vertex_stream_byte_length(&self, stream_index: usize) -> u64 {
        match self.vertex_streams.get(stream_index) {
            None => 0,
            Some((VertexStream { buffer, .. }, _)) => buffer.byte_length as u64,
        }
    }

    /// インデックスバッファを読み込む
    pub fn read_index_buffer_into(
        &self,
        sink: &mut [MaybeUninit<u8>],
    ) -> Result<(), std::io::Error> {
        match self.index_stream {
            IndexStream::None => Ok(()),
            IndexStream::Stream { ref buffer, .. } => {
                self.internal.read_exact(buffer.content_location, sink)
            }
        }
    }

    /// 頂点バッファを読み込む
    pub fn read_vertex_buffer_into(
        &self,
        stream_index: usize,
        sink: &mut [MaybeUninit<u8>],
    ) -> Result<(), std::io::Error> {
        match self.vertex_streams.get(stream_index) {
            None => Ok(()),
            Some((VertexStream { buffer, .. }, _)) => {
                self.internal.read_exact(buffer.content_location, sink)
            }
        }
    }
}

pub struct AssetAsync<Internal: AssetBlobAsync> {
    pub header: Header,
    pub index_stream: IndexStream,
    pub vertex_streams: Vec<(VertexStream, Vec<(Attribute, AttributeData)>)>,
    internal: Internal,
}
impl<Internal: AssetBlobAsync> LogicalAssetData for AssetAsync<Internal> {
    const EXT: &'static str = "pa1-mesh";
}
impl<Internal: AssetBlobAsync> AssetAsync<Internal> {
    pub async fn open(asset: Internal) -> Result<Self, std::io::Error> {
        let mut reader1 = RandomBlobAsyncReadSeekAdapter::new(&asset);
        let mut reader = unsafe { core::pin::Pin::new_unchecked(&mut reader1) };

        let needs_swap = try_validate_signature_async(reader.as_mut())
            .await?
            .expect("invalid asset")
            .needs_swap();
        let header = Header::deserialize_async(reader.as_mut()).await?;
        let index_stream = IndexStream::deserialize_async(reader.as_mut(), needs_swap).await?;
        let mut vertex_streams = Vec::with_capacity(header.vertex_stream_count as _);
        for _ in 0..header.vertex_stream_count {
            let vertex_stream =
                VertexStream::deserialize_async(reader.as_mut(), needs_swap).await?;
            let mut attributes = Vec::with_capacity(vertex_stream.attribute_count as _);
            for _ in 0..vertex_stream.attribute_count {
                let attribute = Attribute::deserialize_async(reader.as_mut()).await?;
                let attribute_data =
                    AttributeData::deserialize_async(reader.as_mut(), needs_swap).await?;
                attributes.push((attribute, attribute_data));
            }
            vertex_streams.push((vertex_stream, attributes));
        }

        drop(reader1);
        Ok(Self {
            header,
            index_stream,
            vertex_streams,
            internal: asset,
        })
    }

    /// インデックス情報を持つか？
    pub const fn has_index(&self) -> bool {
        !matches!(self.index_stream, IndexStream::None)
    }

    /// インデックスの数
    pub const fn index_count(&self) -> u32 {
        match self.index_stream {
            IndexStream::None => 0,
            IndexStream::Stream {
                ref buffer,
                index_type: IndexType::UInt16,
            } => buffer.byte_length / 2,
            IndexStream::Stream {
                ref buffer,
                index_type: IndexType::UInt32,
            } => buffer.byte_length / 4,
        }
    }

    /// インデックスストリームのバイト長
    pub const fn index_stream_byte_length(&self) -> u64 {
        match self.index_stream {
            IndexStream::None => 0,
            IndexStream::Stream { ref buffer, .. } => buffer.byte_length as u64,
        }
    }

    /// 頂点ストリームのバイト長
    pub fn vertex_stream_byte_length(&self, stream_index: usize) -> u64 {
        match self.vertex_streams.get(stream_index) {
            None => 0,
            Some((VertexStream { buffer, .. }, _)) => buffer.byte_length as u64,
        }
    }

    /// インデックスバッファを読み込む
    pub async fn read_index_buffer_into(
        &self,
        sink: &mut [MaybeUninit<u8>],
    ) -> Result<(), std::io::Error> {
        match self.index_stream {
            IndexStream::None => Ok(()),
            IndexStream::Stream { ref buffer, .. } => {
                self.internal
                    .read_exact_async(buffer.content_location, sink)
                    .await
            }
        }
    }

    /// 頂点バッファを読み込む
    pub async fn read_vertex_buffer_into(
        &self,
        stream_index: usize,
        sink: &mut [MaybeUninit<u8>],
    ) -> Result<(), std::io::Error> {
        match self.vertex_streams.get(stream_index) {
            None => Ok(()),
            Some((VertexStream { buffer, .. }, _)) => {
                self.internal
                    .read_exact_async(buffer.content_location, sink)
                    .await
            }
        }
    }
}
