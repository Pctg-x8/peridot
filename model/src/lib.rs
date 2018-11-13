
use std::borrow::Cow;

/// Provides model data(vertices, indices...)
pub trait ModelData {
    /// The data type of a vertex
    type Vertex;
    /// THe data type of a index, use u16 as default
    type Index;

    /// Returns vertices of the model, or generates vertices(for procedural models)
    fn vertices(&self) -> Cow<[Self::Vertex]> where [Self::Vertex]: ToOwned;
    /// Returns indices of the model, or generates indices(can be `None` if the model has no indices)
    fn indices(&self) -> Option<Cow<[Self::Index]>> where [Self::Index]: ToOwned { None }
}

