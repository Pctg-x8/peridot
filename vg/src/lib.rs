//! Peridot Vector Graphics Dept. powered by Pathfinder 2(lyon)

extern crate pathfinder_partitioner;
extern crate lyon_path;

extern crate font_kit; mod font; pub use font::*;

use pathfinder_partitioner::{mesh::Mesh, partitioner::Partitioner, FillRule};
use lyon_path::builder::FlatPathBuilder;

pub struct Context { meshes: Vec<Mesh> }
impl Context {
    pub fn new() -> Self {
        Context { meshes: Vec::new() }
    }
}

pub struct FigureContext<'c> { ctx: &'c mut Context, partitioner: Partitioner, fill_rule: FillRule }
impl Context {
    pub fn begin_figure(&mut self, fill_rule: FillRule) -> FigureContext {
        FigureContext { ctx: self, partitioner: Partitioner::new(), fill_rule }
    }
}
impl<'c> FigureContext<'c> {
    pub fn end(mut self) -> &'c mut Context {
        self.partitioner.partition(self.fill_rule);
        self.partitioner.builder_mut().build_and_reset();
        self.ctx.meshes.push(self.partitioner.into_mesh());

        return self.ctx;
    }
}
