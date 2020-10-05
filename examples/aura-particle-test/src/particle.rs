
use std::collections::BTreeSet;
use rayon::prelude::*;
use bedrock as br;

#[repr(C, align(16))]
pub struct ParticleRenderInstance {
    pub mat: peridot::math::Matrix4F32,
    pub alpha: f32
}

fn lerp(a: f32, b: f32, x: f32) -> f32 { a * (1.0 - x) + b * x }

pub struct CPUParticleInstance {
    pub pos: peridot::math::Vector3F32,
    pub scale: peridot::math::Vector3F32,
    pub velocity: peridot::math::Vector3F32,
    pub lifetime: f32,
    pub living_time: f32
}
impl CPUParticleInstance {
    pub fn update(&mut self, dt: std::time::Duration) {
        self.living_time += (dt.as_micros() as f64 / 1_000_000.0) as f32;
        self.pos = self.pos.clone() + self.velocity.clone() * (dt.as_micros() as f64 / 1_000_000.0) as f32;
        let s = lerp(0.6, 1.0, 1.0 - (self.living_time / self.lifetime).powf(2.0));
        self.scale = peridot::math::Vector3(s, s, s);
    }
    pub fn died(&self) -> bool { self.living_time >= self.lifetime }

    pub fn render_instance(&self) -> ParticleRenderInstance {
        let s = peridot::math::Matrix4F32::scale(peridot::math::Vector4(self.scale.0, self.scale.1, self.scale.2, 1.0));

        ParticleRenderInstance {
            mat: peridot::math::Matrix4F32::translation(self.pos.clone()) * s,
            alpha: 1.0 - self.living_time / self.lifetime
        }
    }
}
pub struct CPUParticleDriver {
    static_buffer_offset: u64,
    instances: Vec<CPUParticleInstance>,
    freespaces: BTreeSet<usize>,
    update_buffer: peridot::Buffer
}
impl CPUParticleDriver {
    pub const MAX_RENDERED_INSTANCE_COUNT: usize = 65536;

    pub fn new(
        g: &peridot::Graphics,
        buf_prealloc: &mut peridot::BufferPrealloc,
        _tfb: &mut peridot::TransferBatch
    ) -> Self {
        let static_buffer_offset = buf_prealloc.add(
            peridot::BufferContent::storage::<[ParticleRenderInstance; Self::MAX_RENDERED_INSTANCE_COUNT]>()
        );

        let mut bp = peridot::BufferPrealloc::new(g);
        bp.add(peridot::BufferContent::raw_multiple::<ParticleRenderInstance>(Self::MAX_RENDERED_INSTANCE_COUNT));
        let ub = bp.build_upload().expect("Failed to build particle driver buffer");
        let mut mb = peridot::MemoryBadget::new(g);
        mb.add(ub);
        let ub = mb.alloc_upload().expect("Failed to alloc particle driver memory").pop()
            .expect("less object").unwrap_buffer();

        CPUParticleDriver {
            static_buffer_offset,
            instances: Vec::new(),
            freespaces: BTreeSet::new(),
            update_buffer: ub
        }
    }
    pub fn post_transfer(&self, tfb: &mut peridot::TransferBatch, static_buf: &peridot::Buffer) {
        tfb.add_buffer_graphics_ready(
            br::PipelineStageFlags::VERTEX_SHADER, static_buf, self.update_range_static(), br::AccessFlags::SHADER.read
        );
    }

    pub fn buffer_bytesize(&self) -> u64 {
        std::mem::size_of::<[ParticleRenderInstance; Self::MAX_RENDERED_INSTANCE_COUNT]>() as _
    }
    pub fn update_range_staging(&self) -> std::ops::Range<u64> {
        0 .. self.buffer_bytesize()
    }
    pub fn update_range_static(&self) -> std::ops::Range<u64> {
        self.static_buffer_offset .. self.static_buffer_offset + self.buffer_bytesize()
    }

    /// Returns newly spawned instance index
    pub fn spawn(&mut self, instance: CPUParticleInstance) -> usize {
        if let Some(&freespace) = self.freespaces.iter().next() {
            self.freespaces.take(&freespace);
            self.instances[freespace] = instance;
            return freespace;
        }
        let next_id = self.instances.len();
        self.instances.push(instance);
        next_id
    }
    pub fn die(&mut self, id: usize) {
        self.freespaces.insert(id);
        self.instances[id].scale = peridot::math::Vector3(0.0, 0.0, 0.0);
    }
    pub fn update(&mut self, dt: std::time::Duration) {
        let range = 0 .. std::mem::size_of::<[ParticleRenderInstance; Self::MAX_RENDERED_INSTANCE_COUNT]>() as _;
        let ub = &self.update_buffer;
        let is = &mut self.instances;
        let died_ids = ub.guard_map(range, move |m| {
            let matrices = unsafe { m.slice_mut::<ParticleRenderInstance>(0, Self::MAX_RENDERED_INSTANCE_COUNT) };
            is.par_iter_mut().zip(matrices).enumerate().filter_map(|(id, (i, m))| {
                if i.died() {
                    i.scale = peridot::math::Vector3(0.0, 0.0, 0.0);
                    m.mat = peridot::math::Matrix4F32::scale(peridot::math::Vector4(0.0, 0.0, 0.0, 0.0));
                    Some(id)
                } else {
                    i.update(dt);
                    *m = i.render_instance();
                    None
                }
            }).collect::<Vec<_>>()
        }).expect("Failed to update stg buffer");
        for id in died_ids { self.die(id); }
    }
}
pub struct ParticleEngine {
    driver: CPUParticleDriver,
    left_next_spawn: std::time::Duration
}
impl ParticleEngine {
    pub fn new(
        g: &peridot::Graphics,
        buf_prealloc: &mut peridot::BufferPrealloc,
        tfb: &mut peridot::TransferBatch
    ) -> Self {
        ParticleEngine {
            driver: CPUParticleDriver::new(g, buf_prealloc, tfb),
            left_next_spawn: std::time::Duration::from_millis(100)
        }
    }
    pub fn post_transfer(&self, tfb: &mut peridot::TransferBatch, static_buf: &peridot::Buffer) {
        self.driver.post_transfer(tfb, static_buf);
    }

    pub fn update(&mut self, dt: std::time::Duration) {
        use rand::distributions::Distribution;

        if self.left_next_spawn <= dt {
            self.left_next_spawn = std::time::Duration::from_millis(100);
            let spawn_count = rand::distributions::Uniform::new_inclusive(1, 5).sample(&mut rand::thread_rng());
            for _ in 0 .. spawn_count {
                self.driver.spawn(CPUParticleInstance {
                    scale: peridot::math::Vector3(1.0, 1.0, 1.0),
                    pos: peridot::math::Vector3(0.0, 0.0, 0.0),
                    velocity: peridot::math::Vector3::<f32>::rand_unit_sphere(&mut rand::thread_rng()) * 0.4f32,
                    lifetime: rand::distributions::Uniform::new_inclusive(0.5, 1.5).sample(&mut rand::thread_rng()),
                    living_time: 0.0
                });
            }
        } else {
            self.left_next_spawn -= dt;
        }
        self.driver.update(dt);
	}
	pub fn staging_buffer_ranged(&self) -> (&br::Buffer, std::ops::Range<u64>) {
		(&self.driver.update_buffer, self.driver.update_range_staging())
	}
	pub fn managed_instance_buffer_range(&self) -> std::ops::Range<u64> {
		self.driver.update_range_static()
	}
}
