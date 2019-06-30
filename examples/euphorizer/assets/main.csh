VertexInput {
    Binding 0 [PerVertex] { pos: vec2; }
}
VertexShader {
    RasterPosition = vec4(pos, 0.0, 1.0);
    vpos = pos * vec2(aspect_wh, 1.0);
}
Varyings VertexShader -> FragmentShader {
    vpos: vec2;
}
Header[FragmentShader] {
    const float PI = 3.1415926;

    vec2 noize2(vec2 st)
    {
        vec2 stt = vec2(dot(st, vec2(1370.23f, 1923.3f)), dot(st.yx, vec2(305.0f, 253.0f)));
        return fract(sin(stt) * vec2(23.50f, 431.0f));
    }
    float perlin(vec2 st)
    {
        vec2 iv = floor(st);
        vec2 fv = fract(st);
        vec2 u = fv * fv * (3.0f - 2.0f * fv);

        float n00 = dot(noize2(iv + vec2(0.0f, 0.0f)), fv - vec2(0.0f, 0.0f));
        float n10 = dot(noize2(iv + vec2(1.0f, 0.0f)), fv - vec2(1.0f, 0.0f));
        float n01 = dot(noize2(iv + vec2(0.0f, 1.0f)), fv - vec2(0.0f, 1.0f));
        float n11 = dot(noize2(iv + vec2(1.0f, 1.0f)), fv - vec2(1.0f, 1.0f));

        return mix(mix(n00, n10, u.x), mix(n01, n11, u.x), u.y);
    }
    float perlin_fractal(vec2 st, int octaves)
    {
        float d = 0.0f;
        float amp = 0.5f;
        for (int i = 0; i < octaves; i++)
        {
            d += perlin(st) * amp;
            // fbm modulation from Elevated: https://www.shadertoy.com/view/MdX3Rr
            // UVを回転させてる？
            st = mat2(0.8, -0.6, 0.6, 0.8) * st * 2.0f;
            amp *= 0.5f;
        }
        return d;
    }

    // Modified Version of https://www.geeks3d.com/20140201/glsl-menger-sponge-raymarching-code-samples-updated/
    float maxcomp3(vec3 v) { return max(max(v.x, v.y), v.z); }
    float dist_box(in vec3 p, in vec3 size)
    {
        vec3 dv = abs(p) - size;
        return min(maxcomp3(dv), length(max(dv, 0.0)));
    }
    float dist_box2(in vec2 p, in vec2 size)
    {
        vec2 dv = abs(p) - size;
        return min(max(dv.x, dv.y), length(max(dv, 0.0)));
    }
    float dist_cross(in vec3 p, in float param)
    {
        float dx = dist_box2(p.xy, vec2(param));
        float dy = dist_box2(p.yz, vec2(param));
        float dz = dist_box2(p.zx, vec2(param));
        return min(min(dx, dy), dz);
    }
    float dist_menger2(vec3 p)
    {
        float d0 = dist_box(p, vec3(1.0));
        float d1 = dist_cross(p * 3.0, 1.0) / 3.0;
        return max(d0, -d1);
    }
    float dist_menger(vec3 p)
    {
        float d = dist_box(p, vec3(1.0));
        float s = 1.0;
        for (int _ = 0; _ < 4; _++)
        {
            vec3 a = mod(p * s, 2.0) - 1.0;
            s *= 3.0;
            vec3 r = 1.0 - 3.0 * abs(a);

            float c = dist_cross(r, mix(0.48, 1.5, pow(cos(time_sec * 0.8) * 0.5 + 0.5, 4.0))) / s;
            d = max(d, c);
        }
        return d;
    }
    mat2 rot(float rad)
    {
        float s = sin(rad), c = cos(rad);
        return mat2(c, s, -s, c);
    }
    vec3 pmod_xy(vec3 pin, float n)
    {
        float nr = 2.0 * PI / n;
        float rbase = atan(pin.y, pin.x) - PI / (n * 2.0);
        float r = floor(rbase / nr) * nr;

        return vec3(pin.xy * rot(r), pin.z);
    }
    mat3 rotx(float rad)
    {
        float s = sin(rad), c = cos(rad);
        return mat3(1, 0, 0, 0, c, s, 0, -s, c);
    }
    mat3 roty(float rad)
    {
        float s = sin(rad), c = cos(rad);
        return mat3(c, 0, -s, 0, 1, 0, s, 0, c);
    }
    vec3 rep_centralized(in vec3 p, float divspace)
    {
        return mod(p - divspace * 0.5, divspace) - divspace * 0.5;
    }

    float dist_scene(vec3 p)
    {
        /*float d = 10000000.0;
        for (float i = 0; i < 6; i++)
        {
            vec3 pp = vec3(rot((i / 6.0) * PI * 2.0) * p.xy, p.z);
            d = min(d, dist_menger(mod(pp, 9.0)));
        }
        return d;*/
        /*vec3 pp = pmod_xy(p, 6.0);
        pp = rep_centralized(pp, 3.0);
        return dist_menger(pp);*/
        return abs(p.y - (0.5f - 4.0f * perlin_fractal(p.xz / 5.0f, 9)));
    }

    // vec3 input: h, s, v
    vec3 hsv2rgb(vec3 c)
    {
        const vec4 k = vec4(1.0, 1.0 / 3.0, 2.0 / 3.0, 3.0);
        vec3 p = abs(fract(c.xxx + k.xzy) * 6.0 - k.www);
        return c.z * mix(k.xxx, clamp(p - k.xxx, 0.0, 1.0), c.y);
    }

    vec4 iterate_ray(vec3 eyepos, vec3 raydir)
    {
        const float maxd = 100.0;
        const int iterate_max = 512;
        float dcur = 0.02, f = 1.0;
        vec3 p;
        bool iterate_over = false;

        for (int _ = 0; _ < iterate_max && f <= maxd; _++)
        {
            p = fma(raydir, vec3(f), eyepos);
            dcur = dist_scene(p);
            if (abs(dcur) < 0.0002 * f) { iterate_over = true; break; }
            f += dcur * 0.4;
        }

        const vec3 fog_color = vec3(0.8, 0.8, 1.0);
        // vec3 mesh_color = hsv2rgb(vec3(time_sec * 0.5, 0.5, 0.7));
        vec3 mesh_color = vec3(0.5f, 0.4f, 0.4f);
        if (f < maxd)
        {
            const vec2 e = vec2(1.0, -1.0);
            const float h = 0.0001 * 0.5773;
            // return vec4(vec3(pow(1.0 - f / maxd, 4.0)), 1.0);
            // http://iquilezles.org/www/articles/normalsSDF/normalsSDF.htm
            vec3 nrm = normalize(
                e.xyy * dist_scene(p + e.xyy * h) +
                e.yyx * dist_scene(p + e.yyx * h) +
                e.yxy * dist_scene(p + e.yxy * h) +
                e.xxx * dist_scene(p + e.xxx * h)
            );
            vec4 col = vec4(mesh_color, 1.0);
            vec3 light_dir_inv = -normalize(vec3(0.1, 0.5, -0.3));
            // return vec4(nrm * 0.5 + 0.5, 1.0);
            float b = dot(nrm, light_dir_inv);
            vec3 mesh_color = b * col.xyz;
            float fog_level = min(f * 0.01 + max(p.y - 0.35f, 0.0f) * 1.3f, 1.0f);
            return vec4(mix(mesh_color, fog_color, fog_level), 1.0) * col.a;
        }
        else { return vec4(fog_color, 1.0); }
    }
}
FragmentShader {
    vec3 eyepos = vec3(0.0, 0.0, -FOCAL_LENGTH);
    vec3 raydir = normalize(vec3(vpos, 0.0) - eyepos);
    float rx = sin(time_sec * 0.3) * 0.02;
    float ry = sin(time_sec * 0.25 - 0.5) * 0.09;

    vec3 camera_pos = vec3(0.0, 0.0, 5.0) * pow(time_sec, 1.0)/* + vec3(sin(time_sec * 0.2) * 1.4, sin(time_sec * 0.18) * 2.6, 0.0)*/;
    vec3 rd = roty(ry) * rotx(rx) * raydir;

    Target[0] = iterate_ray(eyepos + camera_pos, normalize(rd));
}

SpecConstant[FragmentShader](0) FOCAL_LENGTH: float = 25.0;
PushConstant[VertexShader] ScreenInfo {
    float aspect_wh;
}
Uniform[FragmentShader](0, 0) DynamicParams {
    float time_sec;
}
