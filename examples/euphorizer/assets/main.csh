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

    // Modified Version of https://www.geeks3d.com/20140201/glsl-menger-sponge-raymarching-code-samples-updated/
    float maxcomp3(vec3 v) { return max(max(v.x, v.y), v.z); }
    float dist_box(vec3 p, vec3 size)
    {
        vec3 dv = abs(p) - size;
        return min(maxcomp3(dv), length(max(dv, 0.0)));
    }
    float dist_cross(vec3 p, float param)
    {
        float inf = 100.0;
        float dx = dist_box(p.xyz, vec3(inf, param, param));
        float dy = dist_box(p.yzx, vec3(param, inf, param));
        float dz = dist_box(p.zxy, vec3(param, param, inf));
        return min(min(dx, dy), dz);
    }
    float dist_menger(vec3 p)
    {
        float d = dist_box(p, vec3(4.0));
        float s = 1.0;
        for (int _ = 0; _ < 4; _++)
        {
            vec3 a = mod(p * s, 2.0) - 1.0;
            s *= 3.0;
            vec3 r = 1.0 - 4.0 * abs(a);
            float c = dist_cross(r, 1.1) / s;
            d = max(d, c);
        }
        return d;
    }
    mat2 rot(float rad)
    {
        float s = sin(rad), c = cos(rad);
        return mat2(c, s, -s, c);
    }

    float dist_scene(vec3 p)
    {
        float d = 10000000.0;
        for (float i = 0; i < 6; i++)
        {
            vec3 pp = vec3(p.xy * rot((i / 6.0) * PI * 2.0), p.z);
            d = min(d, dist_menger(mod(pp, 9.0)));
        }
        return d;
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
        const int iterate_max = 256;
        float dcur = 0.02, f = 1.0;
        vec3 p;

        for (int _ = 0; _ < iterate_max; _++)
        {
            if (abs(dcur) < .001 || f > maxd) break;

            f += dcur;
            p = eyepos + raydir * f;
            dcur = dist_scene(p);
        }

        const vec3 fog_color = vec3(0.8, 0.8, 1.0);
        vec3 mesh_color = hsv2rgb(vec3(time_sec * 0.5, 0.5, 0.7));
        if (f < maxd)
        {
            const vec2 e = vec2(0.02, 0.0);
            vec4 col = vec4(mesh_color, 1.0);
            vec3 nrm = vec3(
                dist_scene(p + e.xyy) - dist_scene(p - e.xyy),
                dist_scene(p + e.yxy) - dist_scene(p - e.yxy),
                dist_scene(p + e.yyx) - dist_scene(p - e.yyx));
            vec3 light_dir_inv = normalize(eyepos - p);
            float b = dot(normalize(nrm), light_dir_inv);
            vec3 mesh_color = b * col.xyz;
            float fog_level = f * 0.01;
            return vec4(mix(mesh_color, fog_color, fog_level), 1.0) * col.a;
        }
        else { return vec4(fog_color, 1.0); }
    }
}
FragmentShader {
    vec3 eyepos = vec3(0.0, 0.0, -FOCAL_LENGTH);
    vec3 raydir = normalize(vec3(vpos, 0.0) - eyepos);

    vec3 camera_pos = vec3(0.0, 0.0, 5.0) * pow(time_sec, 1.0);
    vec3 rd = vec3(raydir.xy * rot((time_sec * 6.0) * PI / 180.0), raydir.z);

    Target[0] = iterate_ray(eyepos + camera_pos, rd);
}

SpecConstant[FragmentShader](0) FOCAL_LENGTH: float = 25.0;
PushConstant[VertexShader] ScreenInfo {
    float aspect_wh;
}
Uniform[FragmentShader](0, 0) DynamicParams {
    float time_sec;
}
