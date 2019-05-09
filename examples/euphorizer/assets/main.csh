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
    // Modified Version of https://www.geeks3d.com/20140201/glsl-menger-sponge-raymarching-code-samples-updated/
    float maxcomp3(vec3 v) { return max(max(v.x, v.y), v.z); }
    float dist_box(vec3 p, vec3 size)
    {
        vec3 dv = abs(p) - size;
        return min(maxcomp3(dv), length(max(dv, 0.0)));
    }
    float dist_cross(vec3 p)
    {
        float inf = 100.0;
        float dx = dist_box(p.xyz, vec3(inf, 2.0, 2.0));
        float dy = dist_box(p.yzx, vec3(2.0, inf, 2.0));
        float dz = dist_box(p.zxy, vec3(2.0, 2.0, inf));
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
            float c = dist_cross(r) / s;
            d = max(d, c);
        }
        return d;
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
            dcur = dist_menger(p);
        }

        if (f < maxd)
        {
            const vec2 e = vec2(0.02, 0.0);
            vec4 col = vec4(0.6, 0.6, 0.8, 1.0);
            vec3 nrm = vec3(
                dcur - dist_menger(p - e.xyy),
                dcur - dist_menger(p - e.yxy),
                dcur - dist_menger(p - e.yyx));
            float b = dot(normalize(nrm), normalize(eyepos - p));
            return vec4((b * col.xyz) * (1.0 - f * .01), 1.0) * col.a;
        }
        else { return vec4(0.0, 0.0, 0.0, 1.0); }
    }
}
FragmentShader {
    vec3 eyepos = vec3(0.0, 0.0, -FOCAL_LENGTH);
    vec3 raydir = normalize(vec3(vpos, 0.0) - eyepos);

    Target[0] = iterate_ray(eyepos, raydir);
}

SpecConstant[FragmentShader](0) FOCAL_LENGTH: float = 25.0;
PushConstant[VertexShader] ScreenInfo {
    float aspect_wh;
}
