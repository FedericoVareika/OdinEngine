#version 450 core

uniform vec3 color;
uniform float smoothness;

uniform float cutoff;
uniform bool subpixel;

in vec2 uv;
in float inner;

out vec4 frag_color;

float get_intensity(vec2 uv) {
    // uv.z is 1 if outer curve, or -1 if inner curve
    float g = (uv.x * uv.x - uv.y);
    g *= inner;

    float delta_g = sqrt(dFdx(g) * dFdx(g) + dFdy(g) * dFdy(g));
    float dist = g;
    if (delta_g > 0) {
        dist /= delta_g;
    }

    float pixel_size = length(vec2(dFdx(dist), dFdy(dist)));
    // dist *= inner;
    dist -= pixel_size * smoothness * 0.5;

    float alpha = - dist / (pixel_size * smoothness);
    // float alpha = -dist / smoothness;

    return alpha;
}

void main() {
    if (!subpixel) {
        float alpha = get_intensity(uv);
        if (alpha < cutoff) 
            discard;
        frag_color = vec4(color, alpha);
    } else {
        // float dx = dFdxFine(uv.x);
        vec2 duv = dFdx(uv);
        // if (dx == 0) 
        //     discard;
        vec2 offset = duv / 3;

        float c1 = get_intensity(uv - offset);
        float c2 = get_intensity(uv);
        float c3 = get_intensity(uv + offset);

        // bad mixing
        vec3 subpixel_intensity = vec3(
                2 * c1 + c2,
                c1 + c2 + c3,
                c2 + 2 * c3);
        subpixel_intensity /= 3;
        // vec3 subpixel_intensity = vec3(c1, c2, c3);

        // if (subpixel_intensity.r < cutoff)
        //     subpixel_intensity.r = 0;
        // if (subpixel_intensity.g < cutoff)
        //     subpixel_intensity.g = 0;
        // if (subpixel_intensity.b < cutoff)
        //     subpixel_intensity.b = 0;

        if (subpixel_intensity.r < cutoff &&
                subpixel_intensity.g < cutoff &&
                subpixel_intensity.b < cutoff)
            discard;

        frag_color = vec4(vec3(1) - subpixel_intensity, 1);
        // frag_color = vec4(subpixel_intensity, 1);
    }
}

/*

    // uv.z is 1 if outer curve, or -1 if inner curve
    float dist = (uv.x * uv.x - uv.y);
    float pixel_size = length(vec2(dFdx(dist), dFdy(dist)));
    dist *= inner;
    dist -= pixel_size * smoothness * 0.5;

    // pixel_size = length(vec2(dFdx(dist), dFdy(dist)));
    float alpha = - dist / (pixel_size * smoothness);
    // float alpha = (1.0 - dist);

    // vec2 gradient = vec2(2 * uv.x, -1);
    // float distance = f / length(gradient);

    // alpha = step(0, -dist);

    // frag_color = vec4(color, 1-dist);
    // frag_color = vec4(color, abs(dist));
    if (alpha < 0.005f)
        discard;
    frag_color = vec4(color, alpha);
 * */
