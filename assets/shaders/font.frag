#version 430 core

uniform vec3 color;
uniform float smoothness;

in vec2 uv;
in float inner;

out vec4 frag_color;

float linearstep(float edge0, float edge1, float x) {
    return clamp((x - edge0) / (edge1 - edge0), 0.0, 1.0);
}

void main() {
    // uv.z is 1 if outer curve, or -1 if inner curve
    float g = (uv.x * uv.x - uv.y);
    g *= inner;

    float delta_g = sqrt(dFdx(g) * dFdx(g) + dFdy(g) * dFdy(g));
    float dist = g ;
    if (delta_g > 0) {
        dist /= delta_g;
    }

    float pixel_size = length(vec2(dFdx(dist), dFdy(dist)));
    // dist *= inner;
    dist -= pixel_size * smoothness * 0.5;

    // float alpha = - dist / (pixel_size * smoothness);
    float alpha = - dist / smoothness;
    if (alpha < 0.005f)
        discard;
    frag_color = vec4(color, alpha);
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
