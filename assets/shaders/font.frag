#version 430 core
// #extension GL_NV_gpu_shader5 : enable

uniform vec3 color;

in vec2 uv;
in float inner;

out vec4 frag_color;

float linearstep(float edge0, float edge1, float x) {
    return clamp((x - edge0) / (edge1 - edge0), 0.0, 1.0);
}

void main() {
    // uv.z is 1 if outer curve, or -1 if inner curve
    float smoothing_amount = 1;
    float dist = (uv.x * uv.x - uv.y);
    float pixel_size = length(vec2(dFdx(dist), dFdy(dist)));
    dist *= inner;
    dist -= pixel_size * 0.5;
    // pixel_size = length(vec2(dFdx(dist), dFdy(dist)));
    float alpha = (-dist) / (pixel_size * smoothing_amount);
    // float alpha = (1.0 - dist); 

    // vec2 gradient = vec2(2 * uv.x, -1);
    // float distance = f / length(gradient);

    // alpha = step(0, -dist);

    frag_color = vec4(vec3(color.x * inner, color.y, -color.z * inner), alpha);
    // frag_color = vec4(color, 1);
}
