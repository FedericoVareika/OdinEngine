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
    float smoothing_amount = 50;
    float dist = inner * (uv.x * uv.x - uv.y);
    float pixel_size = length(vec2(dFdx(dist), dFdy(dist)));
    float alpha = (1.0 - dist) / (pixel_size * smoothing_amount);
    // float alpha = (1.0 - dist); 

    // vec2 gradient = vec2(2 * uv.x, -1);
    // float distance = f / length(gradient);

    // float alpha = smoothstep(-1, 1, -distance);

    frag_color = vec4(color, alpha);
    // frag_color = vec4(color, 1);
}
