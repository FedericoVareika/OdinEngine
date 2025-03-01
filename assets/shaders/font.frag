#version 430 core
// #extension GL_NV_gpu_shader5 : enable

uniform vec3 color;

in vec2 uv;
in float inner;

out vec4 frag_color;

void main() {
    // uv.z is 1 if outer curve, or -1 if inner curve
    float f = inner * (uv.x * uv.x - uv.y);
    float alpha = step(f, 0);

    // vec2 gradient = vec2(2 * uv.x, -1);
    // float distance = f / length(gradient);

    // float alpha = smoothstep(-1, 1, -distance);

    frag_color = vec4(color, alpha);
    // frag_color = vec4(color, 1);
}
