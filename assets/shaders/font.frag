#version 430 core
// #extension GL_NV_gpu_shader5 : enable

in vec3 color;
in vec2 uv;
in float inner;

out vec4 frag_color;

void main() {
    // uv.z is 1 if outer curve, or -1 if inner curve
    float pct_area = step(inner * (uv.x * uv.x - uv.y), 0);
    // if (inner) {pct_area = 1 - pct_area;}
    
    frag_color = vec4(color, pct_area);
    // frag_color = vec4(color, 1);
}
