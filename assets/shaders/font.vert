#version 430 core
// #extension GL_NV_gpu_shader5 : enable

layout(std430, binding = 0) buffer Vertices {
    vec2 vertices[];
};

layout(std430, binding = 1) buffer Indices {
    uint indices[];
};

struct UVZ {
    vec2 uv;
    bool z;
};

layout(std430, binding = 2) buffer Uvs {
    UVZ uvs[];
};

uniform int selected_vert;

uniform int vertex_offset;
uniform int triangle_offset;

uniform float font_size_mult;
uniform vec2 screen_scale;

uniform vec2 non_scaled_translation; 
uniform vec2 scaled_translation; 

out vec2 uv;
out float inner;

void main() {
    int offset_idx = gl_VertexID + triangle_offset * 3;
    uint index = indices[offset_idx] + vertex_offset;

    uv = uvs[offset_idx].uv;
    inner = uvs[offset_idx].z ? 1.0f : -1.0f;

    vec2 vertex = vertices[index];
    vertex *= font_size_mult;
    vertex += vec2(non_scaled_translation.x, -1 * non_scaled_translation.y); 
    vertex *= screen_scale;
    vertex += scaled_translation;

    gl_Position = vec4(vertex, -0.2, 1);
    gl_PointSize = 3;
    if (selected_vert == indices[offset_idx]) {
        gl_PointSize = 8;
    }
}
