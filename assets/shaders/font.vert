#version 430 core
// #extension GL_NV_gpu_shader5 : enable

layout(std430, binding = 0) buffer Vertices {
    vec2 vertices[];
};

// layout(std430, binding = 1) buffer OnCurve {
//     bool on_curves[];
// };

layout(std430, binding = 2) buffer Indices {
    uint indices[];
};

struct UVZ {
    vec2 uv;
    bool z;
};

layout(std430, binding = 1) buffer Uvs {
    UVZ uvs[];
};

uniform int selected_vert;

out vec3 color;
out vec2 uv;
out float inner;

void main() {
    uint index = indices[gl_VertexID];

    uv = uvs[gl_VertexID].uv;
    inner = uvs[gl_VertexID].z ? 1.0f : -1.0f;

    vec2 vertex = vertices[index];
    // bool on_curve = on_curves[index];

    // uvz = vec3(0, 1, 1);

    uint triangle = index / 3;

    color = vec3(
            step(2, triangle++ % 3),
            step(2, triangle++ % 3),
            step(2, triangle % 3)
        );

    // // Debug output
    // if (gl_VertexID == 0) {
    //     gl_Position = vec4(-1.0, -1.0, 0.0, 1.0); // Bottom-left corner
    //     color = vec3(1.0, 0.0, 0.0); // Red
    // } else if (gl_VertexID == 1) {
    //     gl_Position = vec4(1.0, -1.0, 0.0, 1.0); // Bottom-right corner
    //     color = vec3(0.0, 1.0, 0.0); // Green
    // } else if (gl_VertexID == 2) {
    //     gl_Position = vec4(0.0, 1.0, 0.0, 1.0); // Top-center
    //     color = vec3(0.0, 0.0, 1.0); // Blue
    // } else {
    //     gl_Position = vec4(vertex, -0.2, 1.0);
    //     color = vec3(on_curve, !on_curve, 0.0);
    // }

    // gl_PointSize = 10.0;

    // color = vec3(!on_curve, 0, on_curve);
    // color = vec3(sin(float(index / 3) / 10), cos(float(index / 3) / 10), tan(float(index / 3) / 10));
    gl_Position = vec4(vertex, -0.2, 1);
    gl_PointSize = 10;
    if (selected_vert == index) {
        gl_PointSize = 50;
    }
}
