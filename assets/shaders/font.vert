#version 430 core

layout(std430, binding = 0) buffer Vertices {
    vec2 vertices[];
};

layout(std430, binding = 1) buffer OnCurve {
    bool on_curves[];
};

layout(std430, binding = 2) buffer Indices {
    uint indices[];
};

out vec3 color;

void main() {
    uint index = indices[gl_VertexID];
    vec2 vertex = vertices[index];
    bool on_curve = on_curves[index];

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
    
    color = vec3(on_curve, 0, 0);
    gl_Position = vec4(vertex, -0.2, 1);
    gl_PointSize = 10;
}
