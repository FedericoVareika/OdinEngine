#version 430 core

#define MAX_POINTS 76

struct Coord {
    float x, y;
    // float u, v;
    bool on_curve;
};

layout(std430, binding = 3) readonly buffer VertexData {
    Coord coords[];
};

// layout(std430, binding = 2) readonly buffer VertexIndices {
//     Coord indices[];
// };

out vec3 color;

void main() {
    color = vec3(coords[gl_VertexID].on_curve, !coords[gl_VertexID].on_curve, 0);
    gl_PointSize = 10;
    gl_Position = vec4(coords[gl_VertexID].x, coords[gl_VertexID].y, -0.2, 1);
}
