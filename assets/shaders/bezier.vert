#version 330 core

struct Rect {
    vec2 pos;
    vec2 size;
};

uniform Rect r_norm;
uniform Rect r_real;

out vec2 u_position;
out vec2 u_size;

void main() {
    Rect r = r_norm;
    vec2 vertices[6];
    vertices[0] = r.pos + vec2(0, r.size.y);    // Top Left
    vertices[1] = r.pos;                        // Bottom Left
    vertices[2] = r.pos + r.size;               // Top Right
    vertices[3] = r.pos + r.size;               // Top Right
    vertices[4] = r.pos;                        // Bottom Left
    vertices[5] = r.pos + vec2(r.size.x, 0);    // Bottom Right
    
    gl_Position = vec4(vertices[gl_VertexID], -0.1, 1);
    u_position = r_real.pos;
    u_size = r_real.size;
}
