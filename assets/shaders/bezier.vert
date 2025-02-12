#version 330 core

struct Rect {
    vec2 pos;
    vec2 size;
};

uniform vec2 t[3];

uniform Rect r_norm;
uniform Rect r_real;

out vec2 u_position;
out vec2 u_size;

out vec2 uv;

void main() {
    Rect r = r_norm;
    vec2 vertices[3];
    // vertices[0] = r.pos;                        // Bottom Left
    // vertices[1] = r.pos + vec2(r.size.x, 0);    // Bottom Right
    // vertices[2] = r.pos + r.size;               // Top Right
    
    vertices[0] = t[0] * r.size + r.pos;  // Bottom Left
    vertices[1] = t[1] * r.size + r.pos;  // Bottom Right
    vertices[2] = t[2] * r.size + r.pos;  // Top Right

    vec2 uvs[3];
    uvs[0] = vec2(0); 
    uvs[1] = vec2(0, 0.5); 
    uvs[2] = vec2(1); 
    
    gl_Position = vec4(t[gl_VertexID], -0.1, 1);
    uv = uvs[gl_VertexID];

    u_position = r_real.pos;
    u_size = r_real.size;
}
