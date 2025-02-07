#version 330 core 
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;
layout (location = 2) in vec2 aTexCoord;

out vec3 normal;
out vec3 frag_pos;
out vec2 tex_coords;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;
uniform mat3 normal_matrix;

void main() {
    gl_Position = projection * view * model * vec4(aPos, 1);
    frag_pos = vec3(view * model * vec4(aPos, 1));

    normal = normalize(normal_matrix * aNormal);
    tex_coords = aTexCoord;
}
