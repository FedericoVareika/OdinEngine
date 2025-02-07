#version 330 core
out vec4 FragColor;

// in vec4 vert_color;
in vec3 frag_pos;
in vec3 normal;

// uniform vec3 object_color;

// uniform sampler2D texture1;
// uniform sampler2D texture2;

struct Material {
    sampler2D diffuse;
    sampler2D specular;
    float shininess;
};

uniform Material material;

in vec2 tex_coords;

struct Light {
    // vec3 pos;
    vec4 vector;

    vec3 ambient;
    vec3 diffuse;
    vec3 specular;

    float k_c;
    float k_l;
    float k_q;

    float cutoff;
};

uniform Light light;

void main() {
    vec3 light_dir;
    float att;
    if (light.vector.w == 1) {
        light_dir = normalize(light.vector.xyz - frag_pos);
        float d = distance(light.vector.xyz, frag_pos);
        att = 1 / (light.k_c + d * light.k_l + d * d * light.k_q);
    } else if (light.vector.w == 0) {
        light_dir = normalize(-light.vector.xyz);
        att = 1;
    }

    float diff = max(dot(light_dir, normal), 0);

    vec3 view_dir = normalize(-frag_pos);
    vec3 reflect_dir = reflect(-light_dir, normal);
    float spec = pow(max(dot(reflect_dir, view_dir), 0), material.shininess);

    vec3 diffuse_tex = vec3(texture(material.diffuse, tex_coords));
    vec3 specular_tex = vec3(texture(material.specular, tex_coords));

    vec3 ambient = light.ambient * diffuse_tex;
    vec3 diffuse = light.diffuse * diff * diffuse_tex;
    vec3 specular = light.specular * spec * specular_tex;

    vec3 result = ambient;

    float theta = dot(light_dir, vec3(0, 0, 1));
    float i = (theta - light.cutoff) / 0.1;
    // if (dot(light_dir, vec3(0, 0, 1)) > light.cutoff) {
    result += (diffuse + specular) * i;
    // }

    /*
             * toon shading tryout
             * https://developer.download.nvidia.com/CgTutorial/cg_tutorial_chapter09.html
             * */

    result *= att;
    FragColor = vec4(result, 1);
}
