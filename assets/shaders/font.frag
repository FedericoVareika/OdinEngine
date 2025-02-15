#version 330 core

in vec3 color;
out vec4 frag_color;

void main() {
    frag_color = vec4(color, 1);
}

/*
x=610 y=245
x=610 y=0
x=281 y=0
x=32 y=0
x=32 y=856
x=266 y=856
x=399 y=856
x=468 y=824
x=580 y=772
x=580 y=633
x=580 y=499
x=448 y=451
x=610 y=400
x=488 y=633
x=488 y=722
x=411 y=755
x=362 y=777
x=266 y=777
x=124 y=777
x=124 y=489
x=266 y=489
x=365 y=489
x=414 y=511
x=488 y=545
x=518 y=245
x=518 y=410
x=281 y=410
x=124 y=410
x=124 y=80
x=281 y=80
x=518 y=80
*/
