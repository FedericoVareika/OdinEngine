#version 330 core

in vec2 u_size;
in vec2 u_position;

uniform vec2 p[3];

// Plot a line on Y using a value between 0.0-1.0
float plot(vec2 st) {
    float val = abs(st.y - st.x);
    return step(val, 0.005) + smoothstep(0.01, 0.005, val);
}

float point(vec2 p, vec2 st, float r) {
    float val_x = abs(st.x - p.x);
    float val_y = abs(st.y - p.y);
    return smoothstep(r, 0, val_x * val_x + val_y * val_y); 
}

void main() {
    vec2 st = (gl_FragCoord.xy - u_position) / u_size;

    float y = st.x;

    vec3 color = vec3(y);

    // Plot a line
    // float pct = plot(st);
    float pct = 0;
    for (int i = 0; i < 2; i++) {
        pct += point(p[i], st, 0.1);
    }

    pct = point(p[0], st, 0.1);
    clamp(pct, 0, 1);

    gl_FragColor = vec4(pct * vec3(0, 1, 0), 1.0);
    // gl_FragColor = vec4(pct, 0, vec2(0, 1));

    // gl_FragColor = vec4(step(0.1, (abs(st.x - 0.5))) * vec3(0.0, 1.0, 0.0), 1.0);
    // gl_FragColor += vec4(abs(st.y - 0.5) * vec3(1.0, 0.0, 0.0), 1.0);
}
