#version 330 core

in vec2 u_size;
in vec2 u_position;

in vec2 uv;

#define POINTS 3
uniform vec2 p[POINTS];
uniform bool opposite;

// Plot a line on Y using a value between 0.0-1.0
float plot(vec2 st) {
    float val = abs(st.y - st.x);
    return step(val, 0.005) + smoothstep(0.01, 0.005, val);
}

float point(vec2 p, vec2 coord, float r) {
    float val_x = abs(coord.x - p.x);
    float val_y = abs(coord.y - p.y);
    return smoothstep(r, 0, val_x * val_x + val_y * val_y);
}

void main() {
    vec2 st = (gl_FragCoord.xy - u_position) / u_size;

    float y = st.x;

    vec3 color = vec3(y);

    float pct_line = 0;
    float pct_points = 0;

    // Plot a line
    pct_line += plot(st);
    pct_line = 0;

    for (int i = 0; i < POINTS; i++) {
        pct_points += point(p[i] * u_size + u_position,
                gl_FragCoord.xy,
                0.1 * u_size.x);
    }

    clamp(pct_line, 0, 1);
    clamp(pct_points, 0, 1);

    // float pct_area = step(uv.x * uv.x - uv.y, 0);
    float pct_area = step(uv.y * uv.y - uv.x, 0);
    if (opposite) {pct_area = 1 - pct_area;}
    // float pct_area = step(st.x * st.x - st.y, 0);
    // pct_area *= step(st.y * st.y - st.x, 0);

    gl_FragColor = vec4(
            vec3(pct_line, pct_points, pct_area),
            max(pct_line, max(pct_points, pct_area)));
}
