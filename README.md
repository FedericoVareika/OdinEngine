This project is my first dive into OpenGL and modern computer graphics, following [learnopengl.com](https://learnopengl.com/) . The idea is to create a graphics engine completely from scratch using OdinLang.

The engine is very rudimentary, as most of my time has been spent developing its font rendering for an immediate mode gui. 

### Font rendering
For this engine's font rendering I chose to follow the research paper: [Resolution Independent Curve Rendering using Programmable Graphics Hardware, 2005](https://www.microsoft.com/en-us/research/wp-content/uploads/2005/01/p1000-loop.pdf).
It focuses on using real-time font rendering without bitmaps but with glyph triangulation and quadratic bezier calculations on the GPU in order to achieve resolution independent rendering. 

#### My implementation
For my implementation I made a simple TTF parser, which extracts ASCII glyphs and global data. 
After parsing the according .ttf file, the engine triangulates every glyph's anchor points using delauney triangulation (also implemented by me, following [Primitives for the manipulation of general subdivisions and the computation of Voronoi
, 1985](https://dl.acm.org/doi/10.1145/282918.282923)) and then constrains the glyph's edges following this implementetion shown in the youtube series [CAD From Scratch](https://www.youtube.com/watch?v=_Pe-Raurn34). 
This results in something like so: 
![Result of glyph triangulation](https://github.com/user-attachments/assets/3fb09bf9-d999-439f-b225-74862f195bda)

Applying a fragment shader so that it uses Loop/Blinn: 
```glsl
#version 450 core

uniform vec3 color;
uniform float smoothness;

uniform float cutoff;

in vec2 uv;
in float inner;

out vec4 frag_color;

float get_intensity(vec2 uv) {
    // uv.z is 1 if outer curve, or -1 if inner curve
    float g = (uv.x * uv.x - uv.y);
    g *= inner;

    float delta_g = sqrt(dFdx(g) * dFdx(g) + dFdy(g) * dFdy(g));
    float dist = g;
    if (delta_g > 0) {
        dist /= delta_g;
    }

    float pixel_size = length(vec2(dFdx(dist), dFdy(dist)));
    dist -= pixel_size * smoothness * 0.5;

    float alpha = - dist / (pixel_size * smoothness);

    return alpha;
}

void main() {
  float alpha = get_intensity(uv);
  if (alpha < cutoff) 
    discard;
  frag_color = vec4(color, alpha);
}
```

![Result of base implementation](https://github.com/user-attachments/assets/965afa37-eb4b-4d16-b764-e7195d950200)

I then implemented the paper's anti-aliasing technique, which I still need to tinker with but I do not believe is good enough for modern rendering. 

*base anti-aliasing*
![base anti-aliasing](https://github.com/user-attachments/assets/1e314c18-24cf-4fd4-9136-ebb8f7091b8e)

*base anti-aliasing + MSAAx4*
![base anti-aliasing + MSAAx4](https://github.com/user-attachments/assets/1981fb70-26fa-4899-9b61-e0f3389932b5)

*base anti-aliasing + MSAAx4 + subpixel*
![base anti-aliasing + MSAAx4 + subpixel](https://github.com/user-attachments/assets/5e4f5c78-ea27-47aa-83af-f374f7f89249)


This implementation is not used industry-wise because of its complexity regarding for example triangulation, and the suboptimal anti-aliasing technique shown in the paper. 
Other techniques include:
- Simple bitmap rendering, which is using a quad and a glyph texture to render a single character. This does not produce resolution-independent rendering.
- Multi channel SDF (Signed distance function) rendering, which takes advantage of SDF bitmaps to generate higher resolution glyphs. Its generator including the master thesis regarding this technique can be found [here](https://github.com/Chlumsky/msdfgen)

