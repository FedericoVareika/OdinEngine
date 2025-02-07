package OdinEngine

import "utils"

vertices := [?]utils.Vertex {
	// Front
	{position = {-1, -1, +1}, uv_coords = {0, 0}, normal = {+0, +0, +1}},
    {position = {+1, -1, +1}, uv_coords = {1, 0}, normal = {+0, +0, +1}},
    {position = {+1, +1, +1}, uv_coords = {1, 1}, normal = {+0, +0, +1}},
    {position = {-1, +1, +1}, uv_coords = {0, 1}, normal = {+0, +0, +1}},

	// Back
    {position = {+1, -1, -1}, uv_coords = {0, 0}, normal = {+0, +0, -1}},
	{position = {-1, -1, -1}, uv_coords = {1, 0}, normal = {+0, +0, -1}},
	{position = {-1, +1, -1}, uv_coords = {1, 1}, normal = {+0, +0, -1}},
	{position = {+1, +1, -1}, uv_coords = {0, 1}, normal = {+0, +0, -1}},

	// Left face
	{position = {-1, -1, -1}, uv_coords = {0, 0}, normal = {-1, +0, +0}}, 
	{position = {-1, -1, +1}, uv_coords = {1, 0}, normal = {-1, +0, +0}},  
	{position = {-1, +1, +1}, uv_coords = {1, 1}, normal = {-1, +0, +0}},  
	{position = {-1, +1, -1}, uv_coords = {0, 1}, normal = {-1, +0, +0}},  

	// Right face
	{position = {+1, -1, +1}, uv_coords = {0, 0}, normal = {+1, +0, +0}}, 
	{position = {+1, -1, -1}, uv_coords = {1, 0}, normal = {+1, +0, +0}}, 
	{position = {+1, +1, -1}, uv_coords = {1, 1}, normal = {+1, +0, +0}}, 
	{position = {+1, +1, +1}, uv_coords = {0, 1}, normal = {+1, +0, +0}}, 

	// Top face
	{position = {-1, +1, +1}, uv_coords = {0, 0}, normal = {+0, +1, +0}}, 
	{position = {+1, +1, +1}, uv_coords = {1, 0}, normal = {+0, +1, +0}}, 
	{position = {+1, +1, -1}, uv_coords = {1, 1}, normal = {+0, +1, +0}}, 
	{position = {-1, +1, -1}, uv_coords = {0, 1}, normal = {+0, +1, +0}}, 

	// Bottom face
	{position = {-1, -1, -1}, uv_coords = {0, 0}, normal = {+0, -1, +0}}, 
	{position = {+1, -1, -1}, uv_coords = {1, 0}, normal = {+0, -1, +0}}, 
	{position = {+1, -1, +1}, uv_coords = {1, 1}, normal = {+0, -1, +0}}, 
	{position = {-1, -1, +1}, uv_coords = {0, 1}, normal = {+0, -1, +0}}, 
}

indices := [?]utils.Triangle {
	// Front face
	{0, 1, 2},
	{2, 3, 0},

	// Back face
	{4, 5, 6},
	{6, 7, 4},

	// Left face
	{8, 9, 10},
	{10, 11, 8},

	// Right face
	{12, 13, 14},
	{14, 15, 12},

	// Top face
	{16, 17, 18},
	{18, 19, 16},

	// Bottom face
	{20, 21, 22},
	{22, 23, 20},
}

texture_path := "./assets/textures"
texture_filenames := [utils.Texture]string {
	.Wall = "wall.jpg",
	.Container = "container.jpg",
    .Container2 = "container2.png",
    .Container2_specular = "container2_specular.png",
	.Awesomeface = "awesomeface.png",
}

light_vec: utils.Vec4f = {1.2, 1, 2, 1}
// light_vec: utils.Vec4f = {-0.2, -1, -0.3, 0}
light_color: utils.Vec3f = {1, 1, 1}
