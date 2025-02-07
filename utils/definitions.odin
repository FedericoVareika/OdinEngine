package utils

Mat4 :: matrix[4, 4]f32
Mat3 :: matrix[3, 3]f32
Vec2f :: [2]f32
Vec3f :: [3]f32
Vec4f :: [4]f32

Vec3b :: [3]bool

Triangle :: [3]i32

Texture :: enum {
	// Wall,
	// Container,
	Container2,
    Container2_specular,
	// Awesomeface,
}

Direction :: enum {
	Up,
	Down,
	Left,
	Right,
}

Moving :: enum {
	No,
	Forwards,
	Backwards,
}

Vertex :: struct {
	position:  Vec3f,
	normal:    Vec3f,
	uv_coords: Vec2f,
}

Camera :: struct {
	position:         Vec3f,
	direction:        Vec3f,
	up:               Vec3f,
	euler_angles:     struct {
		yaw, pitch, roll: f32,
	},
	velocity:         Vec3f,
	angular_velocity: Vec3f,
}

Model :: struct {
	transform: Mat4,
	material:  Material,
	VAO:       u32,
}

Material :: struct {
	texture: u32,
	shader:  u32,
}

Window :: struct {
	aspect_ratio, fovy, near, far: f32,
}
