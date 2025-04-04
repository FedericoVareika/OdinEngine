package utils

Mat4 :: matrix[4, 4]f32
Mat3 :: matrix[3, 3]f32
Vec2f :: [2]f32
Vec3f :: [3]f32
Vec4f :: [4]f32

Vec2i :: [2]i32
Vec3i :: [3]i32
Vec4i :: [4]i32

Vec2u :: [2]u32
Vec3u :: [3]u32
Vec4u :: [4]u32

Vec3b :: [3]bool

Triangle :: distinct [3]u32
TriangleUV :: [3]struct #packed {
    uv: Vec2f,
    z: b64,
}

Rect :: struct {
	pos, size: Vec2f,
}

Coordf :: struct {
	p:        Vec2f,
	on_curve: b32,
}

Texture :: enum {
	Wall,
	Container,
	Container2,
	Container2_specular,
	Awesomeface,
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
	VAO:       u32,
	transform: Mat4,
	shader:    u32,
	material:  Material,
}

Material :: struct {
	texture, diffuse_map, specular_map: u32,
}

Window :: struct {
	size:                          Vec2f,
	aspect_ratio, fovy, near, far: f32,
}

MouseAction :: enum {
	Idle,
	Down,
	Up,
}
