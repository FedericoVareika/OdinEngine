package utils

import "core:math"
import "core:math/linalg"

init_transform :: proc() -> Mat4 {
	return {1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1}
}

scale :: proc(m: Mat4, v: Vec3f) -> Mat4 {
	s_m: Mat4 = {v.x, 0, 0, 0, 0, v.y, 0, 0, 0, 0, v.z, 0, 0, 0, 0, 1}

	return s_m * m
}

translate :: proc(m: Mat4, v: Vec3f) -> Mat4 {
	t_m: Mat4 = {1, 0, 0, v.x, 0, 1, 0, v.y, 0, 0, 1, v.z, 0, 0, 0, 1}

	return t_m * m
}

Radians :: distinct f32
Degrees :: distinct f32
Angle :: union {
	Radians,
	Degrees,
}

to_radians :: proc(x: Angle) -> Radians {
	return x.? or_else Radians(x.(Degrees) * linalg.RAD_PER_DEG)
}

rotate :: proc(m: Mat4, angle: Angle, axis: Vec3f) -> Mat4 {
	a := linalg.normalize(axis)

	theta := to_radians(angle)

	c := math.cos(f32(theta))
	s := math.sin(f32(theta))

	t := (1-c) * a 

	// changed
	rot: Mat4 = {}
	rot[0][0] = c + a.x * t.x
	rot[0][1] = 0 + a.y * t.x + a.z * s
	rot[0][2] = 0 + a.z * t.x - a.y * s
	rot[0][3] = 0

	rot[1][0] = 0 + a.x * t.y - a.z * s
	rot[1][1] = c + a.y * t.y
	rot[1][2] = 0 + a.z * t.y + a.x * s
	rot[1][3] = 0

	rot[2][0] = 0 + a.x * t.z + a.y * s
	rot[2][1] = 0 + a.y * t.z - a.x * s
	rot[2][2] = c + a.z * t.z
	rot[2][3] = 0

    rot[3][3] = 1

	return rot * m
}
