package OdinEngine

import "utils"

State :: struct {
	time:       struct {
		last_frame, delta_value: u64,
		delta_s:                 f64,
	},
	graphics:   struct {
		VAOs:    map[string]u32,
		VBOs:    map[string]u32,
		EBOs:    map[string]u32,
		indices: map[u32]u32,
		shaders: map[string]u32,
	},
	models:     [dynamic]utils.Model,
	textures:   [utils.Texture]u32,
	camera:     utils.Camera,
	window:     utils.Window,
	transforms: struct {
		model, view, proj: utils.Mat4,
	},
	mouse:      struct {
		pos:         utils.Vec2f,
		sensitivity: f32,
        last_action: utils.MouseAction,
	},
}
