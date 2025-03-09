package OdinEngine

import "utils"

import "base:runtime"

import "core:fmt"
import "core:math/linalg"

import gl "vendor:OpenGL"
import "vendor:glfw"

error_callback :: proc "c" (code: i32, desc: cstring) {
	context = runtime.default_context()
	fmt.eprintf("%s, %d", desc, code)
}

framebuffer_size_callback :: proc "c" (
	window: glfw.WindowHandle,
	width, height: i32,
) {
	context = runtime.default_context()

	{
		using state
		window.size = {f32(width), f32(height)}
		window.aspect_ratio = f32(width) / f32(height)
		transforms.proj = linalg.matrix4_perspective(
			window.fovy,
			window.aspect_ratio,
			window.near,
			window.far,
		)
	}

	gl.Viewport(0, 0, width, height)
}

key_callback :: proc "c" (
	window: glfw.WindowHandle,
	key, scancode, action, mods: i32,
) {
	context = runtime.default_context()

	using glfw

	mult: f32
	if action == PRESS do mult = 1
	else if action == RELEASE do mult = -1

	switch key {
	case KEY_ESCAPE, KEY_Q:
		SetWindowShouldClose(window, true)
	case KEY_T:
		if action == PRESS {wireframe_mode = !wireframe_mode
			gl.PolygonMode(
				gl.FRONT_AND_BACK,
				wireframe_mode ? gl.LINE : gl.FILL,
			)
		}
	case KEY_SPACE:
		state.camera.velocity.y += mult
	case KEY_LEFT_SHIFT, KEY_RIGHT_SHIFT:
		state.camera.velocity.y -= mult
	case KEY_W:
		state.camera.velocity.z -= mult
	case KEY_A:
		state.camera.velocity.x -= mult
	case KEY_S:
		state.camera.velocity.z += mult
	case KEY_D:
		state.camera.velocity.x += mult
	case KEY_UP:
		state.camera.angular_velocity.x += mult
	case KEY_DOWN:
		state.camera.angular_velocity.x -= mult
	case KEY_LEFT:
		state.camera.angular_velocity.y -= mult
	case KEY_RIGHT:
		state.camera.angular_velocity.y += mult
	}
}

mouse_button_callback :: proc "c" (
	window: glfw.WindowHandle,
	button, action, mods: i32,
) {
	switch button {
	case glfw.MOUSE_BUTTON_RIGHT:
		if action == glfw.PRESS {
			rotating = true
			glfw.SetInputMode(window, glfw.CURSOR, glfw.CURSOR_HIDDEN)
		}
		if action == glfw.RELEASE {
			rotating = false
			glfw.SetInputMode(window, glfw.CURSOR, glfw.CURSOR_NORMAL)
		}
	case glfw.MOUSE_BUTTON_LEFT:
		switch action {
		case glfw.PRESS:
			state.mouse.last_action = .Down
		case glfw.RELEASE:
			state.mouse.last_action = .Up
		}
	}
}

mouse_pos_callback :: proc "c" (window: glfw.WindowHandle, x_pos, y_pos: f64) {
	context = runtime.default_context()

	new_mouse_pos: utils.Vec2f = {f32(x_pos), f32(y_pos)}
	if (rotating) {
		rotate_camera(
			&state,
			new_mouse_pos.x - state.mouse.pos.x,
			new_mouse_pos.y - state.mouse.pos.y,
		)
	}

	state.mouse.pos = new_mouse_pos
}

mouse_scroll_callback :: proc "c" (
	window: glfw.WindowHandle,
	x_offset, y_offset: f64,
) {
	state.mouse.sensitivity += f32(y_offset) * 0.1
}
