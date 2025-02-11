package UI

import "../utils"

import "core:fmt"
import "core:math"

import gl "vendor:OpenGL"

UIID :: struct {
	id: i32,
}

Elem :: struct {
	id:   UIID,
	rect: utils.Rect,
}

UI_State :: struct {
	hot, active: UIID,
	mouse:       struct {
		pos:         utils.Vec2f,
		last_action: utils.MouseAction,
	},
	elems:       [dynamic]Elem,
	graphics:    struct {
		shader: u32,
		colors: struct {
			base, hot, active: utils.Vec3f,
		},
	},
}

ui_state: UI_State = {}

mouse_is_inside :: proc(r: utils.Rect) -> bool {
	if ui_state.mouse.pos.x > r.pos.x &&
	   ui_state.mouse.pos.x < r.pos.x + r.size.x &&
	   ui_state.mouse.pos.y > r.pos.y &&
	   ui_state.mouse.pos.y < r.pos.y + r.size.y {
		return true
	}
	return false
}

do_button :: proc(label: string, r: utils.Rect, id: i32) -> bool {
	assert(id > 0)
	button_id := UIID({id = id})
	append(&ui_state.elems, Elem({id = button_id, rect = r}))

	if ui_state.mouse.last_action == .Up && ui_state.active == button_id {
		ui_state.active = {}
		if mouse_is_inside(r) {
			return true
		} else {
			return false
		}
	}

	if mouse_is_inside(r) {
		ui_state.hot = button_id
		if ui_state.mouse.last_action == .Down {
			ui_state.active = button_id
			fmt.println("hello")
		}
	}

	return false
}

init :: proc(shader: u32) {
	ui_state.graphics.shader = shader
	ui_state.elems = make([dynamic]Elem)
	ui_state.graphics.colors = {
		base   = ({123, 223, 242} / 255),
		hot    = ({178, 247, 239} / 255),
		active = ({247, 214, 224} / 255),
	}

}

update :: proc(
	last_mouse_action: utils.MouseAction,
	mouse_position: utils.Vec2f,
) {
	clear(&ui_state.elems)
	ui_state.hot = {}
	ui_state.mouse.last_action = last_mouse_action
	ui_state.mouse.pos = mouse_position
}

render :: proc(screen_size: utils.Vec2f) {
	for elem in ui_state.elems {
		color := ui_state.graphics.colors.base
		if ui_state.hot == elem.id do color = ui_state.graphics.colors.hot
		if ui_state.active == elem.id do color = ui_state.graphics.colors.active

		{
			error := gl.GetError()
			if error != 0 {
				fmt.println(error)
				panic("OpenGL error")
			}
		}

		shader := ui_state.graphics.shader
		gl.UseProgram(shader)

		screen_rect := utils.position_rect(elem.rect, screen_size)

		utils.set_val(shader, "r", screen_rect)
		utils.set_val(shader, "color", color)

		gl.DrawArrays(gl.TRIANGLES, 0, 6)
	}
}
