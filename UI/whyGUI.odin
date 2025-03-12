package UI

import ttf "../TTFonting"
import "../utils"

import "core:fmt"
import "core:log"
import "core:math"

import gl "vendor:OpenGL"

UIID :: struct {
	id: i32,
}

Elem :: struct {
	id:         UIID,
	label:      string,
	rect:       utils.Rect,
	background: bool,
}

UI_State :: struct {
	hot, active: UIID,
	mouse:       struct {
		pos:         utils.Vec2f,
		prev_pos:    utils.Vec2f,
		delta_pos:   utils.Vec2f,
		last_action: utils.MouseAction,
	},
	elems:       [dynamic]Elem,
	graphics:    struct {
		shader:      u32,
		font_shader: u32,
		glyfs:       struct {
			xmin, xmax, ymin, ymax: u32,
			info:                   [128]utils.GlyfInfo,
			metrics:                ttf.ASCII_Metrics,
		},
		colors:      struct {
			base, hot, active, background: utils.Vec3f,
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
	append(&ui_state.elems, Elem({id = button_id, label = label, rect = r}))

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
		}
	}

	return false
}

lerp :: proc(from, to, weight: f32) -> (val: f32) {
	assert(weight <= 1)
	assert(weight >= 0)
	return (to - from) * weight + from
}

do_slider :: proc(
	label: string,
	bounding_rect: utils.Rect,
	val: ^f32,
	from, to: f32,
	id: i32,
) {
	assert(id > 0)
	slider_id := UIID({id = id})
	append(
		&ui_state.elems,
		Elem {
			id = UIID{-1},
			label = label,
			rect = bounding_rect,
			background = true,
		},
	)

	slider_rect := utils.Rect {
		pos  = {
			bounding_rect.pos.x +
			(9 * bounding_rect.size.x / 10) * ((val^ - from) / (to - from)),
			bounding_rect.pos.y,
		},
		size = {bounding_rect.size.x / 10, bounding_rect.size.y},
	}

	append(
		&ui_state.elems,
		Elem{id = slider_id, label = "", rect = slider_rect},
	)

	if ui_state.active == slider_id {
		delta_weight := ui_state.mouse.delta_pos.x
		delta_weight /= 9 * bounding_rect.size.x / 10
		// delta_weight = clamp(delta_weight, 0, 1)
		val^ = val^ + (delta_weight * (to - from))
		val^ = clamp(val^, from, to)

		if ui_state.mouse.last_action == .Up {
			ui_state.active = {}
		}
		return
	}

	if mouse_is_inside(slider_rect) {
		ui_state.hot = slider_id
		if ui_state.mouse.last_action == .Down {
			ui_state.active = slider_id
		}
	}

}

do_text :: proc(
	label: string,
	rect: utils.Rect,
) {
	append(
		&ui_state.elems,
		Elem {
			id = UIID{-1},
			label = label,
			rect = rect,
			background = true,
		},
	)
}

init :: proc(
	shader: u32,
	font_shader: u32,
	xmin, xmax, ymin, ymax: u32,
	glyf_info: [128]utils.GlyfInfo,
	glyf_metrics: ttf.ASCII_Metrics,
) {
	ui_state.graphics.shader = shader

	ui_state.graphics.font_shader = font_shader

	ui_state.graphics.glyfs.xmin = xmin
	ui_state.graphics.glyfs.xmax = xmax
	ui_state.graphics.glyfs.ymin = ymin
	ui_state.graphics.glyfs.ymax = ymax

	ui_state.graphics.glyfs.info = glyf_info
	ui_state.graphics.glyfs.metrics = glyf_metrics

	assert(shader != 0)
	assert(font_shader != 0)

	ui_state.elems = make([dynamic]Elem)
	ui_state.graphics.colors = {
		base       = ({123, 223, 242} / 255),
		hot        = ({178, 247, 239} / 255),
		active     = ({247, 214, 224} / 255),
		background = ({216, 245, 251}) / 255,
		// background = ({0, 0, 0}),
	}
}

update :: proc(
	last_mouse_action: utils.MouseAction,
	mouse_position: utils.Vec2f,
) {
	clear(&ui_state.elems)
	ui_state.hot = {}
	ui_state.mouse.last_action = last_mouse_action

	ui_state.mouse.prev_pos = ui_state.mouse.pos
	ui_state.mouse.pos = mouse_position

	ui_state.mouse.delta_pos = ui_state.mouse.pos - ui_state.mouse.prev_pos
}

draw_letter :: proc(
	letter: rune,
	font: u32,
	font_size_mult: f32,
	non_scaled_translation, scaled_translation: ^utils.Vec2f,
	glyf_info: utils.GlyfInfo,
	glyf_metrics: ttf.GlyfMetrics,
) {
	if letter == '\n' {
		non_scaled_translation.x = 0
		non_scaled_translation.y += 1100
		return
	}

	non_scaled_translation.x +=
		f32(glyf_metrics.hor_metric.adv_width) * font_size_mult

	utils.set_val(font, "non_scaled_translation", non_scaled_translation^)
	utils.set_val(font, "scaled_translation", scaled_translation^)

	utils.set_val(font, "vertex_offset", i32(glyf_info.vertex_offset))
	utils.set_val(font, "triangle_offset", i32(glyf_info.triangle_offset))
	gl.DrawArrays(gl.TRIANGLES, 0, i32(glyf_info.triangle_count) * 3)

}

render :: proc(
	screen_size: utils.Vec2f,
	font_size: f32,
	font_smoothness: f32 = 1,
) {
	#reverse for elem in ui_state.elems {
		color: utils.Vec3f
		if elem.background do color = ui_state.graphics.colors.background
		else do color = ui_state.graphics.colors.base

		if ui_state.hot == elem.id do color = ui_state.graphics.colors.hot
		if ui_state.active == elem.id do color = ui_state.graphics.colors.active

		shader := ui_state.graphics.shader
		gl.UseProgram(shader)

		screen_rect := utils.position_rect(elem.rect, screen_size)

		utils.set_val(shader, "r", screen_rect)
		utils.set_val(shader, "color", color)
		gl.DrawArrays(gl.TRIANGLES, 0, 6)

		{
			if len(elem.label) == 0 do continue

			font := ui_state.graphics.font_shader
			gl.UseProgram(font)

			screen_scale :=
				utils.Vec2f{1 / screen_size.x, 1 / screen_size.y} * 2

			utils.set_val(font, "inside_val", f32(1))
			utils.set_val(font, "screen_scale", screen_scale)
            utils.set_val(font, "smoothness", font_smoothness)

			pts: f32 = font_size
			font_size_mult :=
				pts /
				f32(
					ui_state.graphics.glyfs.ymax -
					ui_state.graphics.glyfs.ymin,
				)
			utils.set_val(font, "font_size_mult", font_size_mult)

			utils.set_val(font, "color", utils.Vec3f{0, 0, 0})

			non_scaled_translation := elem.rect.pos
			non_scaled_translation.y += (elem.rect.size.y + pts * 0.8) / 2
			scaled_translation := utils.Vec2f{-1, 1}

			for letter in elem.label {
				draw_letter(
					letter,
					font,
					font_size_mult,
					&non_scaled_translation,
					&scaled_translation,
					ui_state.graphics.glyfs.info[letter],
					ui_state.graphics.glyfs.metrics[letter],
				)
			}
		}

		{
			error := gl.GetError()
			if error != 0 {
				log.error("Error at elem:", elem)
				log.error(error)
				panic("OpenGL error")
			}
		}
	}
}
