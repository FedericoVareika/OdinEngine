package OdinEngine

import "core:fmt"
import "core:log"
import "core:math"
import "core:math/linalg"
import "core:os"

import "base:intrinsics"
import "base:runtime"

import gl "vendor:OpenGL"
import "vendor:glfw"

import ttf "TTFonting"
import "UI"
import "utils"

WIDTH :: 1080
HEIGHT :: 600
TITLE :: "Triangle!"

GL_MINOR_VERSION :: 3
GL_MAJOR_VERSION :: 3

wireframe_mode := false
rotating := false

state: State

opposite := false
targeting: ^utils.Vec2f = nil
b: [3]utils.Vec2f = {
	{0.2 * WIDTH, 0.2 * HEIGHT},
	{0.9 * WIDTH, 0.2 * HEIGHT},
	{0.9 * WIDTH, 0.9 * HEIGHT},
}

selected_vert := 0
font_size: f32 = 25
font_smoothness: f32 = 1
// inside_val: f32 = 1
cutoff: f32 = 0.005
subpixel: bool = true

multisample := false
toggle_multisample := false

// letter :: #config(LETTER, '7')
glyf_sbos: utils.GlyfSBOs
glyf_info: [128]utils.GlyfInfo
ttf_info: ttf.TTFInfo
glyf_metrics: ttf.ASCII_Metrics

main :: proc() {
	context.logger = log.create_console_logger()
	glfw.Init()
	defer glfw.Terminate()

	{
		using glfw

		when ODIN_OS == .Darwin { 	// Note(fede): some features probably are not compatible with macOS
			WindowHint(CONTEXT_VERSION_MAJOR, 3)
            WindowHint(CONTEXT_VERSION_MINOR, 3)
			WindowHint(OPENGL_FORWARD_COMPAT, gl.TRUE)
		} else {
			WindowHint(CONTEXT_VERSION_MAJOR, 4)
            WindowHint(CONTEXT_VERSION_MINOR, 5)
		}

		WindowHint(OPENGL_PROFILE, OPENGL_CORE_PROFILE)
		WindowHint(SAMPLES, 4)
	}

	window: glfw.WindowHandle
	{
		window = glfw.CreateWindow(WIDTH, HEIGHT, TITLE, nil, nil)
		if window == nil {
			fmt.eprintf("Error creating window\n")
			return
		}

		glfw.MakeContextCurrent(window)
		glfw.SetInputMode(window, glfw.CURSOR, glfw.CURSOR_NORMAL)
	}
	defer glfw.DestroyWindow(window)

	{
		gl.load_up_to(3, 3, glfw.gl_set_proc_address)
		gl.Enable(gl.DEPTH_TEST)
		gl.Enable(gl.BLEND)
		// gl.Disable(gl.MULTISAMPLE)
		gl.Disable(gl.MULTISAMPLE)
        glfw.WindowHint(glfw.SAMPLES, 16)

		gl.BlendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA)
		// gl.BlendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA)

		// Enable v-sync
		glfw.SwapInterval(1)

		gl.Enable(gl.VERTEX_PROGRAM_POINT_SIZE)
		gl.Enable(gl.PROGRAM_POINT_SIZE)
	}

	// Callbacks
	{
		using glfw
		SetErrorCallback(error_callback)
		SetFramebufferSizeCallback(window, framebuffer_size_callback)
		SetKeyCallback(window, key_callback)
		SetCursorPosCallback(window, mouse_pos_callback)
		SetMouseButtonCallback(window, mouse_button_callback)
		SetScrollCallback(window, mouse_scroll_callback)
	}

	// State
	{
		state = {}
		using state

		// TODO: clean up memory 
		// note(fede): ya fue igual
		graphics.VAOs = make(map[string]u32)
		graphics.VBOs = make(map[string]u32)
		graphics.EBOs = make(map[string]u32)
		graphics.indices = make(map[u32]u32)
		graphics.shaders = make(map[string]u32)

		camera.position = {0, 0, 5}
		camera.direction = {0, 0, 1}
		camera.up = {0, 1, 0}
		camera.euler_angles.yaw = math.to_radians(f32(-90.0))

		window.aspect_ratio = f32(WIDTH) / f32(HEIGHT)
		window.fovy = linalg.to_radians(f32(100.0))
		window.near = 0.1
		window.far = 100
		window.size = {f32(WIDTH), f32(HEIGHT)}

		transforms.proj = linalg.matrix4_perspective(
			window.fovy,
			window.aspect_ratio,
			window.near,
			window.far,
		)

		mouse.sensitivity = 1
	}
	defer {
		delete(state.graphics.VAOs)
		delete(state.graphics.VBOs)
		delete(state.graphics.EBOs)
		delete(state.graphics.indices)
		delete(state.graphics.shaders)
	}


	{
		ok: bool
		state.graphics.shaders["cube"], ok = utils.create_shader_program(
			"cube",
		)
		if !ok do return

		state.graphics.shaders["light_source"], ok =
			utils.create_shader_program("light_source")
		if !ok do return

		state.graphics.shaders["rect"], ok = utils.create_shader_program(
			"rect",
		)
		if !ok do return

		state.graphics.shaders["bezier"], ok = utils.create_shader_program(
			"bezier",
		)
		if !ok do return

		state.graphics.shaders["font"], ok = utils.create_shader_program(
			"font",
		)
		if !ok do return
	}

	// Texture parameters
	utils.init_stbi()
	utils.load_textures(texture_path, texture_filenames, &state.textures)

	// Vertex Array Object: https://learnopengl.com/Getting-started/Hello-Triangle
	{
		using gl
		defer BindBuffer(ARRAY_BUFFER, 0)
		VBO: u32
		GenBuffers(1, &VBO)
		BindBuffer(ARRAY_BUFFER, VBO)
		raw_vertices := raw_data(vertices[:])
		BufferData(
			ARRAY_BUFFER,
			size_of(vertices[0]) * len(vertices),
			raw_vertices,
			STATIC_DRAW,
		)

		state.graphics.VBOs["cube"] = VBO
	}

	{
		glyfs, metrics, info := ttf.parse_ttf(
			// "assets/fonts/JetBrains/JetBrainsMono-Light.ttf", // "assets/fonts/JetBrains/JetBrainsMono-Regular.ttf",
			"assets/fonts/IosevkaTermNerdFontMono-Light.ttf",
			// "assets/fonts/IosevkaCustom-Light.ttf",
		)
		ttf_info = info
		glyf_metrics = metrics
		glyf_sbos, glyf_info = utils.load_glyphs(glyfs, ttf_info)
	}

	// UI
	{
		UI.init(
			state.graphics.shaders["rect"],
			state.graphics.shaders["font"],
			u32(ttf_info.x_min),
			u32(ttf_info.x_max),
			u32(ttf_info.y_min),
			u32(ttf_info.y_max),
			glyf_info,
			glyf_metrics,
		)
	}


	{
		using gl
		defer BindBuffer(ELEMENT_ARRAY_BUFFER, 0)
		EBO: u32
		GenBuffers(1, &EBO)
		BindBuffer(ELEMENT_ARRAY_BUFFER, EBO)
		raw_indices := raw_data(indices[:])
		index_count: u32 = size_of(type_of(indices[0])) * len(indices)
		BufferData(
			ELEMENT_ARRAY_BUFFER,
			int(index_count),
			raw_indices,
			STATIC_DRAW,
		)

		state.graphics.EBOs["cube"] = EBO
	}

	{
		using gl
		defer BindBuffer(ARRAY_BUFFER, 0)
		defer BindBuffer(ELEMENT_ARRAY_BUFFER, 0)

		VAO: u32

		GenVertexArrays(1, &VAO)
		BindVertexArray(VAO)
		defer BindVertexArray(0)

		BindBuffer(ARRAY_BUFFER, state.graphics.VBOs["cube"])
		BindBuffer(ELEMENT_ARRAY_BUFFER, state.graphics.EBOs["cube"])
		state.graphics.VAOs["cube"] = VAO
		state.graphics.indices[VAO] = len(indices) * size_of(indices[0])

		// Position vec
		VertexAttribPointer(
			0,
			3,
			FLOAT,
			false,
			size_of(utils.Vertex),
			auto_cast 0,
		)
		EnableVertexAttribArray(0)

		// Normal vec
		VertexAttribPointer(
			1,
			3,
			FLOAT,
			false,
			size_of(utils.Vertex),
			auto_cast size_of(utils.Vec3f),
		)
		EnableVertexAttribArray(1)

		// Tex_coord vec
		VertexAttribPointer(
			2,
			2,
			FLOAT,
			false,
			size_of(utils.Vertex),
			auto_cast size_of(utils.Vec3f) * 2,
		)
		EnableVertexAttribArray(2)
	}

	{
		using gl
		defer BindBuffer(ARRAY_BUFFER, 0)
		defer BindBuffer(ELEMENT_ARRAY_BUFFER, 0)

		VAO: u32

		GenVertexArrays(1, &VAO)
		BindVertexArray(VAO)
		defer BindVertexArray(0)

		BindBuffer(ARRAY_BUFFER, state.graphics.VBOs["cube"])
		BindBuffer(ELEMENT_ARRAY_BUFFER, state.graphics.EBOs["cube"])
		state.graphics.VAOs["light_source"] = VAO
		state.graphics.indices[VAO] = len(indices) * size_of(indices[0])

		// Position vec
		VertexAttribPointer(
			0,
			3,
			FLOAT,
			false,
			size_of(utils.Vertex),
			auto_cast 0,
		)
		EnableVertexAttribArray(0)

		// Normal vec
		VertexAttribPointer(
			1,
			3,
			FLOAT,
			false,
			size_of(utils.Vertex),
			auto_cast size_of(utils.Vec3f),
		)
		EnableVertexAttribArray(1)
	}

	{
		using gl
		// Texture uniforms
		shader := state.graphics.shaders["cube"]
		UseProgram(shader)
		utils.set_val(shader, "light_color", light_color)
		utils.set_val(shader, "light.ambient", light_color * 0.3)
		utils.set_val(shader, "light.diffuse", light_color * 0.5)
		utils.set_val(shader, "light.specular", utils.Vec3f({1.0, 1.0, 1.0}))
		utils.set_f32(shader, "light.k_c", 1.0)
		utils.set_f32(shader, "light.k_l", 0.09)
		utils.set_f32(shader, "light.k_q", 0.032)
		// set_f32(shader, "light.cutoff", math.cos(math.to_radians(f32(30.0))))
		utils.set_f32(shader, "light.cutoff", 0)

		utils.set_val(
			shader,
			"material.diffuse",
			i32(utils.Texture.Container2),
		)
		utils.set_val(
			shader,
			"material.specular",
			i32(utils.Texture.Container2_specular),
		)

		shader = state.graphics.shaders["light_source"]
		UseProgram(shader)
		utils.set_val(shader, "light_color", light_color)

		// utils.set_val(
		// 	state.graphics.shaders["cube"],
		// 	"texture2",
		// 	i32(utils.Texture.Awesomeface),
		// )
	}

	fmt.println(state.textures)

	load_cubes()

	load_light :: false

	// Light source
	if light_vec.w == 1 && load_light {
		m: utils.Model = {}
		t := utils.init_transform()
		t = utils.scale(t, 0.25)
		t = utils.translate(t, light_vec.xyz)
		m.transform = t
		m.VAO = state.graphics.VAOs["light_source"]
		m.shader = state.graphics.shaders["light_source"]
		append(&state.models, m)
	}

	gl.PolygonMode(gl.FRONT_AND_BACK, wireframe_mode ? gl.LINE : gl.FILL)
	fmt.println(state)
	for !glfw.WindowShouldClose(window) {
		process_input(window)
		UI.update(state.mouse.last_action, state.mouse.pos)

		update()

		render()

		{
			state.mouse.last_action = .Idle
		}

		// check and call events and swap the buffers
		glfw.SwapBuffers(window)
		glfw.PollEvents()

		{
			error := gl.GetError()
			if error != 0 {
				fmt.println(error)
				panic("OpenGL error")
			}
		}
	}
}

load_cubes :: proc() {
	positions := [?]utils.Vec3f {
		{0.0, 0.0, 0.0},
		{2.0, 5.0, -15.0},
		{-1.5, -2.2, -2.5},
		{-3.8, -2.0, -12.3},
		{2.4, -0.4, -3.5},
		{-1.7, 3.0, -7.5},
		{1.3, -2.0, -2.5},
		{1.5, 2.0, -2.5},
		{1.5, 0.2, -1.5},
		{-1.3, 1.0, -1.5},
	}

	models: [len(positions)]utils.Model
	for &m, idx in models {
		t := utils.init_transform()
		angle := utils.Degrees(20 * idx)
		t = utils.scale(t, 0.5)
		t = utils.rotate(t, angle, {1, 0.3, 0.5})
		t = utils.translate(t, positions[idx])
		m.transform = t
		m.VAO = state.graphics.VAOs["cube"]
		m.shader = state.graphics.shaders["cube"]
	}

	append(&state.models, ..models[:])
}

process_input :: proc(window: glfw.WindowHandle) {
	/*
	using glfw
	if GetKey(window, KEY_ESCAPE) == PRESS || GetKey(window, KEY_Q) == PRESS {
		SetWindowShouldClose(window, true)
	}

	if GetKey(window, KEY_SPACE) == PRESS {
		// wireframe_mode = !wireframe_mode
		// gl.PolygonMode(gl.FRONT_AND_BACK, wireframe_mode ? gl.LINE : gl.FILL)
	}
    */
}

update :: proc() {
	using utils

	move_camera(&state)
	rotate_camera(&state)

	{
		using state.camera
		state.transforms.view = linalg.matrix4_look_at(
			position,
			position - direction,
			up,
		)

		shader := state.graphics.shaders["cube"]
		gl.UseProgram(shader)

		{
			set_val(shader, "light.vector", state.transforms.view * light_vec)
			set_vec4(shader, "light.vector", {0.5, -0.5, 0, 1})
		}
	}

	if UI.do_button("Prev", Rect({{20, 20}, {100, 50}}), 1) {
		selected_vert -= 1
		// selected_vert %%= glyf_npoints 
		log.debug("prev selected vert", selected_vert)
	}

	if UI.do_button("Next", Rect({{140, 20}, {100, 50}}), 2) {
		selected_vert += 1
		// selected_vert %%= glyf_npoints 
		log.debug("next selected vert", selected_vert)
	}

	next_y: f32 = 100

	UI.do_slider(
		"Font Size",
		Rect({{20, next_y}, {200, 50}}),
		&font_size,
		1,
		100,
		3,
	)

    if UI.do_button("+", Rect({{120, next_y}, {50, 50}}), 30) do font_size += 1
    if UI.do_button("-", Rect({{180, next_y}, {50, 50}}), 31) do font_size -= 1

	next_y += 70

	builder := strings.builder_make(context.temp_allocator)
	// strings.write_int(&builder, int(font_size), 10)
	strings.write_f32(&builder, font_size, 'f')
	UI.do_text(strings.to_string(builder), Rect({{240, next_y}, {50, 50}}))
	next_y += 70

	UI.do_slider(
		"Font Smoothness",
		Rect({{20, next_y}, {200, 50}}),
		&font_smoothness,
		0,
		100,
		4,
	)

	builder = strings.builder_make(context.temp_allocator)
	strings.write_f32(&builder, font_smoothness, 'f')
	UI.do_text(strings.to_string(builder), Rect({{240, next_y}, {50, 50}}))
	next_y += 70

	// UI.do_slider(
	// 	"Inside Val",
	// 	Rect({{20, next_y}, {200, 50}}),
	// 	&inside_val,
	// 	0,
	// 	1,
	// 	5,
	// )

    builder = strings.builder_make(context.temp_allocator)
	// strings.write_f32(&builder, inside_val, 'f')
	UI.do_text(strings.to_string(builder), Rect({{240, next_y}, {50, 50}}))
    next_y += 70

	if UI.do_button("Toggle MSAA", Rect({{20, next_y}, {100, 50}}), 6) {
		if !multisample do gl.Enable(gl.MULTISAMPLE)
		else do gl.Disable(gl.MULTISAMPLE)

		multisample = !multisample
	}

	if multisample {
		UI.do_text("On", Rect{{150, next_y}, {50, 50}})
        if UI.do_button("4x", Rect{{220, next_y}, {25, 50}}, 61) {
            glfw.WindowHint(glfw.SAMPLES, 4)
        }
        if UI.do_button("16x", Rect{{250, next_y}, {25, 50}}, 62) {
            glfw.WindowHint(glfw.SAMPLES, 16)
        }
	} else {
		UI.do_text("Off", Rect{{150, next_y}, {50, 50}})
	}

    next_y += 70

	if UI.do_button("Toggle Subpixel", Rect({{20, next_y}, {100, 50}}), 7) {
		subpixel = !subpixel
	}

	if subpixel do UI.do_text("On", Rect{{150, next_y}, {50, 50}})
	else do UI.do_text("Off", Rect{{150, next_y}, {50, 50}})

    next_y += 70

    UI.do_slider("Cutoff", 
		Rect({{20, next_y}, {200, 50}}),
		&cutoff,
		0,
		1,
		8,
    )

    builder = strings.builder_make(context.temp_allocator)
	strings.write_f32(&builder, cutoff, 'f')
	UI.do_text(strings.to_string(builder), Rect({{240, next_y}, {50, 50}}))

	update_time()
}

import "core:strings"

update_time :: proc() {
	now := glfw.GetTimerValue()
	then := state.time.last_frame

	state.time.delta_value = now - then
	state.time.last_frame = now

	state.time.delta_s =
		f64(state.time.delta_value) / f64(glfw.GetTimerFrequency())
}

render :: proc() {
	using gl
	// gl.ClearColor(0.96, 0.93, 0.89, 1)
	gl.ClearColor(1, 1, 1, 1)
	// gl.ClearColor(0, 0, 0, 1)
	gl.Clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT)

	for model, idx in state.models {
		gl.BindVertexArray(model.VAO)

		shader := model.shader
		gl.UseProgram(shader)


		if state.graphics.shaders["cube"] == shader {
			normal_matrix: utils.Mat3 = utils.Mat3(
				linalg.inverse_transpose(
					state.transforms.view * model.transform,
				),
			)

			obj_color: utils.Vec3f = {
				f32(math.sin(f32(idx) * 0.5)),
				f32(math.cos(f32(idx) * 0.5)),
				f32(math.tan(f32(idx) * 0.5)),
			}

			utils.set_val(shader, "normal_matrix", normal_matrix)
			// set_val(shader, "material.ambient", obj_color)
			// set_val(shader, "material.diffuse", utils.Vec3f({1.0, 0.5, 0.31}))
			// set_val(shader, "material.specular", utils.Vec3f({0.5, 0.5, 0.5}))
			utils.set_val(shader, "material.shininess", f32(32.0))
		}

		// t := state.transforms.proj * state.transforms.view * model.transform

		utils.set_val(model.shader, "model", model.transform)
		utils.set_val(model.shader, "view", state.transforms.view)
		utils.set_val(model.shader, "projection", state.transforms.proj)

		for id, texture in state.textures {
			ActiveTexture(TEXTURE0 + u32(texture))
			BindTexture(TEXTURE_2D, id)
		}

		gl.DrawElements(
			gl.TRIANGLES,
			i32(state.graphics.indices[model.VAO]),
			gl.UNSIGNED_INT,
			nil,
		)
	}

	// gl.DrawElements(gl.TRIANGLES, len(indices) * 3, gl.UNSIGNED_INT, nil)

	font := state.graphics.shaders["font"]
	gl.UseProgram(font)
    utils.set_val(font, "pixel_size", 2 / (state.window.size.y))
    utils.set_val(font, "cutoff", cutoff)
    utils.set_val(font, "subpixel", subpixel)
    gl.UseProgram(0)

	UI.render(state.window.size, font_size, font_smoothness)
	gl.UseProgram(font)
    utils.set_val(font, "pixel_size", 2 / (state.window.size.y))
    utils.set_val(font, "cutoff", cutoff)
    utils.set_val(font, "subpixel", subpixel)
    gl.UseProgram(0)
	render_glyf()
}

render_glyf :: proc() {
	font := state.graphics.shaders["font"]
	gl.UseProgram(font)

	hello := `
%*!@#$^&_=-
abcdefghijklmnopqrstuvwxyz
ABCDEFGHIJKLMNOPQRSTUVWXYZ
123456789
{[(<>)]}+?'":|,~\
`

    hello = ""


	hello = "The quick brown fox jumps over the lazy dog"
	// hello = "&"
	// hello = "abcdefghijklmnop"

	screen_scale :=
		utils.Vec2f{1 / state.window.size.x, 1 / state.window.size.y} * 2

	utils.set_val(font, "inside_val", 1.0)
	utils.set_val(font, "screen_scale", screen_scale)
	utils.set_val(font, "smoothness", font_smoothness)

	pts: f32 = font_size * 10
	font_size_mult := pts / f32(ttf_info.y_max - ttf_info.y_min)
	utils.set_val(font, "font_size_mult", font_size_mult)

	utils.set_val(font, "color", utils.Vec3f{0, 0, 0})

	non_scaled_translation := utils.Vec2f{200, 200}
	scaled_translation := utils.Vec2f{-1, 1}

	for char in hello {
		UI.draw_letter(
			char,
			font,
            pts,
			font_size_mult,
			&non_scaled_translation,
			&scaled_translation,
			glyf_info[char],
			glyf_metrics[char],
		)
	}
}
