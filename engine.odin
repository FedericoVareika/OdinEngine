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

letter :: #config(LETTER, 'A')
glyf_vec_sbo: u32
glyf_on_curves_sbo: u32
glyf_indices_sbo: u32
glyf_uvs_sbo: u32
glyf_triangles: []utils.Triangle
glyf_uvs: []utils.TriangleUV
glyf: ttf.Glyf
glyf_npoints: int

main :: proc() {
    context.logger = log.create_console_logger()
	glfw.Init()
	defer glfw.Terminate()

	{
		using glfw

		when ODIN_OS == .Darwin { 	// Note(fede): some features probably are not compatible with macOS
			WindowHint(CONTEXT_VERSION_MAJOR, 3)
			WindowHint(OPENGL_FORWARD_COMPAT, gl.TRUE)
		} else {
			WindowHint(CONTEXT_VERSION_MAJOR, 4)
		}

		WindowHint(CONTEXT_VERSION_MINOR, 3)
		WindowHint(OPENGL_PROFILE, OPENGL_CORE_PROFILE)
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
		// gl.BlendFunc(gl.ONE, gl.ONE_MINUS_SRC_ALPHA)
		gl.BlendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA)

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

	// UI
	{
		UI.init(state.graphics.shaders["rect"])
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
        glyfs := ttf.parse_ttf("assets/fonts/JetBrainsMono-Thin.ttf")

        glyf := glyfs['9']


		coords := glyf.value.(ttf.SimpleGlyf).coords
		fmt.println(coords)
		fmt.println(glyf.description)

		end_contours : []u16
        defer delete(end_contours)
		{
            end_contours_be := glyf.value.(ttf.SimpleGlyf).end_pts_of_contours
            end_contours = make([]u16, len(end_contours_be))
			for &c, idx in end_contours {
				c = cast(u16)end_contours_be[idx]
			}
		}

		bounding_rect: utils.Rect = {
			pos  = {f32(glyf.description.x_min), f32(glyf.description.y_min)},
			size = {
				f32(glyf.description.x_max - glyf.description.x_min),
				f32(glyf.description.y_max - glyf.description.y_min),
			},
		}

        // inv_scale := if bounding_rect.size.x  
		fmt.println(bounding_rect)

		// glyf_coordinates := make([]utils.Coordf, len(coords))
		glyf_vertices := make([]utils.Vec2f, len(coords))
		glyf_on_curves := make([]b32, len(coords))

		for &v, idx in glyf_vertices {
			coord_f: utils.Vec2f = {f32(coords[idx].x), f32(coords[idx].y)}
			// fmt.printf("%f,%f\n", coord_f.x, coord_f.y)
			// fmt.println(coord_f, coords[idx])
			v = coord_f - bounding_rect.pos
			v /= bounding_rect.size.y
			v -= {0.5, 0.5}
			fmt.printf("%f,%f\n", v.x, v.y)
		}

		for &c, idx in glyf_on_curves {
			c = b32(coords[idx].on_curve)
		}

		fmt.println(len(glyf_vertices))
        glyf_npoints = len(glyf_vertices)
		// assert(len(glyf_vertices) == 12)
		// assert(len(glyf_vertices) == 4)

		{
			x, y, on_curve := soa_unzip(coords)
			glyf_triangles, glyf_uvs = utils.triangulate_vertices(
				soa_zip(x = x, y = y),
				end_contours,
                on_curve,
			)
			fmt.println("glyf_triangles", glyf_triangles)
            log.info("uvs size", size_of(glyf_uvs))
            log.info("uvs size", size_of(glyf_uvs[0]))
		}

		using gl
		defer BindBuffer(SHADER_STORAGE_BUFFER, 0)

		SBOs: [4]u32
		GenBuffers(4, &SBOs[0])
		fmt.println(SBOs)

		BindBuffer(SHADER_STORAGE_BUFFER, SBOs[0])
		BufferData(
			SHADER_STORAGE_BUFFER,
			size_of(utils.Vec2f) * len(glyf_vertices),
			raw_data(glyf_vertices[:]),
			STATIC_DRAW,
		)
		BindBufferBase(SHADER_STORAGE_BUFFER, 0, SBOs[0])
		glyf_vec_sbo = SBOs[0]

		BindBuffer(SHADER_STORAGE_BUFFER, SBOs[1])
		BufferData(
			SHADER_STORAGE_BUFFER,
			size_of(b32) * len(glyf_on_curves),
			raw_data(glyf_on_curves[:]),
			STATIC_DRAW,
		)
		BindBufferBase(SHADER_STORAGE_BUFFER, 1, SBOs[1])
		glyf_on_curves_sbo = SBOs[1]

		BindBuffer(SHADER_STORAGE_BUFFER, SBOs[2])
		BufferData(
			SHADER_STORAGE_BUFFER,
			size_of(utils.Triangle) * len(glyf_triangles),
			raw_data(glyf_triangles),
			STATIC_DRAW,
		)
		BindBufferBase(SHADER_STORAGE_BUFFER, 2, SBOs[2])
		glyf_indices_sbo = SBOs[2]

		BindBuffer(SHADER_STORAGE_BUFFER, SBOs[3])
		BufferData(
			SHADER_STORAGE_BUFFER,
			size_of(utils.TriangleUV) * len(glyf_uvs),
			raw_data(glyf_uvs),
			STATIC_DRAW,
		)
		BindBufferBase(SHADER_STORAGE_BUFFER, 1, SBOs[3])
		glyf_uvs_sbo = SBOs[3]

		{
			// Read back vertices SSBO
			vertices_data := make([]utils.Vec2f, len(glyf_vertices))
			BindBuffer(SHADER_STORAGE_BUFFER, SBOs[0])
			gl.GetBufferSubData(
				SHADER_STORAGE_BUFFER,
				0,
				size_of(utils.Vec2f) * len(glyf_vertices),
				raw_data(vertices_data),
			)
			log.debug("Vertices SSBO:", vertices_data)

			// Read back on_curves SSBO
			on_curves_data := make([]b32, len(glyf_on_curves))
			BindBuffer(SHADER_STORAGE_BUFFER, SBOs[1])
			gl.GetBufferSubData(
				SHADER_STORAGE_BUFFER,
				0,
				size_of(b32) * len(glyf_on_curves),
				raw_data(on_curves_data),
			)
			log.debug("OnCurves SSBO:", on_curves_data)

			// Read back indices SSBO
			indices_data := make([]utils.Triangle, len(glyf_triangles))
			BindBuffer(SHADER_STORAGE_BUFFER, SBOs[2])
			gl.GetBufferSubData(
				SHADER_STORAGE_BUFFER,
				0,
				size_of(utils.Triangle) * len(glyf_triangles),
				raw_data(indices_data),
			)
			log.debug("Indices SSBO(", len(indices_data), ":", indices_data)

			uvs_data := make([]utils.TriangleUV, len(glyf_uvs))
			BindBuffer(SHADER_STORAGE_BUFFER, SBOs[3])
			gl.GetBufferSubData(
				SHADER_STORAGE_BUFFER,
				0,
				size_of(utils.TriangleUV) * len(glyf_uvs),
				raw_data(uvs_data),
			)
			log.debug("Uvs SSBO(", len(uvs_data), ":", uvs_data)
		}
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

	// if UI.do_button(
	// 	"Do something",
	// 	Rect({pos = {20, 20}, size = {100, 50}}),
	// 	1,
	// ) {
	// 	opposite = !opposite
	// }

	// {
	// 	if targeting != nil {
	// 		targeting^ = state.mouse.pos
	// 	}
	// }

	// b_size: utils.Vec2f = {20, 20}
	// for &b_i, idx in b {
	// 	r := utils.Rect({b_i - (b_size / 2), b_size})
	// 	if UI.do_button("", r, i32(idx) + 2) {
	// 		if targeting == nil do targeting = &b_i
	// 		else do targeting = nil
	// 	}
	// }

    if UI.do_button("Prev", Rect({{20, 20}, {100, 50}}), 1) {
        selected_vert -= 1 
        selected_vert %%= glyf_npoints 
        log.debug("prev selected vert", selected_vert)
    }
    
    if UI.do_button("Next", Rect({{140, 20}, {100, 50}}), 2) {
        selected_vert += 1 
        selected_vert %%= glyf_npoints 
        log.debug("next selected vert", selected_vert)
    }

	update_time()
}

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
	gl.ClearColor(0.96, 0.93, 0.89, 1)
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

	UI.render(state.window.size)
	// render_bezier()
	render_glyf()
}

render_bezier :: proc() {
	bezier := state.graphics.shaders["bezier"]
	gl.UseProgram(bezier)

	bezier_rect: utils.Rect = {
		pos  = {state.window.size.x / 2, 0},
		size = state.window.size / 2,
	}
	bezier_rect_norm := utils.position_rect(bezier_rect, state.window.size)
	utils.set_val(bezier, "r_norm", bezier_rect_norm)
	utils.set_val(bezier, "r_real", bezier_rect)

	b_norm: [len(b)]utils.Vec2f
	for b_i, idx in b {
		b_norm[idx] = utils.normalize_coord(b_i, state.window.size)
		b_norm[idx] = utils.center_coord(b_norm[idx])
	}

	// points := [?]utils.Vec2f{{0, 0}, {1, 0}, {1, 1}}

	utils.set_val(bezier, "t", b_norm[:])
	utils.set_val(bezier, "opposite", opposite)

	gl.DrawArrays(gl.TRIANGLES, 0, 3)
}

render_glyf :: proc() {
	font := state.graphics.shaders["font"]
	gl.UseProgram(font)
    utils.set_val(font, "selected_vert", i32(selected_vert))
	gl.DrawArrays(gl.TRIANGLES, 0, i32(len(glyf_triangles)) * 3)
	// gl.DrawArrays(gl.POINTS, 0, i32(len(glyf_triangles)) * 3)
	// gl.DrawBuffer(glyf_sbo)
}
