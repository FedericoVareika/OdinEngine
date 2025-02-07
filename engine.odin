package engine

import "core:fmt"
import "core:math"
import "core:math/linalg"
import "core:os"

import "base:runtime"

import gl "vendor:OpenGL"
import "vendor:glfw"

import "utils"

WIDTH :: 1080
HEIGHT :: 600
TITLE :: "Triangle!"

GL_MINOR_VERSION :: 3
GL_MAJOR_VERSION :: 3

wireframe_mode := false

state: State

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
		wireframe_mode = !wireframe_mode
		gl.PolygonMode(gl.FRONT_AND_BACK, wireframe_mode ? gl.LINE : gl.FILL)
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

main :: proc() {
	glfw.Init()
	defer glfw.Terminate()

	{
		using glfw
		WindowHint(CONTEXT_VERSION_MAJOR, 3)
		WindowHint(CONTEXT_VERSION_MINOR, 3)
		WindowHint(OPENGL_PROFILE, OPENGL_CORE_PROFILE)

		when ODIN_OS == .Darwin {
			WindowHint(OPENGL_FORWARD_COMPAT, gl.TRUE)
		}
	}

	window: glfw.WindowHandle
	{
		window = glfw.CreateWindow(WIDTH, HEIGHT, TITLE, nil, nil)
		if window == nil {
			fmt.eprintf("Error creating window\n")
			return
		}

		glfw.MakeContextCurrent(window)
	}
	defer glfw.DestroyWindow(window)

	{
		gl.load_up_to(3, 3, glfw.gl_set_proc_address)
		gl.Enable(gl.DEPTH_TEST)

		// Enable v-sync
		// glfw.SwapInterval(1)
	}

	// Callbacks
	{
		using glfw
		SetErrorCallback(error_callback)
		SetFramebufferSizeCallback(window, framebuffer_size_callback)
		SetKeyCallback(window, key_callback)
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

		transforms.proj = linalg.matrix4_perspective(
			window.fovy,
			window.aspect_ratio,
			window.near,
			window.far,
		)
	}

	{
		ok: bool
		state.graphics.shaders["cube"], ok = create_shader_program("cube")
		if !ok do return
		state.graphics.shaders["light_source"], ok = create_shader_program(
			"light_source",
		)
		if !ok do return
	}

	// Texture parameters
	init_stbi()
	load_textures(texture_path, texture_filenames, &state.textures)

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
		set_val(shader, "light_color", light_color)
		set_val(shader, "light.ambient", light_color * 0.3)
		set_val(shader, "light.diffuse", light_color * 0.5)
		set_val(shader, "light.specular", utils.Vec3f({1.0, 1.0, 1.0}))
		set_f32(shader, "light.k_c", 1.0)
		set_f32(shader, "light.k_l", 0.09)
		set_f32(shader, "light.k_q", 0.032)
		// set_f32(shader, "light.cutoff", math.cos(math.to_radians(f32(30.0))))
		set_f32(shader, "light.cutoff", 0)

		set_val(shader, "material.diffuse", i32(utils.Texture.Container2))
		set_val(
			shader,
			"material.specular",
			i32(utils.Texture.Container2_specular),
		)

		shader = state.graphics.shaders["light_source"]
		UseProgram(shader)
		set_val(shader, "light_color", light_color)

		// set_val(
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
		m.material = {0, state.graphics.shaders["light_source"]}
		append(&state.models, m)
	}

	gl.PolygonMode(gl.FRONT_AND_BACK, wireframe_mode ? gl.LINE : gl.FILL)
	fmt.println(state)
	for !glfw.WindowShouldClose(window) {
		process_input(window)

		update()
		update_time()

		render()

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
		m.material = {0, state.graphics.shaders["cube"]}
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

		shader := model.material.shader
		gl.UseProgram(shader)

		if state.graphics.shaders["cube"] == shader {
			normal_matrix: utils.Mat3 = utils.Mat3(
				linalg.inverse_transpose(
					state.transforms.view * model.transform,
				),
			)

			obj_color: utils.Vec3f =  {
				f32(math.sin(f32(idx) * 0.5)),
				f32(math.cos(f32(idx) * 0.5)),
				f32(math.tan(f32(idx) * 0.5)),
			}

			set_val(shader, "normal_matrix", normal_matrix)
			// set_val(shader, "material.ambient", obj_color)
			// set_val(shader, "material.diffuse", utils.Vec3f({1.0, 0.5, 0.31}))
			// set_val(shader, "material.specular", utils.Vec3f({0.5, 0.5, 0.5}))
			set_val(shader, "material.shininess", 32.0)
		}

		// t := state.transforms.proj * state.transforms.view * model.transform

		set_val(model.material.shader, "model", model.transform)
		set_val(model.material.shader, "view", state.transforms.view)
		set_val(model.material.shader, "projection", state.transforms.proj)

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


	gl.DrawElements(gl.TRIANGLES, len(indices) * 3, gl.UNSIGNED_INT, nil)
}
