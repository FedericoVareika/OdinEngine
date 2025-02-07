package engine

import "core:fmt"
import "core:math/linalg"
import "core:os"
import "core:strings"

import gl "vendor:OpenGL"

import "utils"

get_shader :: proc(
	shader_path: string,
	shader_type: u32,
) -> (
	shader_id: u32,
	ok: bool,
) {
	data := os.read_entire_file(shader_path, context.allocator) or_return
	source := cstring(raw_data(data))
	// fmt.printf("source:\n%s\n", source)

	// Compile the shader source
	{
		shader_id = gl.CreateShader(shader_type)
		gl.ShaderSource(shader_id, 1, &source, nil)
		gl.CompileShader(shader_id)
	}

	// Check for compilation errors
	{
		success: i32
		gl.GetShaderiv(shader_id, gl.COMPILE_STATUS, &success)
		if success == 0 {
			info_log: [512]u8
			gl.GetShaderInfoLog(shader_id, 512, nil, raw_data(info_log[:]))
			fmt.eprintf(
				"Error compiling shader at %s: \n%s",
				shader_path,
				info_log,
			)
			return 0, false
		}
	}

	return shader_id, true
}

create_shader_program_from_ids :: proc(
	shaders: ..u32,
) -> (
	shader_program: u32,
	ok: bool,
) {
	shader_program = gl.CreateProgram()
	for shader in shaders {
		gl.AttachShader(shader_program, shader)
	}

	gl.LinkProgram(shader_program)
	{
		success: i32
		gl.GetProgramiv(shader_program, gl.LINK_STATUS, &success)
		if success == 0 {
			info_log: [512]u8
			gl.GetProgramInfoLog(
				shader_program,
				512,
				nil,
				raw_data(info_log[:]),
			)
			fmt.eprintf("Error linking shader program: \n%s", info_log)
			return 0, false
		}
	}

	return shader_program, true
}

create_shader_program_from_name :: proc(
	shader_name: string,
) -> (
	shader_program: u32,
	ok: bool,
) {
	path := strings.concatenate(
		{"./assets/shaders/", shader_name},
		context.temp_allocator,
	)

	vertex_shader_id, fragment_shader_id: u32
	vertex_shader_id, ok = get_shader(
		strings.concatenate({path, ".vert"}, context.temp_allocator),
		gl.VERTEX_SHADER,
	)
	if !ok {
		fmt.eprintf("Could not load vertex shader\n")
		return
	}
	defer gl.DeleteShader(vertex_shader_id)
	fmt.println("Vertex shader id: ", vertex_shader_id)

	fragment_shader_id, ok = get_shader(
		strings.concatenate({path, ".frag"}, context.temp_allocator),
		gl.FRAGMENT_SHADER,
	)
	if !ok {
		fmt.eprintf("Could not load fragment shader\n")
		return
	}
	defer gl.DeleteShader(fragment_shader_id)
	fmt.println("Fragment shader id: ", fragment_shader_id)

	shader_program, ok = create_shader_program_from_ids(
		vertex_shader_id,
		fragment_shader_id,
	)
	if !ok {
		fmt.eprintf("Could not create the shader program\n")
		return
	}

	return shader_program, true
}

create_shader_program :: proc {
	create_shader_program_from_ids,
	create_shader_program_from_name,
}

set_val :: proc {
	set_int,
    set_f32,
	set_vec3,
	set_vec4,
	set_mat4,
	set_mat3,
}

set_int :: proc(shader_program: u32, name: cstring, value: i32) {
	gl.Uniform1i(gl.GetUniformLocation(shader_program, name), value)
}

set_f32 :: proc(shader_program: u32, name: cstring, value: f32) {
	gl.Uniform1f(gl.GetUniformLocation(shader_program, name), value)
}

set_vec3 :: proc(shader_program: u32, name: cstring, value: utils.Vec3f) {
	gl.Uniform3f(
		gl.GetUniformLocation(shader_program, name),
		value.x, value.y, value.z,
	)
}

set_vec4 :: proc(shader_program: u32, name: cstring, value: utils.Vec4f) {
	gl.Uniform4f(
		gl.GetUniformLocation(shader_program, name),
		value.x, value.y, value.z, value.w,
	)
}

set_mat4 :: proc(shader_program: u32, name: cstring, value: utils.Mat4) {
	flat := linalg.matrix_flatten(value)
	gl.UniformMatrix4fv(
		gl.GetUniformLocation(shader_program, name),
		1,
		false,
		raw_data(&flat),
	)
}

set_mat3 :: proc(shader_program: u32, name: cstring, value: utils.Mat3) {
	flat := linalg.matrix_flatten(value)
	gl.UniformMatrix3fv(
		gl.GetUniformLocation(shader_program, name),
		1,
		false,
		raw_data(&flat),
	)
}
