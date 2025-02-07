package utils

import "core:fmt"
import "core:strings"

import gl "vendor:OpenGL"
import stbi "vendor:stb/image"

init_stbi :: proc() {
	stbi.set_flip_vertically_on_load(1)
}

load_texture :: proc(
	filename: cstring,
	pixel_format: i32,
) -> (
	texture: u32,
	ok: bool,
) {
	width, height, nr_channels: i32
	data := stbi.load(filename, &width, &height, &nr_channels, 0)
	if data == nil {
		fmt.eprintf("Error loading %s with stbi\n", filename)
		return 0, false
	}

	defer stbi.image_free(data)

	{
		using gl
		GenTextures(1, &texture)
		BindTexture(TEXTURE_2D, texture)

		TexParameteri(TEXTURE_2D, TEXTURE_WRAP_S, MIRRORED_REPEAT)
		TexParameteri(TEXTURE_2D, TEXTURE_WRAP_T, MIRRORED_REPEAT)
		TexParameteri(TEXTURE_2D, TEXTURE_MIN_FILTER, LINEAR_MIPMAP_LINEAR)
		TexParameteri(TEXTURE_2D, TEXTURE_MAG_FILTER, LINEAR)

	}

	gl.TexImage2D(
		gl.TEXTURE_2D,
		0,
		cast(i32)pixel_format,
		width,
		height,
		0,
		cast(u32)pixel_format,
		gl.UNSIGNED_BYTE,
		data,
	)
	gl.GenerateMipmap(gl.TEXTURE_2D)

	return texture, true
}

load_textures :: proc(
	path: string,
	filenames: [Texture]string,
	textures_out: ^[Texture]u32,
) {
	for filename, idx in filenames {

		pixel_format: i32 = gl.RGB
		if filename[len(filename) - 3:] == "png" {
			pixel_format = gl.RGBA
		}

		full_path := strings.concatenate(
			[]string{path, "/", filename},
			context.temp_allocator,
		)

		texture, ok := load_texture(
			strings.unsafe_string_to_cstring(full_path),
			pixel_format,
		)
		if !ok {
			fmt.eprintf("Could not load texture %i (%s).\n", idx, filename)
			return
		}
		textures_out[idx] = texture
	}
}
