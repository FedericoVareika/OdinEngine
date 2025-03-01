package utils

import "core:log"

import gl "vendor:OpenGL"

import ttf "../TTFonting"

/*
    indices[triangle_offsets[glyf]] + vertex_offsets[glyf]
 */

GlyfSBOs :: struct {
	vertex_sbo, triangle_sbo, uvs_sbo: u32,
}

GlyfInfo :: struct {
	vertex_offset, triangle_offset, triangle_count: u32,
}

load_glyphs :: proc(
	glyfs: ttf.ASCII_Glyfs,
	ttf_info: ttf.TTFInfo,
) -> (
	glyf_sbos: GlyfSBOs,
	glyf_info: [128]GlyfInfo,
) {
	max_verts := ttf_info.max_verts
	all_vertices := make([dynamic]Vec2f, len = 0, cap = max_verts * 128)
	all_triangles := make([dynamic]Triangle, len = 0, cap = max_verts * 128)
	all_uvs := make([dynamic]TriangleUV, len = 0, cap = max_verts * 128)

	log.info(size_of(Vec2f) * cap(all_vertices))
	log.info(size_of(Triangle) * cap(all_triangles))
	log.info(size_of(TriangleUV) * cap(all_uvs))

	defer delete(all_vertices)
	defer delete(all_triangles)
	defer delete(all_uvs)

	init_state(max_verts)
	defer deinit_state()

    bounding_rect: Rect = {
        pos  = {f32(ttf_info.x_min), f32(ttf_info.y_min)},
        size = {
            f32(ttf_info.x_max - ttf_info.x_min),
            f32(ttf_info.y_max - ttf_info.y_min),
        },
    }

    log.info(glyfs['\"'])

	glyf_loop: for _, idx in glyfs {
        log.info("at", idx)
		defer reset_state()
		// if idx == 81 do continue
		glyf := glyfs[idx]
		glyf_info[idx].vertex_offset = u32(len(all_vertices))
		glyf_info[idx].triangle_offset = u32(len(all_triangles))

        coords: #soa[]ttf.Coord
        end_contours_be: []u16be

		switch v in glyf.value {
		case ttf.CompoundGlyf:
            end_contours_be = v.end_pts_of_contours
            coords = v.coords
		case ttf.SimpleGlyf:
            end_contours_be = v.end_pts_of_contours
            coords = v.coords
		}

		end_contours: []u16
		defer delete(end_contours)
		{
			end_contours = make([]u16, len(end_contours_be))
			for &c, idx in end_contours {
				c = cast(u16)end_contours_be[idx]
			}
		}

		for coord, i in coords {
			v: Vec2f = {f32(coord.x), f32(coord.y)}
			append(&all_vertices, v)
		}

		{
            if idx == 34 {
                log.info(coords)
                log.info(end_contours)
            }
			x, y, on_curve := soa_unzip(coords)
			triangulate_vertices(
				soa_zip(x = x, y = y),
				end_contours,
				on_curve,
				&all_triangles,
				&all_uvs,
			)
		}

		glyf_info[idx].triangle_count =
			u32(len(all_triangles)) - glyf_info[idx].triangle_offset
	}

    log.info(ttf_info)
	char := 'H'
	for tri in all_triangles[glyf_info[char].triangle_offset:][:glyf_info[char].triangle_count] {
		log.info("{")
		log.info(all_vertices[tri[0] + glyf_info[char].vertex_offset] / bounding_rect.size.y)
		log.info(all_vertices[tri[1] + glyf_info[char].vertex_offset] / bounding_rect.size.y)
		log.info(all_vertices[tri[2] + glyf_info[char].vertex_offset] / bounding_rect.size.y)
		log.info("}")

	}

	{
		SBOs: [3]u32
		gl.GenBuffers(3, raw_data(&SBOs))
		glyf_sbos.vertex_sbo = SBOs[0]
		glyf_sbos.triangle_sbo = SBOs[1]
		glyf_sbos.uvs_sbo = SBOs[2]
	}

	{
		set_sbo_data(glyf_sbos.vertex_sbo, all_vertices[:])
		set_sbo_data(glyf_sbos.triangle_sbo, all_triangles[:])
		set_sbo_data(glyf_sbos.uvs_sbo, all_uvs[:])
	}

	{
		gl.BindBufferBase(gl.SHADER_STORAGE_BUFFER, 0, glyf_sbos.vertex_sbo)
		gl.BindBufferBase(gl.SHADER_STORAGE_BUFFER, 1, glyf_sbos.triangle_sbo)
		gl.BindBufferBase(gl.SHADER_STORAGE_BUFFER, 2, glyf_sbos.uvs_sbo)
	}

	for glyf in glyfs {
		#partial switch v in glyf.value {
		case ttf.SimpleGlyf:
			delete(v.coords)
			delete(v.end_pts_of_contours)
		}
	}

	return
}

set_sbo_data :: proc(sbo: u32, data: []$T, usage: u32 = gl.STATIC_DRAW) {
	gl.BindBuffer(gl.SHADER_STORAGE_BUFFER, sbo)
	gl.BufferData(
		gl.SHADER_STORAGE_BUFFER,
		size_of(T) * len(data),
		raw_data(data),
		usage,
	)
}
