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

point_inside_triangle :: proc(t1, t2, t3, p: ttf.Coord) -> bool {
	a: DelVert = {t1.x, t1.y}
	b: DelVert = {t2.x, t2.y}
	c: DelVert = {t3.x, t3.y}
	x: DelVert = {p.x, p.y}

	right_ab := right_of(x, a, b)
	right_bc := right_of(x, b, c)
	right_ca := right_of(x, c, a)

	return right_ab == right_bc && right_ab == right_ca
}

midpoint :: proc(a, b: ttf.Coord, on_curve: bool) -> ttf.Coord {
	return {on_curve = on_curve, x = (a.x + b.x) / 2, y = (a.y + b.y) / 2}
}

restructure_overlapping_curves :: proc(
	coords: #soa[]ttf.Coord,
	end_contours: []u16,
) -> (
	new_coords: #soa[dynamic]ttf.Coord,
) {
	new_coords = make(#soa[dynamic]ttf.Coord)
	delta_end_contours := make(
		[]u16,
		len(end_contours),
		context.temp_allocator,
	)

	c_start: u16 = 0
	for c_end, contour_idx in end_contours {
		defer c_start = c_end + 1

		for i := c_start; i <= c_end; i += 1 {
			a := coords[i]
			append(&new_coords, a)

			if !a.on_curve do continue
			next := get_next_vert(c_start, c_end, i)
			if coords[next].on_curve do continue

			// i is b0 (bezier point 0)
			b := coords[next]
			next_next := get_next_vert(c_start, c_end, next)
			c := coords[next_next]

			if collinear(
				DelVert{a.x, a.y},
				DelVert{b.x, b.y},
				DelVert{c.x, c.y},
			) {
				continue
			} 

			for possible_overlap, j in coords {
				if u16(j) == i ||
				   u16(j) == next ||
				   u16(j) == next_next {continue}

				if !possible_overlap.on_curve &&
				   point_inside_triangle(a, b, c, possible_overlap) {
					m_ab := midpoint(a, b, false)
					m_bc := midpoint(b, c, false)
					new_anchor := midpoint(m_ab, m_bc, true)

					append(&new_coords, m_ab)
					append(&new_coords, new_anchor)
					append(&new_coords, m_bc)

					delta_end_contours[contour_idx] += 2
					log.info("overlap at", i, next, next_next, j)
					i += 1 // skip the off curve point

					break
				}
			}
		}
	}

	for &c, i in end_contours do c += delta_end_contours[i]

	log.info(end_contours)
	log.info(new_coords[:])

	return
}

load_glyphs :: proc(
	glyfs: ttf.ASCII_Glyfs,
	ttf_info: ttf.TTFInfo,
) -> (
	glyf_sbos: GlyfSBOs,
	glyf_info: [128]GlyfInfo,
) {
	// contour vertices + chordial axis vertices + bounding box vertices
	max_verts := ttf_info.max_verts + 2 * ttf_info.max_verts + 4

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

	glyf_loop: for _, idx in glyfs {
		log.info("at", idx)
		defer reset_state()
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

		new_coords := restructure_overlapping_curves(coords, end_contours)
		defer delete(new_coords)
		// insert_coords(coords, &all_vertices, end_contours, on_curve)

		for coord, i in new_coords[:] {
			v: Vec2f = {f32(coord.x), f32(coord.y)}
			append(&all_vertices, v)
		}

		{
			x, y, on_curve := soa_unzip(new_coords[:])
			context.logger.lowest_level = idx == '%' ? .Debug : .Warning
			triangulate_vertices(
				soa_zip(x = x, y = y),
				end_contours,
				on_curve,
				&all_vertices,
				&all_triangles,
				&all_uvs,
			)
			context.logger.lowest_level = .Debug
		}


		glyf_info[idx].triangle_count =
			u32(len(all_triangles)) - glyf_info[idx].triangle_offset
	}

	/*
    log.info(ttf_info)
	char := 'H'
	for tri in all_triangles[glyf_info[char].triangle_offset:][:glyf_info[char].triangle_count] {
		log.info("{")
		log.info(all_vertices[tri[0] + glyf_info[char].vertex_offset] / bounding_rect.size.y)
		log.info(all_vertices[tri[1] + glyf_info[char].vertex_offset] / bounding_rect.size.y)
		log.info(all_vertices[tri[2] + glyf_info[char].vertex_offset] / bounding_rect.size.y)
		log.info("}")

	}
    */

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
