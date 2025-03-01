package ttfonting

import "core:fmt"
import "core:log"
import "core:mem"
import "core:os"
import "core:strings"

// https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6glyf.html

@(require_results)
value_from_data :: #force_inline proc(
	data: []u8,
	#any_int idx: int,
	$T: typeid,
) -> T {
	return (^T)(&data[idx])^
}

@(require_results)
value_from_data_offset :: #force_inline proc(
	data: ^[]u8,
	#any_int idx: int,
	$T: typeid,
) -> (
	res: T,
	n_bytes: int,
) {
	res = value_from_data(data^, idx, T)
	n_bytes = idx + size_of(T)
	data^ = data^[n_bytes:]
	return
}

@(require_results)
slice_from_data :: #force_inline proc(
	data: []u8,
	idx, len: int,
	$T: typeid,
) -> []T {

	return mem.slice_ptr((^T)(&data[idx]), len)
}

@(require_results)
slice_from_data_offset :: #force_inline proc(
	data: ^[]u8,
	idx, len: int,
	$T: typeid,
) -> (
	res: []T,
	n_bytes: int,
) {
	res = slice_from_data(data^, idx, len, T)
	n_bytes = idx + size_of(T) * len
	data^ = data^[n_bytes:]
	return
}

@(require_results)
table_tag :: proc(tag: string) -> TableTag {
	switch tag {
	case "cmap":
		return .cmap
	case "glyf":
		return .glyf
	case "head":
		return .head
	case "hhea":
		return .hhea
	case "hmtx":
		return .hmtx
	case "loca":
		return .loca
	case "maxp":
		return .maxp
	case "name":
		return .name
	case "post":
		return .post
	case "cvt_":
		return .cvt_
	case "fpgm":
		return .fpgm
	case "hdmx":
		return .hdmx
	case "kern":
		return .kern
	case "OS_2":
		return .OS_2
	case "prep":
		return .prep
	case:
		return .none
	}
}

@(require_results)
calc_checksum :: proc(data: []u32be, n_bytes: u32) -> (sum: u32be = 0) {
	n_longs := (n_bytes + 3) / 4
	for i in 0 ..< n_longs {
		sum += data[i]
	}
	return
}

state: struct {
	raw_data:        []u8,
	offset_subtable: OffsetSubtable,
	directories:     map[TableTag]TableDirectory,
	tables:          #sparse[TableTag]Table,
	max_n_vertices:  u32,
}

TTFInfo :: struct {
	max_verts:                  u32,
	x_min, y_min, x_max, y_max: FWord,
}

parse_ttf :: proc(
	path: string,
) -> (
	result: ASCII_Glyfs,
	metrics: ASCII_Metrics,
	info: TTFInfo,
) {
	// parse_ttf :: proc(path: string, char: u8) -> Glyf {
	data, ok := os.read_entire_file(path)
	if !ok do panic("couldnt read file")
	defer delete(data)

	state.offset_subtable = value_from_data(data, 0, OffsetSubtable)
	state.raw_data = data

	fmt.println(state.offset_subtable)

	raw_directories := slice_from_data(
		data,
		size_of(OffsetSubtable),
		int(state.offset_subtable.num_tables),
		TableDirectory,
	)

	directories := make(map[TableTag]TableDirectory)
	defer delete(directories)

	for &dir, i in raw_directories {
		tag := strings.string_from_ptr(&(dir.tag[0]), 4)
		tag_enum := table_tag(tag)
		if tag_enum == .none do continue
		table_start := data[dir.offset:]
		table_u32 := transmute([]u32be)table_start

		if tag_enum != .head {
			cs := calc_checksum(table_u32, u32(dir.length))
			assert(dir.checksum == cs)
		}

		directories[tag_enum] = dir
	}

    state.directories = directories

	fmt.println("----------------- cmap -----------------")
	state.tables[.cmap] = parse_cmap(data, directories[.cmap])
	fmt.println("----------------- head -----------------")
	state.tables[.head] = parse_head(data, directories[.head])
	fmt.println("----------------- maxp -----------------")
	state.tables[.maxp] = parse_maxp(data, directories[.maxp])
	fmt.println("----------------- loca -----------------")
	state.tables[.loca] = parse_loca(data, directories[.loca])
	fmt.println("----------------- hhea -----------------")
	state.tables[.hhea] = parse_hhea(data, directories[.hhea])

	global_glyf_offset := u32(directories[.glyf].offset)
	local_glyf_locations: [128]u32

	cmap4 := state.tables[.cmap].(CmapFormat4)
	loca := state.tables[.loca].(Loca)
	// log.info(cmap4.glyph_index_array)

    log.info("':", get_glyf_offset('\''))

	for &glyf, idx in result { 	// assume that ascii chars are simple glyfs
		glyf_offset, loca_size := get_glyf_offset(idx)
		log.infof(
			"parsing glyf %d at %d, table length: %d",
			u8(idx),
			glyf_offset,
			loca_size,
		)
		if loca_size > 0 {
			context.logger.lowest_level = .Warning
			glyf = parse_glyf(data[glyf_offset:])
			context.logger.lowest_level = .Debug
		} else {
			glyf = {}
			glyf.value = SimpleGlyf({})
		}
	}

	delete(cmap4.glyph_index_array)

	info.max_verts = state.max_n_vertices
	{
		head := state.tables[.head].(Head)
		info.x_min = head.x_min
		info.y_min = head.y_min
		info.x_max = head.x_max
		info.y_max = head.y_max
	}

	parse_hmtx(data, directories[.hmtx], &metrics)

	return
}

get_glyf_offset :: proc(unicode_idx: int) -> (glyf_offset: u32, loca_size: u32) {
	cmap4 := state.tables[.cmap].(CmapFormat4)
	loca := state.tables[.loca].(Loca)
	mapped_idx := cmap4.glyph_index_array[unicode_idx]
    log.info(mapped_idx)
	loca_offset: u32
	switch v in loca.offsets {
	case []u32be:
		loca_offset = u32(v[mapped_idx])
		loca_size = u32(v[mapped_idx + 1] - v[mapped_idx])
	case []u16be:
		loca_offset = u32(v[mapped_idx])
		loca_size = u32(v[mapped_idx + 1] - v[mapped_idx])
	}
	glyf_offset = u32(state.directories[.glyf].offset) + loca_offset
	return
}

parse_cmap :: proc(
	data: []u8,
	cmap_dir: TableDirectory,
) -> (
	cmap: CmapFormat4,
) {
	index := value_from_data(data, cmap_dir.offset, CmapIndex)
	// index: CmapIndex = (^CmapIndex)(&data[cmap_dir.offset])^
	fmt.println(index)

	subtables := slice_from_data(
		data,
		int(cmap_dir.offset) + size_of(CmapIndex),
		int(index.number_subtables),
		CmapSubtable,
	)

	usable_subtable: CmapSubtable = {}
	for subtable in subtables {
		if subtable.platform_id == .macintosh do continue
		usable_subtable = subtable
		break
	}

	fmt.println(usable_subtable)
	cmap_data := data[cmap_dir.offset + usable_subtable.offset:]
	cmap.prefix, _ = value_from_data_offset(
		&cmap_data,
		0,
		type_of(cmap.prefix),
	)

	assert(cmap.prefix.format == 4)

	cmap.header, _ = value_from_data_offset(
		&cmap_data,
		0,
		type_of(cmap.header),
	)

	fmt.println(cmap.prefix)
	fmt.println(cmap.header)

	seg_count := int(cmap.header.seg_count_x2 >> 1) // seg_count_x2 / 2
	when ODIN_TEST {
		assert(seg_count == int(cmap.header.seg_count_x2 / 2))
	}

	{
		cmap_segments := cmap_data

		end_codes, _ := slice_from_data_offset(
			&cmap_segments,
			0,
			seg_count,
			u16be,
		)

		// checking reserved padding
		padding, _ := value_from_data_offset(&cmap_segments, 0, u16be)
		assert(padding == 0)

		start_codes, _ := slice_from_data_offset(
			&cmap_segments,
			0,
			seg_count,
			u16be,
		)

		id_deltas, _ := slice_from_data_offset(
			&cmap_segments,
			0,
			seg_count,
			u16be,
		)

		id_range_offsets := slice_from_data(cmap_segments, 0, seg_count, u16be)

		cmap.segments = soa_zip(
			end_codes,
			start_codes,
			id_deltas,
			id_range_offsets,
		)

		when ODIN_TEST {
			assert(cmap.segments[len(cmap.segments) - 1].end_code == 0xFFFF)
		}

		/*
        if the segment.id_range_offset == 0 then: 
            the index of char c to glyph table is c - segment.id_delta

        if the id_range_offset is not 0 then: 

        [n]-----------n bytes-----------[_]----(c - start_code)----[idx]
         ^           n/2 u16be           ^           u16be           ^
   id_range_offset                   start_code                  idx glyph 
                                        idx                        table

         */
		glyph_index_dyn := make([dynamic]u16be)
		{
			for segment in cmap.segments {
				defer cmap_segments = cmap_segments[2:]

				// in bytes
				offset_to_start_code_idx := segment.id_range_offset

				for c in segment.start_code ..= segment.end_code {
					if segment.id_range_offset == 0 {
						assign_at(
							&glyph_index_dyn,
							c,
							u16be(u32(c + segment.id_delta) % 0xFFFF),
						)
						continue
					}

					// in bytes
					offset_to_idx :=
						offset_to_start_code_idx + 2 * (c - segment.start_code)
					assign_at(
						&glyph_index_dyn,
						c,
						value_from_data(cmap_segments, offset_to_idx, u16be),
					)
				}
			}
		}
		shrink(&glyph_index_dyn)
		log.warn(len(glyph_index_dyn))
		cmap.glyph_index_array = glyph_index_dyn[:]
	}

	return
}

parse_head :: proc(data: []u8, dir: TableDirectory) -> (head: Head) {
	head_data := data[dir.offset:]
	head = value_from_data(head_data, 0, Head)
	log.info(head)
	assert(head.magic_number == 0x5F0F3CF5)

	return
}

parse_maxp :: proc(data: []u8, dir: TableDirectory) -> (maxp: Maxp) {
	maxp_data := data[dir.offset:]
	maxp = value_from_data(maxp_data, 0, Maxp)
	fmt.println(maxp)
	return
}

parse_loca :: proc(data: []u8, dir: TableDirectory) -> (loca: Loca) {
	loca_data := data[dir.offset:]

	n := int(state.tables[.maxp].(Maxp).num_glyfs)
	if state.tables[.head].(Head).index_to_loc_fmt == .long_offsets {
		loca.offsets = slice_from_data(loca_data, 0, n, u32be)
	} else {
		loca.offsets = slice_from_data(loca_data, 0, n, u16be)
	}

	// fmt.println(loca.offsets)
	switch v in loca.offsets {
	case []u32be:
		log.info("len", len(v))
		log.info("all", v)
	case []u16be:
		log.info("len", len(v))
		log.info("all", v)
	}

	return
}

parse_glyfs :: proc(data: []u8, dir: TableDirectory) -> (glyfs: []Glyf) {

	return
}

parse_glyf :: proc(glyf_data: []u8) -> (glyf: Glyf) {
	glyf_data := glyf_data
	glyf.description, _ = value_from_data_offset(
		&glyf_data,
		0,
		GlyfDescription,
	)

	if (glyf.description.n_contours < 0) {
		glyf.value = parse_compound_glyf(glyf_data, glyf.description)
		// glyf.value = CompoundGlyf({})
	} else if (glyf.description.n_contours > 0) {
		glyf.value = parse_simple_glyf(glyf_data, glyf.description)
	} else {
		glyf.value = SimpleGlyf({})
	}

	return
}

parse_simple_glyf :: proc(
	glyf_data: []u8,
	glyf_desc: GlyfDescription,
) -> (
	simple_glyf: SimpleGlyf,
) {
	total_bytes := 0
	bytes := 0

	// simple_glyf_data := glyf_data[size_of(GlyfDescription):]
	simple_glyf_data := glyf_data
	total_bytes += size_of(GlyfDescription)

	end_pts_of_contours: []u16be
	end_pts_of_contours, bytes = slice_from_data_offset(
		&simple_glyf_data,
		0,
		int(glyf_desc.n_contours),
		u16be,
	)
	simple_glyf.end_pts_of_contours = make([]u16be, len(end_pts_of_contours))
	for &e, i in simple_glyf.end_pts_of_contours {
		e = end_pts_of_contours[i]
	}

	total_bytes += bytes

	simple_glyf.instruction_len, bytes = value_from_data_offset(
		&simple_glyf_data,
		0,
		u16be,
	)
	total_bytes += bytes

	simple_glyf.instructions, bytes = slice_from_data_offset(
		&simple_glyf_data,
		0,
		int(simple_glyf.instruction_len),
		u8,
	)
	total_bytes += bytes
	// copy_slice(simple_glyf.instructions, simple_glyf.instructions)

	n_points := int(
		simple_glyf.end_pts_of_contours[len(simple_glyf.end_pts_of_contours) - 1] +
		1,
	)

    simple_glyf.real_indices = make([]u16, n_points)

	log.info("n_points", n_points)
	flags := make([dynamic]OutlineFlags)
	defer delete(flags)

	// Read flags, append midpoints if need be, and adjust the 
	// end_pts_of_contours
	log.info(simple_glyf.end_pts_of_contours)
	n_flags := 0
	{
		current_contour := 0
		contour_offset: u16be = 0

		flags_read := 0

		flag_to_repeat: OutlineFlags
		read_repeat := false
		repeat: u8 = 0

		prev_off_curve := false

		for flags_read < n_points {
			log.info(flags_read, len(flags))
			if len(flags) >
			   int(simple_glyf.end_pts_of_contours[current_contour]) {
				current_contour += 1
				simple_glyf.end_pts_of_contours[current_contour] +=
					contour_offset
				log.info(simple_glyf.end_pts_of_contours)
			}

			if read_repeat {
				log.info("reading repeat")
				repeat, _ = value_from_data_offset(&simple_glyf_data, 0, u8)
				read_repeat = false
				// flags_read += 1
				continue
			}

			flag: OutlineFlags
			defer append(&flags, flag)
			if repeat > 0 {
				flag = flag_to_repeat
				repeat -= 1
			} else {
				flag, _ = value_from_data_offset(
					&simple_glyf_data,
					0,
					OutlineFlags,
				)
				flag -= {.midpoint, .zero2}
				if .repeat in flag {
					flag_to_repeat = flag
					read_repeat = true
				}
			}

            simple_glyf.real_indices[flags_read] = u16(len(flags))
            flags_read += 1

			if prev_off_curve && .on_curve not_in flag {
				midpoint_flag := flag + {.midpoint, .on_curve}
				append(&flags, midpoint_flag)
				simple_glyf.end_pts_of_contours[current_contour] += 1
				contour_offset += 1
			}

			prev_off_curve = .on_curve not_in flag
		}
		n_flags = flags_read
	}

    log.warn(simple_glyf.real_indices)


	log.info("contours", simple_glyf.end_pts_of_contours)
	log.info("n_flags", n_flags)
	// simple_glyf_data = simple_glyf_data[n_flags:]
	total_bytes += n_flags

	simple_glyf.coords = make(#soa[]Coord, len(flags))
	state.max_n_vertices = max(state.max_n_vertices, u32(len(flags)))

	x, y, on_curve := soa_unzip(simple_glyf.coords)

	for &c, i in on_curve {
		assert(.zero2 not_in flags[i])
		if .on_curve in flags[i] do c = true
		else do c = false
	}

	simple_glyf_data, bytes = parse_coords(
		&x,
		simple_glyf_data,
		flags[:],
		.x_short_vec,
		.same_x_or_positive_short,
	)
	total_bytes += bytes

	simple_glyf_data, bytes = parse_coords(
		&y,
		simple_glyf_data,
		flags[:],
		.y_short_vec,
		.same_y_or_positive_short,
	)
	total_bytes += bytes


	log.info("total bytes:", total_bytes)


	return

	//simple_glyf.coords[i].on_curve = .on_curve in flag
}

parse_coords :: proc(
	coords: ^[]i16,
	data: []u8,
	flags: []OutlineFlags,
	v_short, same_v_or_pos_short: OutlineFlag,
) -> (
	offset_data: []u8,
	total_bytes: int,
) {
	offset_data = data

	bytes := 0
	offset_u8: u8 = 0
	offset_i16be: i16be = 0

	prev: i16 = 0

	prev_was_midpoint := false
	midpoint_value: i16 = 0

	for &v, i in coords {
		if prev_was_midpoint {
			assert(.midpoint not_in flags[i])
			prev_was_midpoint = false
			v = midpoint_value
			prev = v
			midpoint_value = 0
			continue
		}

		bytes = 0
		defer total_bytes += bytes

		offset: i16 = 0

		if v_short in flags[i] {
			offset_u8, bytes = value_from_data_offset(&offset_data, 0, u8)
			offset = i16(offset_u8)
			if same_v_or_pos_short not_in flags[i] {
				offset *= -1
			}
		} else if same_v_or_pos_short in flags[i] {
			bytes = 0
			offset = 0
		} else {
			offset_i16be, bytes = value_from_data_offset(
				&offset_data,
				0,
				i16be,
			)
			offset = i16(offset_i16be)
		}

		log.info("offset[", i, "]:", offset)
		log.info("bytes[", i, "]:", bytes)

		if .midpoint in flags[i] {
			midpoint_value = prev + offset
			offset /= 2
			prev_was_midpoint = true
		} else {
			prev_was_midpoint = false
		}

		v = prev + offset
		prev = v
	}

	return
}

parse_component :: proc(
    glyf_data: []u8,
    coords: ^#soa[dynamic]Coord,
    end_pts: ^[dynamic]u16be,
    real_indices: ^[dynamic]u16,
    last_end_pt: ^u16be,
    last_real_index: ^u16,
    component: ComponentGlyf,
) -> []u8 {
    glyf_data := glyf_data
    // parse glyf component
    comp_glyf_loc: u32
    switch v in state.tables[.loca].(Loca).offsets {
    case []u32be: 
        comp_glyf_loc = u32(v[component.glyph_idx])
    case []u16be: 
        comp_glyf_loc = u32(v[component.glyph_idx])
    }
    glyf_offset := u32(state.directories[.glyf].offset) + comp_glyf_loc

    log.warn("idx", component.glyph_idx)
    log.warn("loc", glyf_offset)
    component_glyf := parse_glyf(state.raw_data[glyf_offset:])  

    coords_comp := component_glyf.value.(SimpleGlyf).coords
    end_pts_comp := component_glyf.value.(SimpleGlyf).end_pts_of_contours
    real_indices_comp := component_glyf.value.(SimpleGlyf).real_indices

    defer {
        delete(coords_comp)
        delete(end_pts_comp)
        delete(real_indices_comp)
    }

    arg1, arg2: i32
    if .args_are_xy_values in component.flags {
        if .args_are_words in component.flags {
            arg1 = i32(value_from_data(glyf_data, 0, i16be))
            arg2 = i32(value_from_data(glyf_data, size_of(i16be), i16be))
            glyf_data = glyf_data[2 * size_of(i16be):]
        } else {
            arg1 = i32(value_from_data(glyf_data, 0, i8))
            arg2 = i32(value_from_data(glyf_data, size_of(i8), i8))
            glyf_data = glyf_data[2 * size_of(i8):]
        }
    } else {
        if .args_are_words in component.flags {
            arg1 = i32(value_from_data(glyf_data, 0, u16be))
            arg2 = i32(value_from_data(glyf_data, size_of(u16be), u16be))
            glyf_data = glyf_data[2 * size_of(u16be):]
        } else {
            arg1 = i32(value_from_data(glyf_data, 0, u8))
            arg2 = i32(value_from_data(glyf_data, size_of(u8), u8))
            glyf_data = glyf_data[2 * size_of(u8):]
        }
    }

    // assert(.args_are_xy_values in component.flags)

    a, b, c, d, e, f: f64

    if .args_are_xy_values in component.flags {
        e = f64(arg1)
        f = f64(arg2)
    } else {
        log.warn(arg1, arg2)
        log.warn(real_indices[:len(real_indices)], real_indices_comp)
        pt1 := coords[real_indices[arg1]]
        pt2 := coords_comp[real_indices_comp[arg2]]

        offset := [2]i16{pt1.x - pt2.x, pt1.y - pt2.y}
        e = f64(offset.x)
        f = f64(offset.y)
    }

    u16be_to_f2dot14 :: proc(a: u16be) -> f64 {
        return f64(transmute(i16)a) / f64(1 << 14)
    }

    if .we_have_a_scale in component.flags {
        first_short, _ := value_from_data_offset(&glyf_data, 0, u16be)
        a = u16be_to_f2dot14(first_short)
        d = u16be_to_f2dot14(first_short)
    } else if .we_have_a_x_and_y_scale in component.flags {
        first_short, _ := value_from_data_offset(&glyf_data, 0, u16be)
        second_short, _ := value_from_data_offset(&glyf_data, 0, u16be)
        a = u16be_to_f2dot14(first_short)
        d = u16be_to_f2dot14(second_short)
    } else if .we_have_a_2_by_2 in component.flags {
        first_short, _ := value_from_data_offset(&glyf_data, 0, u16be)
        second_short, _ := value_from_data_offset(&glyf_data, 0, u16be)
        third_short, _ := value_from_data_offset(&glyf_data, 0, u16be)
        fourth_short, _ := value_from_data_offset(&glyf_data, 0, u16be)
        a = u16be_to_f2dot14(first_short)
        b = u16be_to_f2dot14(second_short)
        c = u16be_to_f2dot14(third_short)
        d = u16be_to_f2dot14(fourth_short)
    } else {
        a = 1 
        d = 1
    }

    m, n: f64
    m_0 := max(abs(a), abs(b))
    n_0 := max(abs(c), abs(d))

    if abs(abs(a) - abs(c)) <= 33 / 65536 do m = 2 * m_0
    else do m = m_0

    if abs(abs(b) - abs(d)) <= 33 / 65536 do n = 2 * n_0
    else do n = n_0

    transform_point :: proc(
        x, y: i16,
        a, b, c, d, e, f, m, n: f64,
    ) -> (
        new_x, new_y: i16,
    ) {
        x_prime := m * ((a / m) * f64(x) + (c / m) * f64(y) + e)
        y_prime := n * ((b / n) * f64(x) + (d / n) * f64(y) + f)

        log.warn(x_prime, y_prime)

        new_x = i16(x_prime)
        new_y = i16(y_prime)
        return
    }

    for coord in coords_comp {
        x := coord.x
        y := coord.y
        on_curve := coord.on_curve
        new_x, new_y := transform_point(x, y, a, b, c, d, e, f, m, n)
        log.warn()
        log.warn(coord)
        log.warn(a, b, c, d, e, f, m, n)
        log.warn(Coord{new_x, new_y, on_curve})
        append_soa(coords, Coord{new_x, new_y, on_curve})
    }

    for end_pt in end_pts_comp {
        append(end_pts, end_pt + last_end_pt^)
    } 

    for index in real_indices_comp {
        append(real_indices, index + last_real_index^)
    } 

    last_end_pt^ = end_pts[len(end_pts) - 1] 
    last_real_index^ = real_indices[len(real_indices) - 1]

    return glyf_data
}

parse_compound_glyf :: proc(
	glyf_data: []u8,
	glyf_desc: GlyfDescription,
) -> (
	compound_glyf: CompoundGlyf,
) {
	glyf_data := glyf_data
	// components := make([dynamic]ComponentGlyf)
	coords := make(#soa[dynamic]Coord)
	end_pts := make([dynamic]u16be)
    real_indices := make([dynamic]u16)

    last_end_pt : u16be
    last_real_index : u16
	for {
		component: ComponentGlyf = {}
		// defer append(&components, component)
		component.flags, _ = value_from_data_offset(
			&glyf_data,
			0,
			ComponentFlags,
		)
		component.glyph_idx, _ = value_from_data_offset(&glyf_data, 0, u16be)

        glyf_data = parse_component(
            glyf_data,
            &coords,
            &end_pts,
            &real_indices,
            &last_end_pt,
            &last_real_index,
            component,
        )

        if .more_components not_in component.flags do break
	}

    { /*
        xs, ys, on_curves := soa_unzip(coords)
        shrink(&xs)
        shrink(&ys)
        shrink(&on_curves)
        coords = soa_zip(x=xs, y=ys, on_curve=on_curves)

        shrink(&end_pts)
    */ }
    compound_glyf.coords = coords[:len(coords)]
    compound_glyf.end_pts_of_contours = end_pts[:len(end_pts)]
    compound_glyf.real_indices = real_indices[:len(real_indices)]

	return
}

parse_hhea :: proc(data: []u8, dir: TableDirectory) -> (hhea: Hhea) {
	hhea_data := data[dir.offset:]
	hhea = value_from_data(hhea_data, 0, Hhea)

	return
}

parse_hmtx :: proc(data: []u8, dir: TableDirectory, metrics: ^ASCII_Metrics) {
	hmtx_data := data[dir.offset:]
	last_adv_width: u16be = 0
	i := 0
	for i <
	    min(
		    int(state.tables[.hhea].(Hhea).num_long_hor_metrics),
		    len(metrics),
	    ) {
		defer i += 1
		metrics[i].hor_metric.adv_width, _ = value_from_data_offset(
			&hmtx_data,
			0,
			u16be,
		)
		last_adv_width = metrics[i].hor_metric.adv_width
		metrics[i].hor_metric.lsb, _ = value_from_data_offset(
			&hmtx_data,
			0,
			i16be,
		)
	}


	for i < len(metrics) {
		defer i += 1
		metrics[i].hor_metric.adv_width = last_adv_width
		metrics[i].hor_metric.lsb, _ = value_from_data_offset(
			&hmtx_data,
			0,
			i16be,
		)
	}
}
