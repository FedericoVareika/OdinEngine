package ttfonting

import "core:fmt"
import "core:log"
import "core:mem"
import "core:os"
import "core:strings"

// https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6glyf.html

@(require_results)
value_from_data :: #force_inline proc(data: []u8, idx: int, $T: typeid) -> T {
	return (^T)(&data[idx])^
}

@(require_results)
value_from_data_offset :: #force_inline proc(
	data: ^[]u8,
	idx: int,
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
	offset_subtable: OffsetSubtable,
	tables:          #sparse[TableTag]Table,
}

parse_ttf :: proc(path: string, idx: int) -> Glyf {
	data, ok := os.read_entire_file(path)
	if !ok do panic("couldnt read file")
	defer delete(data)

	state.offset_subtable = value_from_data(data, 0, OffsetSubtable)

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

	fmt.println("----------------- cmap -----------------")
	state.tables[.cmap] = parse_cmap(data, directories[.cmap])
	fmt.println("here")
	fmt.println("----------------- head -----------------")
	state.tables[.head] = parse_head(data, directories[.head])
	fmt.println("----------------- maxp -----------------")
	state.tables[.maxp] = parse_maxp(data, directories[.maxp])
	fmt.println("----------------- loca -----------------")
	state.tables[.loca] = parse_loca(data, directories[.loca])

	fmt.println("head size", size_of(Head))

	glyf_offset := directories[.glyf].offset
	log.info(glyf_offset)
	switch v in state.tables[.loca].(Loca).offsets {
	case []u32be:
		glyf_offset += v[idx]
		log.info(
			"start:",
			v[idx],
			"end:",
			v[idx + 1],
			"len: ",
			v[idx + 1] - v[idx],
		)
	case []u16be:
		glyf_offset += 2 * u32be(v[idx])
		log.info(
			"start:",
			v[idx] * 2,
			"end:",
			v[idx + 1] * 2,
			"len: ",
			(v[idx + 1] - v[idx]) * 2,
		)
	}

	log.info(glyf_offset)

	// fmt.println("------------")
	glyf := parse_glyf(data[glyf_offset:])
	// fmt.println(glyf)
	for coord, idx in glyf.value.(SimpleGlyf).coords {
		log.infof("[%d], %d,%d", idx, coord.x, coord.y)
	}
	// for coord in glyf.value.(SimpleGlyf).coords {
	// 	fmt.println(coord.y)
	// }
	// fmt.println("------------")
	// {
	// 	glyf := parse_glyf(data[directories[.glyf].offset:])
	// 	for coord in glyf.value.(SimpleGlyf).coords {
	// 		fmt.println(coord.x)
	// 	}
	// 	for coord in glyf.value.(SimpleGlyf).coords {
	// 		fmt.println(coord.y)
	// 	}
	// }
	// fmt.println("------------")
	return glyf
}

parse_cmap :: proc(
	data: []u8,
	cmap_dir: TableDirectory,
) -> (
	cmap: CmapFormat4,
) {
	index := value_from_data(data, int(cmap_dir.offset), CmapIndex)
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
	}

	{
		segments_len := len(cmap.segments) * size_of(cmap.segments[0])
		segments_len += 1 * size_of(u16be)

		glyph_ids_len := int(cmap.prefix.length)
		glyph_ids_len -= len(cmap.segments) * size_of(cmap.segments[0])
		glyph_ids_len -= 1 * size_of(u16be)
		glyph_ids_len -= size_of(cmap.header)
		glyph_ids_len -= size_of(cmap.prefix)

		cmap.glyph_ids = slice_from_data(
			cmap_data,
			segments_len,
			glyph_ids_len / size_of(u16be),
			u16be,
		)
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
	glyf.description = value_from_data(glyf_data, 0, GlyfDescription)

	if (glyf.description.n_contours < 0) {
		glyf.value = parse_compound_glyf(glyf_data, glyf.description)
	} else if (glyf.description.n_contours > 0) {
		log.info("n contours", glyf.description.n_contours)
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

	simple_glyf_data := glyf_data[size_of(GlyfDescription):]
	total_bytes += size_of(GlyfDescription)


	simple_glyf.end_pts_of_contours, bytes = slice_from_data_offset(
		&simple_glyf_data,
		0,
		int(glyf_desc.n_contours),
		u16be,
	)

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

	n_points := int(
		simple_glyf.end_pts_of_contours[len(simple_glyf.end_pts_of_contours) - 1] +
		1,
	)
	fmt.println("n_points", n_points)
	flags := make([]OutlineFlags, n_points, context.temp_allocator)

	n_flags := 0
	{
		i := 0
		count := 0
		repeat := false
		for count < n_points {
			defer i += 1
			if !repeat {
				flags[count] = value_from_data(simple_glyf_data, i, OutlineFlags)
				log.info("[", count, "]", simple_glyf_data[i], flags[i])
				if .repeat in flags[count] do repeat = true
				count += 1
				continue
			}

			repeat_size := int(value_from_data(simple_glyf_data, i, u8))
			log.info("repeat_size", repeat_size)
			for j in 0 ..< repeat_size {
				log.info(
					"[",
					i + j,
					"]",
					simple_glyf_data[i - 1],
					flags[i - 1],
				)
				flags[i + j] = flags[i - 1]
			}
			count += repeat_size
			repeat = false
		}
		assert(count == n_points)
		n_flags = i
	}

	log.info("n_flags", n_flags)
	simple_glyf_data = simple_glyf_data[n_flags:]
	total_bytes += n_flags

	simple_glyf.coords = make(#soa[]Coord, n_points)

	x, y, on_curve := soa_unzip(simple_glyf.coords)

	for &c, i in on_curve {
		assert(.zero1 not_in flags[i])
		assert(.zero2 not_in flags[i])
		if .on_curve in flags[i] do c = true
		else do c = false
	}

	simple_glyf_data, bytes = parse_coords(
		&x,
		simple_glyf_data,
		flags,
		.x_short_vec,
		.same_x_or_positive_short,
	)
	total_bytes += bytes

	simple_glyf_data, bytes = parse_coords(
		&y,
		simple_glyf_data,
		flags,
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
	for &v, i in coords^ {
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

		v = prev + offset
		prev = v
	}

	return
}

parse_compound_glyf :: proc(
	glyf_data: []u8,
	glyf_desc: GlyfDescription,
) -> (
	compound_glyf: CompoundGlyf,
) {

	return
}
