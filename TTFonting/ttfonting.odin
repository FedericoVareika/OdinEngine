package ttfonting

import "core:fmt"
import "core:mem"
import "core:os"
import "core:strings"

// https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6glyf.html

@(require_results)
value_from_data :: #force_inline proc(data: []u8, idx: int, $T: typeid) -> T {
	return (^T)(&data[idx])^
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
	directories:     []TableDirectory,
    
    tables: #sparse [TableTag]Table,
}

parse_ttf :: proc(path: string) {
	data, ok := os.read_entire_file("../assets/fonts/Anonymous.ttf")
	if !ok do panic("couldnt read file")
	defer delete(data)

	state.offset_subtable = value_from_data(data, 0, OffsetSubtable)
	// state.offset_subtable = (^OffsetSubtable)(&data[0])^

	fmt.println(state.offset_subtable)

	state.directories = slice_from_data(
		data,
		size_of(OffsetSubtable),
		int(state.offset_subtable.num_tables),
		TableDirectory,
	)
	// state.directories = mem.slice_ptr(
	// 	(^TableDirectory)(&data[size_of(OffsetSubtable)]),
	// 	int(state.offset_subtable.num_tables),
	// )

	for &dir, i in state.directories {
		tag := strings.string_from_ptr(&(dir.tag[0]), 4)
		tag_enum := table_tag(tag)
		if tag_enum == .none do continue
		fmt.println("-----------------", tag, "-----------------")
		table_start := data[dir.offset:]
		table_u32 := transmute([]u32be)table_start

		if tag_enum != .head {
			cs := calc_checksum(table_u32, u32(dir.length))
			assert(dir.checksum == cs)
		}

		#partial switch tag_enum {
		case .cmap:
			state.tables[.cmap] = parse_cmap(data, dir)
        case .glyf:
            break
		}
	}
}

parse_cmap :: proc(
	data: []u8,
	cmap_dir: TableDirectory,
) -> (
	cmap: CmapFormat4,
) {
	index: CmapIndex = (^CmapIndex)(&data[cmap_dir.offset])^
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
	cmap.prefix = value_from_data(cmap_data, 0, type_of(cmap.prefix))

	assert(cmap.prefix.format == 4)

	cmap.header = value_from_data(
		cmap_data,
		size_of(cmap.prefix),
		type_of(cmap.header),
	)

    fmt.println(cmap.prefix)
    fmt.println(cmap.header)

	seg_count := int(cmap.header.seg_count_x2 >> 1) // seg_count_x2 / 2
	when ODIN_TEST {
		assert(seg_count == int(cmap.header.seg_count_x2 / 2))
	}

	{
		cmap_segments := cmap_data[size_of(cmap.prefix) +
		size_of(cmap.header):]

		end_codes := slice_from_data(cmap_segments, 0, seg_count, u16be)
		cmap_segments = cmap_segments[size_of(u16be) * seg_count:]

		// checking reserved padding
		assert(value_from_data(cmap_segments, 0, u16be) == 0)
		cmap_segments = cmap_segments[size_of(u16be):]

		start_codes := slice_from_data(cmap_segments, 0, seg_count, u16be)
		cmap_segments = cmap_segments[size_of(u16be) * seg_count:]

		id_deltas := slice_from_data(cmap_segments, 0, seg_count, u16be)
		cmap_segments = cmap_segments[size_of(u16be) * seg_count:]

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
		glyph_ids_len := int(cmap.prefix.length)
		glyph_ids_len -= len(cmap.segments) * size_of(cmap.segments[0])
		glyph_ids_len -= 1 * size_of(u16be)
		glyph_ids_len -= size_of(cmap.header)
		glyph_ids_len -= size_of(cmap.prefix)

		cmap.glyph_ids = slice_from_data(
			cmap_data[int(cmap.prefix.length) - glyph_ids_len:],
			0,
			glyph_ids_len / size_of(u16be),
            u16be,
		)
	}

    return 
}

parse_glyf :: proc() {

}


