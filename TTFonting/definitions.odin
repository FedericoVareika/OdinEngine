package ttfonting

ShortFrac :: bit_field i16 {
	// TODO: Revise this
	sign: bool | 1,
	frac: u16  | 15,
}
Fixed :: bit_field i32 {
	whole: i16 | 16,
	frac:  i16 | 16,
}
FWord :: i16
uFWord :: u16
F2Dot14 :: bit_field i16 {
	whole: i16 | 2,
	frac:  i16 | 14,
}
LongDateTime :: u64

TableTag :: enum {
	none = 0,
	// Required
	cmap,
	glyf,
	head,
	hhea,
	hmtx,
	loca,
	maxp,
	name,
	post,
	// Optional
	cvt_,
	fpgm,
	hdmx,
	kern,
	OS_2,
	prep,
}

OffsetSubtable :: struct {
	scaler_type:                                           u32be,
	num_tables, search_range, entry_selector, range_shift: u16be,
}

TableDirectory :: struct {
	tag:                      [4]u8,
	checksum, offset, length: u32be,
}

/*
-----------------------------
------------Tables-----------
-----------------------------
*/

Table :: union {
	CmapFormat4,
}

/*
------------Cmap-----------
 */
CmapIndex :: struct {
	version:          u16be,
	number_subtables: u16be,
}

CmapSubtable :: struct {
	platform_id:          enum u16be {
		unicode   = 0,
		macintosh = 1,
		reserved  = 2,
		microsoft = 3,
	},
	platform_specific_id: u16be,
	offset:               u32be,
}


CmapFormat4 :: struct {
	prefix:    struct {
		format, length, language: u16be,
	},
	header:    struct {
		seg_count_x2, search_range, entry_selector, range_shift: u16be,
	},
	segments:  #soa[]struct {
		end_code, start_code, id_delta, id_range_offset: u16be,
	},
	glyph_ids: []u16be,
}

/*
------------Glyf-----------
 */

GlyfDescription :: struct {
	n_contours:                 i16be,
	x_min, y_min, x_max, y_max: FWord,
}

SimpleGlyf :: struct {
	end_pts_of_contours: []u16be,
	instruction_len:     u16be,
	instructions:        []u8,
	flags:               [dynamic]OutlineFlags,
	coords:              #soa[dynamic]struct {
        x, y: i16be,
    },
}

OutlineFlags :: bit_set[OutlineFlag;u8]

OutlineFlag :: enum u8 {
	on_curve = 0,
	x_short_vec,
	y_short_vec,
	repeat,
	same_x_or_positive_short,
	same_y_or_positive_short,
}

CompoundGlyf :: struct {

}

ComponentGlyf :: struct {
    flags: ComponentFlags,
    glyph_idx: u16be,
    arguments: [2]union {
        u8, u16be, i8, i16be, 
    },
    transformation_option: union {
    },
    transform: struct {
        a, b, c, d, e, f: u16be,
    }
}



ComponentFlags :: bit_set[ComponentFlag; u16]
ComponentFlag :: enum u8 {
    args_are_words = 0,
    args_are_xy_values, 
    round_xy_to_grid,
    we_have_a_scale, 
    more_components = 5,
    we_have_a_x_and_y_scale,
    we_have_a_2_by_2,
    we_have_instructions,
    use_my_metrics,
    overlap_compound,
}
