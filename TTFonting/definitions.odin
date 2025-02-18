package ttfonting

ShortFrac :: bit_field i16be {
	// TODO: Revise this
	sign: bool  | 1,
	frac: u16be | 15,
}
Fixed :: bit_field i32be {
	whole: i16be | 16,
	frac:  i16be | 16,
}
FWord :: i16be
uFWord :: u16be
F2Dot14 :: bit_field i16be {
	whole: i16be | 2,
	frac:  i16be | 14,
}
LongDateTime :: i64be

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

OffsetSubtable :: struct #packed {
	scaler_type:                                           u32be,
	num_tables, search_range, entry_selector, range_shift: u16be,
}

TableDirectory :: struct #packed {
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
	Head,
	Maxp,
	Loca,
	[]Glyf,
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
------------Head-----------
 */

/*
    Head: https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6head.html 

    lsb: Left side bearing
    rsb: Right side bearing
    lbb: Left-most black bit
*/

Head :: struct #packed {
	version, font_revision:     Fixed,
	checksum_adj, magic_number: u32be,
	flags:                      HeadFlags,
	units_per_em:               u16be,
	created, modified:          LongDateTime,
	x_min, y_min, x_max, y_max: FWord,
	mac_style:                  MacStyle,
	lowest_rec_PPEM:            u16be, // lowest readable size in pixels
	font_direction_hint:        enum i16be {
		mixed_directional_glyphs                  = 0,
		only_strongly_left_to_right_glyphs        = 1,
		only_strongly_left_to_right_with_neutrals = 2,
		only_strongly_right_to_left_glyphs        = -1,
		only_strongly_right_to_left_with_neutrals = -2,
	},
	index_to_loc_fmt:           enum i16be {
		short_offsets = 0,
		long_offsets  = 1,
	},
	// glyph_data_fmt:             enum i16be {
	// 	current_format = 0,
	// },
}

HeadFlags :: bit_set[HeadFlag;u16be]
HeadFlag :: enum u8 {
	baseline_y0     = 0,
	lsb_at_x_lbb    = 1,
	integer_scaling = 2,
	zero            = 6,
}

MacStyle :: bit_set[MacStyleEnum;u16]
MacStyleEnum :: enum u16 {
	bold,
	italic,
	underline,
	outline,
	shadow,
	condensed,
	extended,
}

FontDirectionHint :: enum i16be {}

/*
------------Maxp-----------
 */

Maxp :: struct #packed {
	version:   Fixed,
	num_glyfs: u16be,
}


/*
------------Loca-----------
 */

Loca :: struct {
	offsets: union {
		[]u32be,
		[]u16be,
	},
}


/*
------------Glyf-----------
 */

Glyf :: struct {
	description: GlyfDescription,
	value:       union {
		SimpleGlyf,
		CompoundGlyf,
	},
}

GlyfDescription :: struct #packed {
	n_contours:                 i16be,
	x_min, y_min, x_max, y_max: FWord,
}

SimpleGlyf :: struct #packed {
	end_pts_of_contours: []u16be,
	instruction_len:     u16be,
	instructions:        []u8,
	// flags:               [dynamic]OutlineFlags,
	coords:              #soa[]Coord,
}

Coord :: struct {
	x, y:     i16,
	on_curve: bool,
}

OutlineFlags :: bit_set[OutlineFlag;u8]
OutlineFlag :: enum u8 {
	on_curve = 0,
	x_short_vec = 1,
	y_short_vec = 2,
	repeat = 3,
	same_x_or_positive_short = 4,
	same_y_or_positive_short = 5,
	zero1 = 6,
	zero2 = 7,
}

CompoundGlyf :: struct {
	components: []ComponentGlyf,
}

ComponentGlyf :: struct {
	flags:     ComponentFlags,
	glyph_idx: u16be,
	// The transform is interpreted at runtime, it is not a direct parse
	transform: struct {
		a, b, c, d, e, f: union {
			F2Dot14,
			u16be,
		},
	},
}

ComponentFlags :: bit_set[ComponentFlag;u16]
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
