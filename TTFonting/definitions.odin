package ttfonting

ShortFrac :: bit_field i16 {
	// TODO: Revise this
	sign: bool | 1,
	frac: u16  | 15,
}
Fixed :: bit_field i32 {
	whole: i16       | 16,
	frac:  ShortFrac | 16,
}
FWord :: i16
uFWord :: u16
F2Dot14 :: bit_field i16 {
	whole: i16       | 2,
	frac:  ShortFrac | 14,
}
LongDateTime :: u64

TableTag :: union {
	u64,
	[4]u8,
}

TableTagEnum :: enum {
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

TableDirectory :: struct {
	tag, checksum, offset, length: u32,
}
