package ttfonting

import "core:fmt"
import "core:log"
import "core:testing"

@(test)
main_test :: proc(_: ^testing.T) {
	// parse_ttf("../assets/fonts/JetBrainsMono-Thin.ttf", 1) // A
	// parse_ttf("../assets/fonts/JetBrainsMono-Thin.ttf", 26) // B
	// parse_ttf("../assets/fonts/JetBrainsMono-Thin.ttf", 27) // C

	// glyfs, _, _ := parse_ttf("../assets/fonts/JetBrainsMono-Thin.ttf")
	glyfs, _, _ := parse_ttf("../assets/fonts/IosevkaTermNerdFontMono-Light.ttf")

	// glyfs := parse_ttf("../assets/fonts/JetBrainsMono-Thin.ttf", 'U')
	// log.info(len(glyfs['9'].value.(SimpleGlyf).coords))
	// log.info(glyfs['9'].value.(SimpleGlyf).coords)
	log.info(glyfs['\"'].value.(CompoundGlyf))

    /*
	for coord in glyfs['9'].value.(SimpleGlyf).coords {
		fmt.printf("%d,%d\n", coord.x, coord.y)
	}
    */

	for glyf in glyfs {
		#partial switch v in glyf.value {
		case SimpleGlyf:
			delete(v.coords)
			delete(v.end_pts_of_contours)
		}
	}


	// for glyf in glyfs {

	//     delete(glyf.value.(SimpleGlyf).end_pts_of_contours)
	//     delete(glyf.value.(SimpleGlyf).instructions)
	//     delete(glyf.value.(SimpleGlyf).coords)
	// }
}


// 1092

/*
x=310 y=-10
x=219 y=-10
x=109 y=99
x=109 y=190
x=109 y=540
x=109 y=632
x=219 y=740
x=310 y=740
x=403 y=740
x=514 y=626
x=514 y=530
x=464 y=530
x=464 y=607
x=381 y=695
x=310 y=695
x=240 y=695
x=159 y=612
x=159 y=540
x=159 y=190
x=159 y=118
x=240 y=35
x=310 y=35
x=382 y=35
x=464 y=124
x=464 y=200
x=514 y=200
x=514 y=104
x=403 y=-10
 */
