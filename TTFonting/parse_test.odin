package ttfonting

import "core:testing"
import "core:log"

// @(require_results)
// get_val :: #force_inline proc "contextless" (
// 	data: []u8,
// 	index: int,
// 	$T: typeid,
// ) -> T {
// 	return (^T)(&data[index])^
// }

// get_string :: #force_inline proc(data: []byte, len: int) -> string {
// 	return strings.string_from_ptr(raw_data(data), 4)
// }

@(test)
main_test :: proc(_: ^testing.T) {
    // parse_ttf("../assets/fonts/JetBrainsMono-Thin.ttf", 1) // A
    // parse_ttf("../assets/fonts/JetBrainsMono-Thin.ttf", 26) // B
    // parse_ttf("../assets/fonts/JetBrainsMono-Thin.ttf", 27) // C

    glyfs := parse_ttf("../assets/fonts/JetBrainsMono-Thin.ttf")
    // glyfs := parse_ttf("../assets/fonts/JetBrainsMono-Thin.ttf", 'U')
    log.info(glyfs)
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
