package ttfonting

import "core:testing"

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
    parse_ttf("../assets/fonts/Anonymous.ttf")
}
