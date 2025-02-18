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
    // parse_ttf("../assets/fonts/JetBrainsMono-Thin.ttf", 1) // A
    parse_ttf("../assets/fonts/JetBrainsMono-Thin.ttf", 26) // B
    // parse_ttf("../assets/fonts/JetBrainsMono-Thin.ttf", 27) // C
}


// 1092


/*
x=110 y=0       110
x=110 y=730     110
x=298 y=730     298
x=389 y=730     389
x=498 y=628     498
x=498 y=542     498
x=498 y=505     498
x=466 y=437     466
x=410 y=391     410
x=375 y=384     375
x=375 y=381     375
x=415 y=375     415
x=478 y=322     478
x=515 y=245     478
x=515 y=200     478
x=515 y=109     441
x=400 y=0       326
x=301 y=0       227
x=160 y=398     368
x=298 y=398     506
x=369 y=398     577
x=448 y=474     577
x=448 y=541     577
x=448 y=608     498
x=368 y=684     418
x=298 y=684     348
x=160 y=684     348
x=160 y=46      486
x=301 y=46      627
x=378 y=46      704
x=466 y=128     704
x=466 y=199     704
x=466 y=270     616
x=378 y=353     528
x=301 y=353     451
x=160 y=353

110,0
110,-9626
298,-9626
389,-9626
498,-9712
498,-9749
498,-9817
466,-9863
410,-9870
375,-9873
375,-9879
415,-9932
478,-10009
478,-10054
478,-10145
441,-10254
326,-10254
227,-9856
368,-9856
506,-9856
577,-9780
577,-9713
577,-9646
498,-9570
418,-9570
348,-9570
348,-10208
486,-10208
627,-10208
704,-10126
704,-10055
704,-9984
616,-9901
528,-9901
451,-9901
-28987,-9901
 */
