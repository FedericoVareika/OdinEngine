package utils

// FIST: Fast Industrial-Strength Triangulation of Polygons
// https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=e5ba1f1d8d508f7d8f0aa3133cf947cc6578d524

// https://github.com/Samson-Mano/Delaunay-triangulation
// https://www.cs.cmu.edu/afs/andrew/scs/cs/15-463/2001/pub/src/a2/quadedge.html

// Quad edge data structure implementation: 
// https://github.com/rexdwyer/DelaunayTriangulation/blob/master/common.c#L157

/*
   Note(fede): 
    I want to clarify that my implementation of the quad edge data structure 
    strongly resembles that of Rex A. Dwyer (rexdwyer), i will try to implement
    the rest of this algorithm using:
        L.J. Guibas & J. Stolfi
        "Primitives for the manipulation of general subdivisions
        and the computation of Voronoi diagrams"
 */

EdgePtr :: i32
VertPtr :: i32
@(private = "file")
Vertex :: struct {
	x, y: i16,
}

MAX_VERTICES :: 200
MAX_EDGES :: (MAX_VERTICES - 1) * 3
NIL: EdgePtr : -1
EPSILON :: #config(EPSILON, 0)

@(private = "file")
State :: struct {
	edges:                  []EdgePtr,
	data:                   []VertPtr, // points to the mappings
	next_edge:              EdgePtr,
	available_edge:         EdgePtr,
	vertices:               #soa[]Vertex,
	vert_count:             int,
	mappings:               []u16,
	original_mappings:      []u16,
	removed:                []bool,
	origin_to_edge_mapping: []EdgePtr,
	on_curve:               []bool,
	end_contours:           []u16,
}

// @(private = "file")
@(private)
state: State

init_state :: proc(max_verts: u32) {
	n_edges := max_verts * 3
	state.vertices = make(#soa[]Vertex, max_verts)
	state.edges = make([]EdgePtr, n_edges * 4)
	state.data = make([]EdgePtr, n_edges * 2)

	state.original_mappings = make([]u16, max_verts)
	state.mappings = state.original_mappings
	state.origin_to_edge_mapping = make([]EdgePtr, max_verts)

	state.next_edge = 0
	state.available_edge = NIL
}

reset_state :: proc() {
	state.next_edge = 0
	state.available_edge = NIL
	state.mappings = state.original_mappings
}

deinit_state :: proc() {
	delete(state.vertices)
	delete(state.edges)
	delete(state.data)
	delete(state.original_mappings)
	delete(state.origin_to_edge_mapping)
}

@(require_results)
get_vert_from_ptr :: #force_inline proc(v: VertPtr) -> Vertex {
	return state.vertices[state.mappings[v]]
}

@(require_results)
get_vert_from_real :: #force_inline proc(v: u16) -> Vertex {
	return state.vertices[v]
}

get_vert :: proc {
	get_vert_from_ptr,
	get_vert_from_real,
}

@(require_results)
rot_1 :: #force_inline proc(e: EdgePtr) -> EdgePtr {
	return (e &~ 3) | ((e + 1) & 3)
}

@(require_results)
rot_n :: #force_inline proc(e: EdgePtr, $N: i32) -> EdgePtr where N < 4,
	N > -4 {
	return (e &~ 3) | ((e + N) & 3)
}

rot :: proc {
	rot_1,
	rot_n,
}

@(require_results)
rot_inv :: #force_inline proc(e: EdgePtr) -> EdgePtr {
	return rot_n(e, 3)
}

@(require_results)
o_next :: #force_inline proc(e: EdgePtr) -> EdgePtr {
	return state.edges[e]
}

@(require_results)
l_next :: #force_inline proc(e: EdgePtr) -> EdgePtr {
	return rot(o_next(rot_inv(e)))
}

@(require_results)
r_next :: #force_inline proc(e: EdgePtr) -> EdgePtr {
	return rot_inv(o_next(rot(e)))
}

@(require_results)
sym :: #force_inline proc(e: EdgePtr) -> EdgePtr {
	return rot(e, 2)
}

@(require_results)
d_next :: #force_inline proc(e: EdgePtr) -> EdgePtr {
	return sym(o_next(sym(e)))
}

@(require_results)
o_prev :: #force_inline proc(e: EdgePtr) -> EdgePtr {
	return rot(o_next(rot(e)))
}

@(require_results)
l_prev :: #force_inline proc(e: EdgePtr) -> EdgePtr {
	return sym(o_next(e))
}

@(require_results)
r_prev :: #force_inline proc(e: EdgePtr) -> EdgePtr {
	return o_next(sym(e))
}

@(require_results)
d_prev :: #force_inline proc(e: EdgePtr) -> EdgePtr {
	return rot_inv(o_next(rot_inv(e)))
}

orig :: #force_inline proc(e: EdgePtr) -> ^VertPtr {
	return &state.data[e / 2]
}

dest :: #force_inline proc(e: EdgePtr) -> ^VertPtr {
	return &state.data[sym(e) / 2]
}

@(require_results)
real_orig :: proc(e: EdgePtr) -> u16 {
	return state.mappings[orig(e)^]
}

@(require_results)
real_dest :: proc(e: EdgePtr) -> u16 {
	return state.mappings[dest(e)^]
}

@(require_results)
alloc_edge :: proc() -> EdgePtr {
	if state.available_edge == NIL {
		e := state.next_edge
		state.next_edge += 4
		return e
	}

	e := state.available_edge
	state.available_edge = o_next(state.available_edge)
	return e
}

free_edge :: proc(e: EdgePtr) {
	e := e ~ (e & 3)
	state.edges[e] = state.available_edge
	state.available_edge = e
}

@(require_results)
make_edge :: proc(v1, v2: VertPtr) -> (e: EdgePtr) {
	e = alloc_edge()
	assert(e % 4 == 0)

	/*
            L=R 
       a --------- b
            R=L
     */

	state.edges[e] = e
	state.edges[e + 1] = e + 3
	state.edges[e + 2] = e + 2
	state.edges[e + 3] = e + 1

	orig(e)^ = v1
	dest(e)^ = v2
	// log.debug("orig(e)^", orig(e)^)
	// log.debug("state.mappings[orig(e)^]", state.mappings[orig(e)^])
	state.origin_to_edge_mapping[real_orig(e)] = e
	state.origin_to_edge_mapping[real_dest(e)] = sym(e)

	return
}

splice :: proc(a, b: EdgePtr) {
	alpha := rot(o_next(a))
	beta := rot(o_next(b))

	a_next := o_next(a)
	b_next := o_next(b)
	alpha_next := o_next(alpha)
	beta_next := o_next(beta)

	state.edges[a] = b_next
	state.edges[b] = a_next
	state.edges[alpha] = beta_next
	state.edges[beta] = alpha_next
}

connect :: proc(a, b: EdgePtr) -> EdgePtr {
	e := make_edge(dest(a)^, orig(b)^)
	// orig(e)^ = dest(a)^
	// state.origin_to_edge_mapping[state.mappings[orig(e)^]] = e

	// dest(e)^ = orig(b)^

	splice(e, l_next(a))
	splice(sym(e), b)
	return e
}

delete_edge :: proc(e: EdgePtr) {
	splice(e, o_prev(e))
	splice(sym(e), o_prev(sym(e)))
	free_edge(e)
}

swap :: proc(e: EdgePtr, loc := #caller_location) {
	a := o_prev(e)
	b := o_prev(sym(e))
	splice(e, a)
	splice(sym(e), b)
	splice(e, l_next(a))
	splice(sym(e), l_next(b))

	// orig(e)^ = orig(l_next(a))^
	// Note(fede): should not have to change the dest value since it is the same (i think)
	// dest(e)^ = orig(l_next(b))^
	orig(e)^ = dest(a)^
	dest(e)^ = dest(b)^
	state.origin_to_edge_mapping[real_orig(e)] = e
	state.origin_to_edge_mapping[real_dest(e)] = sym(e)
	state.origin_to_edge_mapping[real_orig(a)] = a
	state.origin_to_edge_mapping[real_orig(b)] = b
}

import la "core:math/linalg"

@(require_results)
incircle_vec2 :: proc(pts: [4]Vertex) -> bool {
	x2_y2: [4]i128 = {}
	for &v, i in x2_y2 {
		px := i128(pts[i].x) * i128(pts[i].x)
		py := i128(pts[i].y) * i128(pts[i].y)
		v = px + py
	}

	m: matrix[4, 4]i128
	m[0] = {i128(pts[0].x), i128(pts[1].x), i128(pts[2].x), i128(pts[3].x)}
	m[1] = {i128(pts[0].y), i128(pts[1].y), i128(pts[2].y), i128(pts[3].y)}
	m[2] = x2_y2
	m[3] = {1, 1, 1, 1}
	d := la.determinant(m)

	return d > EPSILON
	// return d > 0
	// return d > EPSILON
}


/*
This algorigthm is stolen from Jonathan Richard Shewchuk, more info:
    https://www.cs.cmu.edu/~quake/robust.html
 */
@(require_results)
incircle_fast :: proc(pts: [4]Vertex) -> bool {
	a: [2]i128 = {i128(pts[0].x), i128(pts[0].y)}
	b: [2]i128 = {i128(pts[1].x), i128(pts[1].y)}
	c: [2]i128 = {i128(pts[2].x), i128(pts[2].y)}
	d: [2]i128 = {i128(pts[3].x), i128(pts[3].y)}


	adx, ady, bdx, bdy, cdx, cdy: i128
	abdet, bcdet, cadet: i128
	alift, blift, clift: i128

	adx = a[0] - d[0]
	ady = a[1] - d[1]
	bdx = b[0] - d[0]
	bdy = b[1] - d[1]
	cdx = c[0] - d[0]
	cdy = c[1] - d[1]

	abdet = adx * bdy - bdx * ady
	bcdet = bdx * cdy - cdx * bdy
	cadet = cdx * ady - adx * cdy
	alift = adx * adx + ady * ady
	blift = bdx * bdx + bdy * bdy
	clift = cdx * cdx + cdy * cdy

	return alift * bcdet + blift * cadet + clift * abdet > EPSILON
}

@(require_results)
incircle_ptr :: proc(pts: [4]VertPtr) -> bool {
	pts_vec2: [4]Vertex
	for &p, i in pts_vec2 {
		p = get_vert(pts[i])
		// state.vertices[state.mappings[pts[i]]]
	}
	return incircle_fast(pts_vec2)
}

@(require_results)
incircle_expand_ptr :: #force_inline proc(p1, p2, p3, p4: VertPtr) -> bool {
	return incircle_ptr({p1, p2, p3, p4})
}

@(require_results)
ccw_vec2 :: proc(a, b, c: Vertex) -> bool {
	m: matrix[3, 3]i128
	m[0] = {i128(a.x), i128(b.x), i128(c.x)}
	m[1] = {i128(a.y), i128(b.y), i128(c.y)}
	m[2] = {1, 1, 1}

	// epsilon_calc := abs(max(a.x, b.x, c.x, a.y, b.y, c.y)) * 0.0001
	// log.info("epsilon calc:", epsilon_calc)
	d := la.determinant(m)
	// log.debug("det:", d)
	// return d > 0
	return d > EPSILON
	// return d > max(epsilon_calc, EPSILON)
}

@(require_results)
ccw_ptr :: proc(A, B, C: VertPtr) -> bool {
	a := get_vert(A)
	b := get_vert(B)
	c := get_vert(C)
	// a := state.vertices[state.mappings[A]]
	// b := state.vertices[state.mappings[B]]
	// c := state.vertices[state.mappings[C]]
	// log.debug("a:", a, "b:", b, "c:", c)
	return ccw_vec2(a, b, c)
}

@(require_results)
right_of_vec2 :: proc(x: Vertex, e: EdgePtr) -> bool {
	return ccw(
		x,
		get_vert(dest(e)^),
		get_vert(orig(e)^),
		// state.vertices[state.mappings[dest(e)^]],
		// state.vertices[state.mappings[orig(e)^]],
	)
}

@(require_results)
right_of_ptr :: proc(x: VertPtr, e: EdgePtr) -> bool {
	return ccw(x, dest(e)^, orig(e)^)
}

@(require_results)
left_of_vec2 :: proc(x: Vertex, e: EdgePtr) -> bool {
	// return ccw(x, state.vertices[orig(e)^], state.vertices[dest(e)^])
	return ccw(x, get_vert(orig(e)^), get_vert(dest(e)^))
}

@(require_results)
left_of_ptr :: proc(x: VertPtr, e: EdgePtr) -> bool {
	return ccw(x, orig(e)^, dest(e)^)
}

@(require_results)
valid :: proc(cand, base_l: EdgePtr) -> bool {
	return ccw(orig(base_l)^, dest(cand)^, dest(base_l)^)
}

ccw :: proc {
	ccw_ptr,
	ccw_vec2,
}
right_of :: proc {
	right_of_ptr,
	right_of_vec2,
}
left_of :: proc {
	left_of_ptr,
	left_of_vec2,
}
incircle :: proc {
	incircle_ptr,
	incircle_expand_ptr,
	incircle_vec2,
}

collinear :: proc(a, b, c: u16) -> bool {
	a_vert := get_vert(a)
	b_vert := get_vert(b)
	c_vert := get_vert(c)

	m: matrix[3, 3]i128
	m[0] = {i128(a_vert.x), i128(b_vert.x), i128(c_vert.x)}
	m[1] = {i128(a_vert.y), i128(b_vert.y), i128(c_vert.y)}
	m[2] = {1, 1, 1}

	d := la.determinant(m)
	return abs(d) <= EPSILON
}

init_mappings_wo_len :: proc() {
	for i in 0 ..< state.vert_count do state.mappings[i] = u16(i)
}

init_mappings_w_len :: proc(verts: int) {
	for i in 0 ..< verts do state.mappings[i] = u16(i)
}

init_mappings :: proc {
	init_mappings_w_len,
	init_mappings_wo_len,
}

switch_places :: proc(#any_int i, j: int) {
	{
		temp := state.mappings[j]
		state.mappings[j] = state.mappings[i]
		state.mappings[i] = temp
	}
}

partition :: proc(#any_int l, u: int) -> VertPtr {
	pivot := get_vert(VertPtr(u))
	i := l

	// fmt.println(l, u)
	for m, j in state.mappings[l:u] {
		v := state.vertices[m]
		if v.x < pivot.x || (v.x == pivot.x && v.y < pivot.y) {
			switch_places(i, j + l)
			i += 1
		}
	}

	switch_places(i, u)

	return VertPtr(i)
}

quick_sort :: proc(#any_int l, u: int) {
	// fmt.println(l, u)
	// fmt.println(l, u)
	if l >= u do return

	pivot := partition(l, u)
	quick_sort(l, pivot - 1)
	quick_sort(pivot + 1, u)
}

remove_duplicates :: proc() -> []bool {
	n_verts := state.vert_count
	to_remove := make([]bool, n_verts)
	for i := 1; i < n_verts; i += 1 {
		prev_mapping := state.mappings[i - 1]
		mapping := state.mappings[i]
		if get_vert(mapping) == get_vert(prev_mapping) {
			to_remove[mapping] = true
		}
	}

	removed := 0
	for i := 0; i < n_verts; i += 1 {
		if to_remove[state.mappings[i]] {
			removed += 1
			continue
		}
		state.mappings[i - removed] = state.mappings[i]
	}

	state.mappings = state.mappings[:n_verts - removed]
	return to_remove
}

delaunay :: proc(set: []u16, offset: i32 = 0) -> (le, re: EdgePtr) {
	switch len(set) {
	case 2:
		a := make_edge(offset, offset + 1)
		return a, sym(a)
	case 3:
		a := make_edge(offset, offset + 1)
		b := make_edge(offset + 1, offset + 2)
		splice(sym(a), b)
		if ccw(
			state.vertices[set[0]],
			state.vertices[set[1]],
			state.vertices[set[2]],
		) {
			c := connect(b, a)
			return a, sym(b)
		} else if ccw(
			state.vertices[set[0]],
			state.vertices[set[2]],
			state.vertices[set[1]],
		) {
			c := connect(b, a)
			return sym(c), c
		} else do return a, sym(b)
	case:
		L := set[0:len(set) / 2]
		R := set[len(set) / 2:]
		when ODIN_DEBUG {
			log.debug("L", L)
			log.debug("R", R)
		}
		ldo, ldi := delaunay(L, offset = offset)
		rdi, rdo := delaunay(R, offset = offset + i32(len(set) / 2))

		// Compute the lower common tangent of L and R
		for {
			if left_of(orig(rdi)^, ldi) do ldi = l_next(ldi)
			else if right_of(orig(ldi)^, rdi) do rdi = r_prev(rdi)
			else do break
		}

		// Create first cross edge base_l from rdi.orig to ldi.orig
		base_l := connect(sym(rdi), ldi)
		if orig(ldi)^ == orig(ldo)^ do ldo = sym(base_l)
		if orig(rdi)^ == orig(rdo)^ do rdo = base_l

		merge_loop: for {
			// Locate the first L point (l_cand.dest) to be encountered by the
			// rising bubble and delete L edges of base_l.dest that fail the 
			// circle test
			l_cand := o_next(sym(base_l))
			if valid(l_cand, base_l) { 	// valid
				for incircle(
					    dest(base_l)^,
					    orig(base_l)^,
					    dest(l_cand)^,
					    dest(o_next(l_cand))^,
				    ) {
					when ODIN_DEBUG {
						log.debug(
							"deleting inner lcands that dont comply with incircle",
						)
					}
					t := o_next(l_cand)
					delete_edge(l_cand)
					l_cand = t
				}
			}

			// Symmetrically, locate the first R point to be hit, delete the rest
			r_cand := o_prev(base_l)
			if valid(r_cand, base_l) { 	// valid
				for incircle(
					    dest(base_l)^,
					    orig(base_l)^,
					    dest(r_cand)^,
					    dest(o_prev(r_cand))^,
				    ) {
					when ODIN_DEBUG {
						log.debug(
							"deleting inner rcands that dont comply with incircle",
						)
					}
					t := o_prev(r_cand)
					delete_edge(r_cand)
					r_cand = t
				}
			}

			// If both are invalid, then base_l is the upper common tangent
			if !valid(l_cand, base_l) && !valid(r_cand, base_l) {
				break merge_loop
			}

			right_is_delaunay :=
				!valid(l_cand, base_l) ||
				(valid(r_cand, base_l) &&
						incircle(
							dest(l_cand)^,
							orig(l_cand)^,
							orig(r_cand)^,
							dest(r_cand)^,
						))

			when ODIN_DEBUG {
				log.debug("right is delaunay:", right_is_delaunay)
			}

			if right_is_delaunay do base_l = connect(r_cand, sym(base_l))
			else do base_l = connect(sym(base_l), sym(l_cand)) // l_cand is delaunay
		}
		return ldo, rdo
	}
}

@(require_results)
get_contour :: proc(end_contours: []u16, vert: u16) -> (start, end: u16) {
	start = 0
	for e, idx in end_contours {
		end = e
		if vert <= e do break
		start = e + 1
	}
	return
}

@(require_results)
get_next_vert_wo_contour :: proc(
	end_contours: []u16,
	vert: u16,
) -> (
	next: u16,
) {
	start, end := get_contour(end_contours, vert)
	if vert == end do return start
	return vert + 1
}

@(require_results)
get_next_vert_wi_contour :: proc(start, end, vert: u16) -> (next: u16) {
	if vert >= end do return start + (vert - end)
	return vert + 1
}

get_next_vert :: proc {
	get_next_vert_wo_contour,
	get_next_vert_wi_contour,
}

is_constraint_edge_w_params :: proc(
	on_curve: []bool,
	end_contours: []u16,
	e: EdgePtr,
) -> bool {

	orig_vert := real_orig(e)
	dest_vert := real_dest(e)

	if orig_vert >= u16(len(on_curve)) do return false
	if dest_vert >= u16(len(on_curve)) do return false

	so, eo := get_contour(end_contours, orig_vert)
	sd, ed := get_contour(end_contours, dest_vert)

	if so != sd do return false

	s := so
	e := eo

	next_orig_vert := get_next_vert(s, e, orig_vert)
	next_dest_vert := get_next_vert(s, e, dest_vert)
	// log.info("checking constraint:", orig_vert, dest_vert, s, e, next_orig_vert, next_dest_vert)
	if dest_vert == next_orig_vert do return true
	if orig_vert == next_dest_vert do return true

	if !on_curve[next_orig_vert] {
		next_next_vert := get_next_vert(s, e, next_orig_vert)
		if dest_vert == next_next_vert do return true
	}

	if !on_curve[next_dest_vert] {
		next_next_vert := get_next_vert(s, e, next_dest_vert)
		if orig_vert == next_next_vert do return true
	}

	return false
}

@(require_results)
is_constraint_edge_wo_params :: proc(e: EdgePtr) -> bool {
	return is_constraint_edge(state.on_curve, state.end_contours, e)
}

is_constraint_edge :: proc {
	is_constraint_edge_w_params,
	is_constraint_edge_wo_params,
}

@(require_results)
is_bound_edge :: proc(on_curve: []bool, e: EdgePtr) -> bool {
	orig_vert := real_orig(e)
	dest_vert := real_dest(e)

	if orig_vert >= u16(len(on_curve)) do return true
	if dest_vert >= u16(len(on_curve)) do return true

	return false
}

append_valid_triangles :: proc(
	dest_dyn: ^[dynamic]Triangle,
	uvs_dyn: ^[dynamic]TriangleUV,
	end_contours: []u16,
	on_curve: []bool,
	visited_edges: ^[]bool,
	start_edge: EdgePtr,
) {
	first := true
	start_edge := start_edge
	running := true
	for running {
		log.info("appending at:", real_orig(start_edge), real_dest(start_edge))
		defer {
			start_edge = o_prev(start_edge)
			if is_constraint_edge(on_curve, end_contours, start_edge) {
				log.info(
					"edge is constraint",
					real_orig(start_edge),
					real_dest(start_edge),
				)
				running = false
			}
		}
		defer first = false

		if visited_edges[start_edge] do continue

		triangle_constraint_edge := start_edge

		is_curve := false
		off_curve_idx := 0

		new_uv: TriangleUV = {{{0, 1}, true}, {{0, 1}, true}, {{0, 1}, true}}

		new_tri: Triangle
		prev_vert := real_orig(start_edge)
		for k in 0 ..< 3 {
			if first && !on_curve[real_orig(start_edge)] {
				is_curve = true
				off_curve_idx = k
			}

			visited_edges[start_edge] = true
			new_tri[k] = u32(real_orig(start_edge))
			start_edge = r_prev(start_edge)
		}
		assert(start_edge == triangle_constraint_edge)

		inner_edge := false
		if off_curve_idx == 2 do inner_edge = true
		if off_curve_idx == 0 do is_curve = false
		if first {
			a := u16(new_tri[0])
			b := u16(new_tri[1])
			c := u16(new_tri[2])
			a_s, a_e := get_contour(end_contours, a)
			b_s, b_e := get_contour(end_contours, b)
			c_s, c_e := get_contour(end_contours, c)
			consecutive :=
				!inner_edge &&
					(get_next_vert(a_s, a_e, a) == b &&
								get_next_vert(b_s, b_e, b) == c ||
							get_next_vert(b_s, b_e, b) == c &&
								get_next_vert(c_s, c_e, c) == a ||
							get_next_vert(c_s, c_e, c) == a &&
								get_next_vert(a_s, a_e, a) == b) ||
				inner_edge &&
					(get_next_vert(a_s, a_e, a) == c &&
								get_next_vert(c_s, c_e, c) == b ||
							get_next_vert(b_s, b_e, b) == a ||
							get_next_vert(a_s, a_e, a) == c &&
								get_next_vert(c_s, c_e, c) == b &&
								get_next_vert(b_s, b_e, b) == a)
			if !consecutive do is_curve = false
		}

		if is_curve {
			after := inner_edge ? off_curve_idx + 1 : off_curve_idx + 2
			after %%= 3
			before := inner_edge ? off_curve_idx + 2 : off_curve_idx + 1
			before %%= 3

			// if get_next_vert(s, e, new_tri[before]) != 

			new_uv[before].uv = {0, 0}
			new_uv[off_curve_idx].uv = {0.5, 0}
			new_uv[after].uv = {1, 1}
			if inner_edge {
				new_uv[0].z = false
				new_uv[1].z = false
				new_uv[2].z = false
			}
		}

		// new_uv = {{{0, 1}, true}, {{0, 1}, true}, {{0, 1}, true}}

		append(dest_dyn, new_tri)
		append(uvs_dyn, new_uv)

		// start_edge = o_prev(start_edge)
	}
}

get_edge :: proc(start, end: u16) -> EdgePtr {
	e := state.origin_to_edge_mapping[start]
	for real_dest(e) != end {
		e = o_next(e)
	}

	return e
}

traverse_triangles_2 :: proc(
	dest_dyn: ^[dynamic]Triangle,
	uvs_dyn: ^[dynamic]TriangleUV,
	end_contours: []u16,
	on_curve: []bool,
) {
	visited_edges := make([]bool, state.next_edge)
	defer delete(visited_edges)
	// log.info(end_contours)

	c_start: u16 = 0
	for c_end in end_contours {
		defer c_start = c_end + 1
		// log.info(c_start, c_end)
		for i := c_start; i <= c_end; i += 1 {
			if state.removed[i] do continue
			next_vert := get_next_vert(c_start, c_end, i)
			for state.removed[next_vert] {
				next_vert = get_next_vert(c_start, c_end, next_vert)
			}
			constraint_edge := get_edge(i, next_vert)
			assert(constraint_edge < state.next_edge)

			// Append valid trianges 
			// (the ones to the right of the constraint edge)
			// if !visited_edges[constraint_edge] {
			append_valid_triangles(
				dest_dyn,
				uvs_dyn,
				end_contours,
				on_curve,
				&visited_edges,
				constraint_edge,
			)
			// }

			if on_curve[next_vert] do continue

			next_next_vert := get_next_vert(c_start, c_end, next_vert)
			for state.removed[next_next_vert] {
				next_next_vert = get_next_vert(c_start, c_end, next_next_vert)
			}
			if !collinear(i, next_vert, next_next_vert) {
				constraint_edge = get_edge(i, next_next_vert)
				// if !visited_edges[constraint_edge] {
				append_valid_triangles(
					dest_dyn,
					uvs_dyn,
					end_contours,
					on_curve,
					&visited_edges,
					constraint_edge,
				)
				// }
			}
		}
	}
}

cut :: #config(CUT, true)
traverse_triangles :: proc(
	dest_dyn: ^[dynamic]Triangle,
	uvs_dyn: ^[dynamic]TriangleUV,
	end_contours: []u16,
	on_curve: []bool,
) {
	visited_edges := make(map[EdgePtr]bool, context.temp_allocator)
	main_loop: for i := 0; i < int(state.next_edge); i += 2 {
		e := state.edges[i]
		if i == int(state.available_edge) {
			state.available_edge = o_next(state.available_edge)
			continue
		}
		half_edge: for _ in 0 ..< 2 {
			defer e = sym(e)
			if visited_edges[e] do continue

			current_edge := e
			new_tri: Triangle
			// new_uv: TriangleUV = {{0, 1, 1}, {0, 1, 1}, {0, 1, 1}}
			new_uv: TriangleUV = {
				{{0, 1}, true},
				{{0, 1}, true},
				{{0, 1}, true},
			}

			triangle: for tri_idx in 0 ..< 3 {
				defer current_edge = l_next(current_edge)
				visited_edges[current_edge] = true

				current_origin := real_orig(current_edge)
				current_dest := real_dest(current_edge)

				new_tri[tri_idx] = u32(current_origin)
			}

			if e != current_edge do continue

			should_cut: bool
			{ 	// Mastermined by me, can you tell? 
				a := u16(new_tri[0])
				b := u16(new_tri[1])
				c := u16(new_tri[2])

				when cut == true {
					start_a, end_a := get_contour(end_contours, a)
					start_b, end_b := get_contour(end_contours, b)
					start_c, end_c := get_contour(end_contours, c)
					same_contour := end_a == end_b && end_a == end_c
					s := start_a
					e := end_a

					cw_from_a :=
						get_next_vert(s, e, a) == c &&
						get_next_vert(s, e, c) == b
					cw_from_b :=
						get_next_vert(s, e, b) == a &&
						get_next_vert(s, e, a) == c
					cw_from_c :=
						get_next_vert(s, e, c) == b &&
						get_next_vert(s, e, b) == a
					outer_edge := cw_from_a || cw_from_b || cw_from_c // cw orientation 

					ccw_from_a :=
						get_next_vert(s, e, a) == b &&
						get_next_vert(s, e, b) == c
					ccw_from_b :=
						get_next_vert(s, e, b) == c &&
						get_next_vert(s, e, c) == a
					ccw_from_c :=
						get_next_vert(s, e, c) == a &&
						get_next_vert(s, e, a) == b
					inner_edge := ccw_from_a || ccw_from_b || ccw_from_c // ccw orientation 

					vertices_growing :=
						(a < b && b < c) ||
						(a < b && a > c) ||
						(a > b && b < c && a > c) ||
						(a > b && a > c && a < c)
					consecutive_verts :=
						get_next_vert(start_a, end_a, a) == b ||
						get_next_vert(start_b, end_b, b) == c ||
						get_next_vert(start_c, end_c, c) == a
					is_outside := same_contour && vertices_growing
					filled := on_curve[a] && on_curve[b] && on_curve[c]

					filled ||= !on_curve[a] && !(ccw_from_c || cw_from_b)
					filled ||= !on_curve[b] && !(ccw_from_a || cw_from_c)
					filled ||= !on_curve[c] && !(ccw_from_b || cw_from_a)

					should_cut =
						is_outside && filled ||
						(consecutive_verts && !(outer_edge || inner_edge))

					when cut == true {
						if should_cut do continue
					}


					// Uv calculations
					if !filled && (inner_edge || outer_edge) {
						off_curve_idx := 0
						for v, i in new_tri {
							if !on_curve[v] do off_curve_idx = i
						}
						after :=
							inner_edge ? off_curve_idx + 1 : off_curve_idx + 2
						after %%= 3
						before :=
							inner_edge ? off_curve_idx + 2 : off_curve_idx + 1
						before %%= 3

						new_uv[before].uv = {0, 0}
						new_uv[off_curve_idx].uv = {0.5, 0}
						new_uv[after].uv = {1, 1}
						if inner_edge {
							new_uv[0].z = false
							new_uv[1].z = false
							new_uv[2].z = false
						}
					}
				}
			}

			new_uv = {{{0, 1}, true}, {{0, 1}, true}, {{0, 1}, true}}

			log.debug(new_tri)
			append(dest_dyn, new_tri)
			append(uvs_dyn, new_uv)
		}
	}
}

@(require_results)
intersects :: proc(a1, b1, a2, b2: Vertex) -> bool {
	// (a1b1 x b1b2) . (a1b1 x b1a2) < 0
	// (a2b2 x b2b1) . (a2b2 x b2a1) < 0
	a1_: [2]i128 = {i128(a1.x), i128(a1.y)}
	b1_: [2]i128 = {i128(b1.x), i128(b1.y)}
	a2_: [2]i128 = {i128(a2.x), i128(a2.y)}
	b2_: [2]i128 = {i128(b2.x), i128(b2.y)}

	a1b1 := b1_ - a1_
	b1b2 := b2_ - b1_
	b1a2 := a2_ - b1_

	// log.debug("cross(a1b1, b1b2)", la.cross(a1b1, b1b2))
	// log.debug("cross(a1b1, b1a2)", la.cross(a1b1, b1a2))
	// log.debug("dot(crosses)", la.cross(a1b1, b1b2) * la.cross(a1b1, b1a2))
	result := la.cross(a1b1, b1b2) * la.cross(a1b1, b1a2) < 0

	a2b2 := b2_ - a2_
	b2b1 := b1_ - b2_
	b2a1 := a1_ - b2_
	// log.debug("cross(a1b1, b1b2)", la.cross(a2b2, b2b1))
	// log.debug("cross(a1b1, b1a2)", la.cross(a2b2, b2a1))
	// log.debug("dot(crosses)", la.cross(a2b2, b2b1) * la.cross(a2b2, b2a1))
	result &&= la.cross(a2b2, b2b1) * la.cross(a2b2, b2a1) < 0
	// result &&= linalg.vector_dot(linalg.vector_cross2(a2b2, b2b1), linalg.vector_cross2(a2b2, b2a1)) < 0

	// log.debugf("intersects_vecs: [ %d, %d ], [ %d, %d ]", a1b1.x, a1b1.y, a2b2.x, a2b2.y)
	// log.debug(result)
	return result
}

apply_constraint :: proc(a, b: u16) {
	a_edge: EdgePtr = state.origin_to_edge_mapping[a]
	b_edge: EdgePtr = state.origin_to_edge_mapping[b]

	a_vert_ptr: VertPtr = orig(a_edge)^
	b_vert_ptr: VertPtr = orig(b_edge)^

	when ODIN_DEBUG {
		log.debug(a, b)
		log.debug(a_edge, b_edge)
	}

	assert(get_vert(a_vert_ptr) == state.vertices[a])
	assert(get_vert(b_vert_ptr) == state.vertices[b])

	a_vert := get_vert(a_vert_ptr)
	b_vert := get_vert(b_vert_ptr)

	for !left_of(b_vert_ptr, a_edge) || !right_of(b_vert_ptr, o_next(a_edge)) {
		if dest(a_edge)^ == orig(b_edge)^ {
			when ODIN_DEBUG {
				log.debug(
					"edge already exists:",
					real_orig(a_edge),
					real_dest(a_edge),
				)
			}
			return
		}
		a_edge = o_next(a_edge)
	}
	when ODIN_DEBUG {
		log.debug("found_pos", real_orig(a_edge), real_dest(a_edge))
	}

	aux := l_next(a_edge)

	{
		second := aux
		for _ in 0 ..= 3 {
			log.debug("lnext = ", real_orig(second), real_dest(second))
			second = l_next(second)
		}
	}

	log.debug("aux", real_orig(aux), real_dest(aux))
	intersecting_queue := make([dynamic]EdgePtr, context.temp_allocator)

	j := 0
	main_loop: for dest(aux)^ != b_vert_ptr &&
	    dest(o_next(aux))^ != b_vert_ptr {
		defer j += 1
		if j > 100 do break

		log.info(real_orig(aux), real_dest(aux))

		aux_orig := get_vert(orig(aux)^)
		aux_dest := get_vert(dest(aux)^)
		k := 0
		log.info(
			"intersects(",
			a_vert,
			b_vert,
			aux_orig,
			aux_dest,
			"):",
			intersects(a_vert, b_vert, aux_orig, aux_dest),
		)
		for !intersects(a_vert, b_vert, aux_orig, aux_dest) {
			defer k += 1
			if dest(aux)^ == b_vert_ptr do break main_loop
			aux = l_next(aux)
			aux_orig = get_vert(orig(aux)^)
			aux_dest = get_vert(dest(aux)^)
		}

		append(&intersecting_queue, aux)
		aux = sym(aux)
		aux = l_next(aux)
	}

	when ODIN_DEBUG {
		for inter, i in intersecting_queue {
			log.info("[", i, "]", real_orig(inter), real_dest(inter))
		}
	}

	i := 0
	for len(intersecting_queue) != 0 {
		e := intersecting_queue[i]
		prev_orig := real_orig(e)
		prev_dest := real_dest(e)
		swap(e)
		e_origin := get_vert(orig(e)^)
		e_dest := get_vert(dest(e)^)


		if (orig(e)^ == a_vert_ptr && dest(e)^ == b_vert_ptr) ||
		   (orig(e)^ == b_vert_ptr && dest(e)^ == a_vert_ptr) ||
		   !intersects(a_vert, b_vert, e_origin, e_dest) {
			if collinear(real_orig(e), real_dest(e), prev_orig) ||
			   collinear(real_orig(e), real_dest(e), prev_dest) {
				when ODIN_DEBUG {
					log.info(
						"edge is collinear, do not remove: ",
						real_orig(e),
						real_dest(e),
					)
				}
			} else {
				when ODIN_DEBUG {
					log.info("removing: ", real_orig(e), real_dest(e))
				}
				unordered_remove(&intersecting_queue, i)
				i -= 1
			}
		}
		if len(intersecting_queue) == 0 do break
		i = (i + 1) %% len(intersecting_queue)
	}
}

apply :: #config(APPLY, true)
@(disabled = !apply)
apply_constraints :: proc(end_contours: []u16, on_curve: []bool) {
	c_start: u16 = 0
	for c_end in end_contours {
		defer c_start = c_end + 1
		for i := c_start; i <= c_end; i += 1 {
			if state.removed[i] do continue

			next_vert := get_next_vert(c_start, c_end, i)
			for state.removed[next_vert] {
				next_vert = get_next_vert(c_start, c_end, next_vert)
			}

			apply_constraint(i, next_vert)
			next_next_vert := get_next_vert(c_start, c_end, next_vert)
			for state.removed[next_next_vert] {
				next_next_vert = get_next_vert(c_start, c_end, next_next_vert)
			}
			if !on_curve[next_vert] &&
			   !collinear(i, next_vert, next_next_vert) {
				apply_constraint(i, next_next_vert)
			}
		}
	}
}

append_vertex :: proc(all_verts: ^[dynamic]Vec2f, v: Vertex) -> int {
	state.vertices[state.vert_count] = v
	state.vert_count += 1

	v_f := Vec2f{f32(v.x), f32(v.y)}
	append(all_verts, v_f)

	return state.vert_count - 1
}

add_bounding_box :: proc(all_verts: ^[dynamic]Vec2f) {
	leftmost: i16 = -32768
	rightmost: i16 = 32767
	uppermost: i16 = -32768
	lowermost: i16 = 32767


	for i in 0 ..< state.vert_count {
		v := state.vertices[i]
		if v.x > leftmost do leftmost = v.x
		if v.x < rightmost do rightmost = v.x
		if v.y > uppermost do uppermost = v.y
		if v.y < lowermost do lowermost = v.y
	}

	x_size := rightmost - leftmost
	y_size := uppermost - lowermost

	x_min := leftmost - x_size / 10
	x_max := rightmost + x_size / 10
	y_min := lowermost - y_size / 10
	y_max := uppermost + y_size / 10

	bounding_box := [?]Vertex {
		Vertex{x_min, y_min},
		Vertex{x_min, y_max},
		Vertex{x_max, y_min},
		Vertex{x_max, y_max},
	}

	for v, i in bounding_box {
		append_vertex(all_verts, v)
	}
}

EdgeFlags :: bit_set[EdgeFlag;u8]
EdgeFlag :: enum {
	visited_inside,
	visited_outside,
	midpoint_added,
}

TriangleType :: enum {
	sleeve,
	junction_a,
	junction_o,
	terminal,
	hole,
	bounding,
	curve,
}

triangle_type :: proc(
	tri: Triangle,
	n_constraints: int,
	has_bound_edge: bool,
	is_curve: bool,
) -> TriangleType {
	if has_bound_edge do return .bounding
	switch n_constraints {
	case 0:
		a := transmute([2]u16)get_vert(u16(tri[0]))
		b := transmute([2]u16)get_vert(u16(tri[1]))
		c := transmute([2]u16)get_vert(u16(tri[2]))
		ac2 := la.dot(c - a, c - a)
		ab2 := la.dot(b - a, b - a)
		bc2 := la.dot(c - b, c - b)
		acute := ac2 < ab2 + bc2
		if acute do return .junction_a
		else do return .junction_o
	case 1:
		return .sleeve
	case 2:
		return .terminal
	case 3:
		if is_curve do return .curve
		return .hole
	case:
		return .bounding
	}
}

append_subtriangles :: proc(
	all_verts: ^[dynamic]Vec2f,
	dest_dyn: ^[dynamic]Triangle,
	uvs_dyn: ^[dynamic]TriangleUV,
	triangle: Triangle,
	type: TriangleType,
	constraint_edges: []int,
	midpoint_indices: []u16,
) {
	end_contours := state.end_contours
	on_curve := state.on_curve

	full_uv := TriangleUV{{{0, 1}, true}, {{0, 1}, true}, {{0, 1}, true}}
	edge_uv := TriangleUV{{{0, 0}, true}, {{0, 0}, true}, {{1, 0}, true}}

	switch type {
	case .terminal:
		assert(len(constraint_edges) == 2)
		assert(len(midpoint_indices) == 1)

		midpoint_edge_idx := 3 - constraint_edges[0] - constraint_edges[1]
		opposite_vert := triangle[(midpoint_edge_idx + 2) % 3]
		adj_vert_1 := triangle[midpoint_edge_idx]
		adj_vert_2 := triangle[(midpoint_edge_idx + 1) % 3]
		triangles := [2]Triangle {
			{adj_vert_2, opposite_vert, u32(midpoint_indices[0])},
			{opposite_vert, adj_vert_1, u32(midpoint_indices[0])},
		}
		uvs := [2]TriangleUV{edge_uv, edge_uv}

		append(dest_dyn, ..triangles[:])
		append(uvs_dyn, ..uvs[:])

	case .sleeve:
		assert(len(constraint_edges) == 1)
		assert(len(midpoint_indices) == 2)

		opp_vert_idx := 3 - constraint_edges[0] - (constraint_edges[0] + 1) % 3
		opp := triangle[opp_vert_idx]
		sleeve_vert_1 := triangle[(opp_vert_idx + 1) % 3]
		sleeve_vert_2 := triangle[(opp_vert_idx + 2) % 3]
		m1, m2: u32
		if opp_vert_idx == 0 {
			m1 = u32(midpoint_indices[0])
			m2 = u32(midpoint_indices[1])
		} else {
			m1 = u32(midpoint_indices[1])
			m2 = u32(midpoint_indices[0])
		}

		triangles := [3]Triangle {
			{u32(m1), sleeve_vert_1, sleeve_vert_2},
			{u32(m1), sleeve_vert_2, u32(m2)},
			{opp, u32(m1), u32(m2)},
		}
		uvs := [3]TriangleUV{full_uv, full_uv, full_uv}

		append(dest_dyn, ..triangles[:])
		append(uvs_dyn, ..uvs[:])

	case .junction_o, .junction_a:
		assert(len(constraint_edges) == 0)
		assert(len(midpoint_indices) == 3)
		a := triangle[0]
		b := triangle[1]
		c := triangle[2]

		m1 := u32(midpoint_indices[0])
		m2 := u32(midpoint_indices[1])
		m3 := u32(midpoint_indices[2])

		triangles := [4]Triangle {
			{m1, m2, m3},
			{m1, b, m2},
			{m2, c, m3},
			{m3, a, m1},
		}
		uvs := [4]TriangleUV{full_uv, full_uv, full_uv, full_uv}

		append(dest_dyn, ..triangles[:])
		append(uvs_dyn, ..uvs[:])

	case .hole:
		log.debug("hole")
		assert(len(constraint_edges) == 3)
		assert(len(midpoint_indices) == 0)
		a := triangle[0]
		b := triangle[1]
		c := triangle[2]

		centroid := Vertex {
			get_vert(u16(a)).x + get_vert(u16(b)).x + get_vert(u16(c)).x,
			get_vert(u16(a)).y + get_vert(u16(b)).y + get_vert(u16(c)).y,
		}
		centroid.x /= 3
		centroid.y /= 3

		cent := u32(append_vertex(all_verts, centroid))

		triangles := [3]Triangle{{a, b, cent}, {b, c, cent}, {c, a, cent}}
		uvs := [3]TriangleUV{full_uv, full_uv, full_uv}

		append(dest_dyn, ..triangles[:])
		append(uvs_dyn, ..uvs[:])

	case .bounding:
		append(dest_dyn, triangle)
		append(uvs_dyn, full_uv)
	case .curve:
		off_curve_vert := 0
		for on_curve[triangle[off_curve_vert]] do off_curve_vert += 1

		next := get_next_vert(end_contours, u16(triangle[off_curve_vert]))

		inner := true
		if triangle[(off_curve_vert + 1) % 3] == u32(next) do inner = false
		before := inner ? off_curve_vert + 2 : off_curve_vert + 1
		before %= 3
		after := inner ? off_curve_vert + 1 : off_curve_vert + 2
		after %= 3

		uvs: TriangleUV
		uvs[before] = {{0, 0}, b64(inner)}
		uvs[off_curve_vert] = {{0.5, 0}, b64(inner)}
		uvs[after] = {{1, 1}, b64(inner)}

		append(dest_dyn, triangle)
		append(uvs_dyn, uvs)
	}
}

chordal_edge_triangulation :: proc(
	all_verts: ^[dynamic]Vec2f,
	dest_dyn: ^[dynamic]Triangle,
	uvs_dyn: ^[dynamic]TriangleUV,
) {
	edge_flags := make([]EdgeFlags, state.next_edge, context.temp_allocator)

	main_loop: for i := 0; i < int(state.next_edge); i += 2 {
		e := state.edges[i]
		if i == int(state.available_edge) {
			state.available_edge = o_next(state.available_edge)
			continue
		}

		half_edge: for _ in 0 ..< 2 {
			defer e = sym(e)
			if .visited_inside in edge_flags[e] do continue

			current_edge := e
			new_tri: Triangle

			constraint_edges := [3]int{}
			n_constraints := 0
			midpoint_indices := [3]u16{}
			n_midpoints := 0

			has_bound_edge := false

			is_curve := false
			off_curve_idx := 0
			n_consecutive := 0

			triangle: for tri_idx in 0 ..< 3 {
				defer current_edge = l_next(current_edge)
				edge_flags[current_edge] += {.visited_inside}

				current_origin := real_orig(current_edge)
				current_dest := real_dest(current_edge)

				new_tri[tri_idx] = u32(current_origin)

				if is_constraint_edge(current_edge) {
					constraint_edges[n_constraints] = tri_idx
					n_constraints += 1
				} else if .midpoint_added not_in edge_flags[current_edge] {
					a := get_vert(current_origin)
					b := get_vert(current_dest)
					midpoint: Vertex = {(a.x + b.x) / 2, (a.y + b.y) / 2}
					midpoint_indices[n_midpoints] = u16(
						append_vertex(all_verts, midpoint),
					)
					n_midpoints += 1
					edge_flags[current_edge] += {.midpoint_added}
				}

				if is_bound_edge(state.on_curve, current_edge) {
					has_bound_edge = true
				} else if !state.on_curve[current_origin] {
					off_curve_idx = tri_idx
					is_curve = true
				}

				{
					orig_next := get_next_vert(
						state.end_contours,
						current_origin,
					)
					dest_next := get_next_vert(
						state.end_contours,
						current_dest,
					)
					if orig_next == current_dest || dest_next == orig_next {
						n_consecutive += 1
					}
				}
			}

			if e != current_edge do continue

			if is_curve {
				b1 := u16(new_tri[off_curve_idx])
				s, e := get_contour(state.end_contours, b1)
				b2 := get_next_vert(s, e, b1)

				other := u16(new_tri[(off_curve_idx + 1) % 3])

				if other == b2 do other = u16(new_tri[(off_curve_idx + 2) % 3])

				if get_next_vert(s, e, other) != b1 do is_curve = false
			}

			type := triangle_type(
				new_tri,
				n_constraints,
				has_bound_edge,
				is_curve,
			)
			append_subtriangles(
				all_verts,
				dest_dyn,
				uvs_dyn,
				new_tri,
				type,
				constraint_edges[:n_constraints],
				midpoint_indices[:n_midpoints],
			)
		}
	}
}

build_uv :: proc(t: Triangle, inside: bool) -> (uv: TriangleUV) {
	a := t[0]
	b := t[1]
	c := t[2]
	draw_full: [3]bool = {
		int(a) >= len(state.on_curve) || !state.on_curve[a],
		int(b) >= len(state.on_curve) || !state.on_curve[b],
		int(c) >= len(state.on_curve) || !state.on_curve[c],
	}
	uv = {
		{{0, draw_full[0] ? 1 : 0}, b64(inside)},
		{{0, draw_full[1] ? 1 : 0}, b64(inside)},
		{{0, draw_full[2] ? 1 : 0}, b64(inside)},
	}
	return
}

append_subtriangles_2 :: proc(
	all_verts: ^[dynamic]Vec2f,
	dest_dyn: ^[dynamic]Triangle,
	uvs_dyn: ^[dynamic]TriangleUV,
	triangle: Triangle,
	type: TriangleType,
	inside: bool,
	at_border: [3]bool,
	constraint_edges: []int,
	midpoint_indices: []u16,
) {
	end_contours := state.end_contours
	on_curve := state.on_curve

	full_uv := TriangleUV{{{0, 1}, true}, {{0, 1}, true}, {{0, 1}, true}}

	// border, border, inside 
	edge_uv :: proc(flag: bool) -> TriangleUV {
		return TriangleUV {
			{{0, 0}, b64(flag)},
			{{0, 0}, b64(flag)},
			{{0, 1}, b64(flag)},
		}
	}

	// inside, inside, border 
	vert_uv :: proc(flag: bool) -> TriangleUV {
		return TriangleUV {
			{{0, 1}, b64(flag)},
			{{0, 1}, b64(flag)},
			{{0, 0}, b64(flag)},
		}
	}

	switch type {
	case .terminal:
		assert(len(constraint_edges) == 2)
		assert(len(midpoint_indices) == 1)

		midpoint_edge_idx := 3 - constraint_edges[0] - constraint_edges[1]
		opp_idx := (midpoint_edge_idx + 2) % 3
		adj_vert_1_idx := midpoint_edge_idx
		adj_vert_2_idx := (midpoint_edge_idx + 1) % 3

		opp := triangle[opp_idx]
		adj_vert_1 := triangle[adj_vert_1_idx]
		adj_vert_2 := triangle[adj_vert_2_idx]


		triangles := [2]Triangle {
			{adj_vert_2, opp, u32(midpoint_indices[0])},
			{opp, adj_vert_1, u32(midpoint_indices[0])},
		}

        // uvs := [2]TriangleUV{}
        // for t, i in triangles {
        //     uvs[i] = build_uv(t, inside)
        // }

		uvs := [2]TriangleUV {
			at_border[adj_vert_2_idx] ? edge_uv(inside) : full_uv,
			at_border[opp_idx] ? edge_uv(inside) : full_uv,
		}

		append(dest_dyn, ..triangles[:])
		append(uvs_dyn, ..uvs[:])

	case .sleeve:
		assert(len(constraint_edges) == 1)
		assert(len(midpoint_indices) == 2)

		opp_idx := 3 - constraint_edges[0] - (constraint_edges[0] + 1) % 3
		sleeve_1_idx := (opp_idx + 1) % 3
		sleeve_2_idx := (opp_idx + 2) % 3

		opp := triangle[opp_idx]
		sleeve_1 := triangle[sleeve_1_idx]
		sleeve_2 := triangle[sleeve_2_idx]

		m1, m2: u32
		if opp_idx == 0 {
			m1 = u32(midpoint_indices[0])
			m2 = u32(midpoint_indices[1])
		} else {
			m1 = u32(midpoint_indices[1])
			m2 = u32(midpoint_indices[0])
		}

		triangles := [3]Triangle{
			{sleeve_1, sleeve_2, u32(m1)},
			{u32(m2), u32(m1), sleeve_2},
			{u32(m1), u32(m2), opp},
		}
		// uvs := [3]TriangleUV{}
        // for t, i in triangles {
            // uvs[i] = build_uv(t, inside)
        // }

		// 	{sleeve_1, sleeve_2, u32(m1)},
		// 	{u32(m2), u32(m1), sleeve_2},
		// 	{u32(m1), u32(m2), opp},

        uvs := [3]TriangleUV {
			at_border[sleeve_1_idx] ? edge_uv(inside) : full_uv,
			full_uv,
			full_uv,
		}

		append(dest_dyn, ..triangles[:])
		append(uvs_dyn, ..uvs[:])

	case .junction_o, .junction_a:
		assert(len(constraint_edges) == 0)
		assert(len(midpoint_indices) == 3)
		a := triangle[0]
		b := triangle[1]
		c := triangle[2]

		m1 := u32(midpoint_indices[0])
		m2 := u32(midpoint_indices[1])
		m3 := u32(midpoint_indices[2])

		triangles := [4]Triangle {
			{m1, m2, m3},
			{m1, b, m2},
			{m2, c, m3},
			{m3, a, m1},
		}
		uvs := [4]TriangleUV{full_uv, full_uv, full_uv, full_uv}

		append(dest_dyn, ..triangles[:])
		append(uvs_dyn, ..uvs[:])

	case .hole:
		log.debug("hole")
		assert(len(constraint_edges) == 3)
		assert(len(midpoint_indices) == 0)
		a := triangle[0]
		b := triangle[1]
		c := triangle[2]

		centroid := Vertex {
			get_vert(u16(a)).x + get_vert(u16(b)).x + get_vert(u16(c)).x,
			get_vert(u16(a)).y + get_vert(u16(b)).y + get_vert(u16(c)).y,
		}
		centroid.x /= 3
		centroid.y /= 3

		cent := u32(append_vertex(all_verts, centroid))

		triangles := [3]Triangle{{a, b, cent}, {b, c, cent}, {c, a, cent}}
		uvs := [3]TriangleUV{full_uv, full_uv, full_uv}

		append(dest_dyn, ..triangles[:])
		append(uvs_dyn, ..uvs[:])

	case .bounding:
		append(dest_dyn, triangle)
		append(uvs_dyn, full_uv)
	case .curve:
		off_curve_vert := 0
		for on_curve[triangle[off_curve_vert]] do off_curve_vert += 1

		next := get_next_vert(end_contours, u16(triangle[off_curve_vert]))

		inner := true
		if triangle[(off_curve_vert + 1) % 3] == u32(next) do inner = false
		before := inner ? off_curve_vert + 2 : off_curve_vert + 1
		before %= 3
		after := inner ? off_curve_vert + 1 : off_curve_vert + 2
		after %= 3

		uvs: TriangleUV
		uvs[before] = {{0, 0}, b64(inner)}
		uvs[off_curve_vert] = {{0.5, 0}, b64(inner)}
		uvs[after] = {{1, 1}, b64(inner)}

		append(dest_dyn, triangle)
		append(uvs_dyn, uvs)
	}
}

edge_is_at_border :: proc(
	e: EdgePtr,
	next_in_face: proc(_: EdgePtr) -> EdgePtr,
) -> bool {
	esym := sym(e)
	for k in 0 ..< 3 {
		defer esym = next_in_face(esym)
		if !is_constraint_edge(esym) do return true
	}

	return false
}

append_triangles_2 :: proc(
	all_verts: ^[dynamic]Vec2f,
	dest_dyn: ^[dynamic]Triangle,
	uvs_dyn: ^[dynamic]TriangleUV,
	edge_flags: ^[]EdgeFlags,
	midpoints: ^[]u16,
	start_edge: EdgePtr,
	inside: bool,
) {
	next_edge := o_next
	next_edge_in_face := l_next
	if inside {
		next_edge = o_prev
		next_edge_in_face = r_prev
	}

	first := true
	current_edge := start_edge
	running := true
	log.info("inside:", inside)
	for running {
		log.info(
			"appending at:",
			real_orig(current_edge),
			real_dest(current_edge),
		)
		defer {
			current_edge = next_edge(current_edge)
			if is_constraint_edge(current_edge) {
				log.info(
					"edge is constraint",
					real_orig(current_edge),
					real_dest(current_edge),
				)
				running = false
			}
		}
		defer first = false

		if inside && .visited_inside in edge_flags[current_edge] do continue
		if !inside && .visited_outside in edge_flags[current_edge] do continue

		triangle_constraint_edge := current_edge

		is_curve := false
		off_curve_idx := 0

		at_border := [3]bool{}

		constraint_edges := [3]int{}
		n_constraint_edges := 0
		midpoint_indices := [3]u16{}
		n_midpoints := 0

		has_bound_edge := false

		new_uv: TriangleUV = {{{0, 1}, true}, {{0, 1}, true}, {{0, 1}, true}}

		new_tri: Triangle
		prev_vert := real_orig(current_edge)
		for k in 0 ..< 3 {
			defer current_edge = next_edge_in_face(current_edge)

			if is_bound_edge(state.on_curve, current_edge) {
				has_bound_edge = true
			} else {
				at_border[k] = edge_is_at_border(
					current_edge,
					next_edge_in_face,
				)

				if first && !state.on_curve[real_orig(current_edge)] {
					is_curve = true
					off_curve_idx = k
				}
			}

			current_origin := real_orig(current_edge)
			current_dest := real_dest(current_edge)

			edge_flags[current_edge] +=
				inside ? {.visited_inside} : {.visited_outside}
			new_tri[k] = u32(current_origin)

			if is_constraint_edge(current_edge) {
				constraint_edges[n_constraint_edges] = k
				n_constraint_edges += 1
			} else {
				if .midpoint_added not_in edge_flags[current_edge] {
					a := get_vert(current_origin)
					b := get_vert(current_dest)
					midpoint: Vertex = {(a.x + b.x) / 2, (a.y + b.y) / 2}
					midpoints[current_edge] = u16(
						append_vertex(all_verts, midpoint),
					)
					midpoints[sym(current_edge)] = midpoints[current_edge]
					edge_flags[current_edge] += {.midpoint_added}
					edge_flags[sym(current_edge)] += {.midpoint_added}
				}

				midpoint_indices[n_midpoints] = midpoints[current_edge]
				n_midpoints += 1
			}

		}
		assert(current_edge == triangle_constraint_edge)

		inner_edge := false
		{ 	// verify is_curve
			if off_curve_idx == 2 do inner_edge = true
			if off_curve_idx == 0 do is_curve = false
			if first {
				a := u16(new_tri[0])
				b := u16(new_tri[1])
				c := u16(new_tri[2])
				a_s, a_e := get_contour(state.end_contours, a)
				b_s, b_e := get_contour(state.end_contours, b)
				c_s, c_e := get_contour(state.end_contours, c)
				consecutive :=
					!inner_edge &&
						(get_next_vert(a_s, a_e, a) == b &&
									get_next_vert(b_s, b_e, b) == c ||
								get_next_vert(b_s, b_e, b) == c &&
									get_next_vert(c_s, c_e, c) == a ||
								get_next_vert(c_s, c_e, c) == a &&
									get_next_vert(a_s, a_e, a) == b) ||
					inner_edge &&
						(get_next_vert(a_s, a_e, a) == c &&
									get_next_vert(c_s, c_e, c) == b ||
								get_next_vert(b_s, b_e, b) == a ||
								get_next_vert(a_s, a_e, a) == c &&
									get_next_vert(c_s, c_e, c) == b &&
									get_next_vert(b_s, b_e, b) == a)
				if !consecutive do is_curve = false
			}
		}
		log.debug("inner_edge", inner_edge)
		log.debug("is_curve", is_curve)
		log.debug("n_constraints", n_constraint_edges)

		type := triangle_type(
			new_tri,
			n_constraint_edges,
			has_bound_edge,
			is_curve,
		)
		log.debug("type", type)

		#partial switch type {
		case .curve:
			assert(is_curve)
			after := inner_edge ? off_curve_idx + 1 : off_curve_idx + 2
			after %%= 3
			before := inner_edge ? off_curve_idx + 2 : off_curve_idx + 1
			before %%= 3

			new_uv[before].uv = {0, 0}
			new_uv[off_curve_idx].uv = {0.5, 0}
			new_uv[after].uv = {1, 1}
			if inner_edge {
				new_uv[0].z = false
				new_uv[1].z = false
				new_uv[2].z = false
			}

			append(dest_dyn, new_tri)
			append(uvs_dyn, new_uv)
		case:
			append_subtriangles_2(
				all_verts,
				dest_dyn,
				uvs_dyn,
				new_tri,
				type,
				inside,
				at_border,
				constraint_edges[:n_constraint_edges],
				midpoint_indices[:n_midpoints],
			)
		}
	}
}

chordal_edge_triangulation_2 :: proc(
	all_verts: ^[dynamic]Vec2f,
	dest_dyn: ^[dynamic]Triangle,
	uvs_dyn: ^[dynamic]TriangleUV,
) {
	edge_flags := make([]EdgeFlags, state.next_edge)
	defer delete(edge_flags)
	midpoints := make([]u16, state.next_edge)
	defer delete(midpoints)

	c_start: u16 = 0
	for c_end in state.end_contours {
		defer c_start = c_end + 1
		// log.info(c_start, c_end)
		for i := c_start; i <= c_end; i += 1 {
			if state.removed[i] do continue
			next_vert := get_next_vert(c_start, c_end, i)
			for state.removed[next_vert] {
				next_vert = get_next_vert(c_start, c_end, next_vert)
			}
			constraint_edge := get_edge(i, next_vert)
			assert(constraint_edge < state.next_edge)

			log.info("-------------", i, next_vert, "-------------")
			// Append valid trianges 
			// (the ones to the right of the constraint edge)
			// if !visited_edges[constraint_edge] {
			append_triangles_2(
				all_verts,
				dest_dyn,
				uvs_dyn,
				&edge_flags,
				&midpoints,
				constraint_edge,
				inside = true,
			)

			// }

			if !state.on_curve[i] {
				continue
			} else if state.on_curve[next_vert] {
				// append_triangles_2(
				// 	all_verts,
				// 	dest_dyn,
				// 	uvs_dyn,
				// 	&edge_flags,
				// 	&midpoints,
				// 	constraint_edge,
				// 	inside = false,
				// )
				log.info("here")
				continue
			}

			leftmost := constraint_edge

			next_next_vert := get_next_vert(c_start, c_end, next_vert)
			for state.removed[next_next_vert] {
				next_next_vert = get_next_vert(c_start, c_end, next_next_vert)
			}

			if collinear(i, next_vert, next_next_vert) do continue

			constraint_edge = get_edge(i, next_next_vert)

			if o_next(leftmost) == constraint_edge {
				leftmost = constraint_edge
			}

			// if !visited_edges[constraint_edge] {
			append_triangles_2(
				all_verts,
				dest_dyn,
				uvs_dyn,
				&edge_flags,
				&midpoints,
				constraint_edge,
				inside = true,
			)

			// append_triangles_2(
			// 	all_verts,
			// 	dest_dyn,
			// 	uvs_dyn,
			// 	&edge_flags,
			// 	&midpoints,
			// 	leftmost,
			// 	inside = false,
			// )
			// }
		}
	}
}


version :: #config(V, 4)

triangulate_vertices_w_inout :: proc(
	vertices: #soa[]Vertex,
	end_contours: []u16,
	on_curve: []bool,
	all_verts: ^[dynamic]Vec2f,
	triangles: ^[dynamic]Triangle,
	uvs: ^[dynamic]TriangleUV,
) {
	state.vert_count = len(vertices)
	if state.vert_count == 0 do return

	{ 	// init state
		n := 0
		for v, i in vertices {
			state.vertices[i] = v
			n += 1
		}
		assert(n == state.vert_count)

		add_bounding_box(all_verts)

		init_mappings()
	}

	{ 	// sort vertices
		quick_sort(0, state.vert_count - 1)
		state.removed = remove_duplicates()
	}
	defer delete(state.removed)

	le, re := delaunay(state.mappings)

	apply_constraints(end_contours, on_curve)

	when ODIN_DEBUG {
		log.debug("mappings", state.mappings)
		log.debug("edges", state.edges[:state.next_edge])
		log.debug("data", state.data[:state.next_edge / 2])
		log.debug("edge_mapp", state.origin_to_edge_mapping)
		log.debug("next_avail", state.available_edge)
		log.debug("end_contours", end_contours)
	}

	state.on_curve = on_curve
	state.end_contours = end_contours

	when version == 1 {
		traverse_triangles(triangles, uvs, end_contours, on_curve)
	} else when version == 2 {
		traverse_triangles_2(triangles, uvs, end_contours, on_curve)
	} else when version == 3 {
		chordal_edge_triangulation(all_verts, triangles, uvs)
	} else when version == 4 {
		chordal_edge_triangulation_2(all_verts, triangles, uvs)
	}
}

triangulate_vertices :: proc {
	triangulate_vertices_w_inout,
}

import "core:fmt"
import "core:log"
import "core:os"
import "core:testing"

import ttf "../TTFonting"

@(test)
mem_test :: proc(_: ^testing.T) {
	glyfs, _, n := ttf.parse_ttf(
		"../assets/fonts/JetBrains/JetBrainsMono-Regular.ttf",
	)
	load_glyphs(glyfs, n)
}
