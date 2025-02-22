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

/*
@(private = "file")
State :: struct($E: u32, $V: u32) where E < 1000,
	V < 200 {
	edges:          [E * 4]EdgePtr,
	data:           [E]VertPtr,
	vertices:       [V]Vec2f,
	next_edge:      EdgePtr,
	available_edge: EdgePtr,
}
*/

@(private = "file")
State :: struct {
	edges:                  []EdgePtr,
	data:                   []VertPtr, // points to the mappings
	next_edge:              EdgePtr,
	available_edge:         EdgePtr,
	vertices:               #soa[]Vertex,
	mappings:               []u16,
	origin_to_edge_mapping: []EdgePtr,
}

// @(private = "file")
@(private)
state: State

@(require_results)
get_vert :: #force_inline proc(v: VertPtr) -> Vertex {
	return state.vertices[state.mappings[v]]
	// return state.vertices[v]
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
	state.origin_to_edge_mapping[state.mappings[orig(e)^]] = e
	state.origin_to_edge_mapping[state.mappings[dest(e)^]] = sym(e)

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
	state.origin_to_edge_mapping[state.mappings[orig(e)^]] = e
	state.origin_to_edge_mapping[state.mappings[dest(e)^]] = sym(e)
	state.origin_to_edge_mapping[state.mappings[orig(a)^]] = a
	state.origin_to_edge_mapping[state.mappings[orig(b)^]] = b
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

init_mappings :: proc() {
	for &m, i in state.mappings do m = u16(i)
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

// @(test)
// test_sort :: proc(_: ^testing.T) {
// 	n_vertices := 5
// 	state.vertices = make([]Vec2f, n_vertices)
// 	defer delete_slice(state.vertices)
// 	state.vertices = {{1, 1}, {0, 1}, {1, 0}, {3, 2}, {2, 1}}
// 	delete_slice(state.vertices)
// 	fmt.println(state.vertices)
// 	quick_sort(0, n_vertices - 1)
// 	fmt.println(state.vertices)
// }

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
	if vert == end do return start
	return vert + 1
}

get_next_vert :: proc {
	get_next_vert_wo_contour,
	get_next_vert_wi_contour,
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

				current_origin := state.mappings[orig(current_edge)^]
				current_dest := state.mappings[dest(current_edge)^]

				new_tri[tri_idx] = u32(current_origin)
			}

			if e != current_edge do continue

			should_cut: bool
			{ 	// Mastermined by me, can you tell? 
				a := u16(new_tri[0])
				b := u16(new_tri[1])
				c := u16(new_tri[2])


				start_a, end_a := get_contour(end_contours, a)
				start_b, end_b := get_contour(end_contours, b)
				start_c, end_c := get_contour(end_contours, c)
				same_contour := end_a == end_b && end_a == end_c
				s := start_a
				e := end_a

				cw_from_a :=
					get_next_vert(s, e, a) == c && get_next_vert(s, e, c) == b
				cw_from_b :=
					get_next_vert(s, e, b) == a && get_next_vert(s, e, a) == c
				cw_from_c :=
					get_next_vert(s, e, c) == b && get_next_vert(s, e, b) == a
				outer_edge := cw_from_a || cw_from_b || cw_from_c // cw orientation 

				ccw_from_a :=
					get_next_vert(s, e, a) == b && get_next_vert(s, e, b) == c
				ccw_from_b :=
					get_next_vert(s, e, b) == c && get_next_vert(s, e, c) == a
				ccw_from_c :=
					get_next_vert(s, e, c) == a && get_next_vert(s, e, a) == b
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
					after := inner_edge ? off_curve_idx + 1 : off_curve_idx + 2
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

			// new_uv = {{{0, 1}, true}, {{0, 1}, true}, {{0, 1}, true}}

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
					state.mappings[orig(a_edge)^],
					state.mappings[dest(a_edge)^],
				)
			}
			return
		}
		a_edge = o_next(a_edge)
	}
	when ODIN_DEBUG {
		log.debug(
			"found_pos",
			state.mappings[orig(a_edge)^],
			state.mappings[dest(a_edge)^],
		)
	}

	aux := l_next(a_edge)
	intersecting_queue := make([dynamic]EdgePtr, context.temp_allocator)

	j := 0
	main_loop: for dest(aux)^ != b_vert_ptr &&
	    dest(o_next(aux))^ != b_vert_ptr {
		defer j += 1
		if j > 100 do break

		aux_orig := get_vert(orig(aux)^)
		aux_dest := get_vert(dest(aux)^)
		k := 0
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
			log.info(
				"[",
				i,
				"]",
				state.mappings[orig(inter)^],
				state.mappings[dest(inter)^],
			)
		}
	}

	i := 0
	for len(intersecting_queue) != 0 {
		defer i = (i + 1) % len(intersecting_queue)

		e := intersecting_queue[i]
		swap(e)
		e_origin := get_vert(orig(e)^)
		e_dest := get_vert(dest(e)^)

		if (orig(e)^ == a_vert_ptr && dest(e)^ == b_vert_ptr) ||
		   (orig(e)^ == b_vert_ptr && dest(e)^ == a_vert_ptr) ||
		   !intersects(a_vert, b_vert, e_origin, e_dest) {
			unordered_remove(&intersecting_queue, i)
			i -= 1
		}
		if len(intersecting_queue) == 0 do break
	}
}

apply :: #config(APPLY, true)
@(disabled = !apply)
apply_constraints :: proc(end_contours: []u16, on_curve: []bool) {
	c_start: u16 = 0
	for c_end in end_contours {
		defer c_start = c_end + 1
		for i := c_start; i <= c_end; i += 1 {
			next_vert := get_next_vert(c_start, c_end, i)
			apply_constraint(i, next_vert)
			if !on_curve[next_vert] {
				apply_constraint(i, get_next_vert(c_start, c_end, next_vert))
			}
		}
	}
}

triangulate_vertices :: proc(
	vertices: #soa[]Vertex,
	end_contours: []u16,
	on_curve: []bool,
) -> (
	triangles: []Triangle,
	uvs: []TriangleUV,
) {
	{ 	// init state
		state.vertices = vertices
		state.mappings = make([]u16, len(vertices))
		init_mappings()

		n_edges := len(vertices) * len(vertices)

		state.edges = make([]EdgePtr, n_edges * 4)
		state.data = make([]EdgePtr, n_edges * 2)
		state.origin_to_edge_mapping = make([]EdgePtr, len(vertices))

		state.next_edge = 0
		state.available_edge = NIL
	}

	defer { 	// de-init state
		delete(state.vertices)
		delete(state.mappings)
		delete(state.edges)
		delete(state.data)
		delete(state.origin_to_edge_mapping)
	}

	{ 	// sort vertices
		quick_sort(0, len(vertices) - 1)

		when ODIN_DEBUG {
			log.debug(state.mappings)
			for m, i in state.mappings {
				log.debug(
					"mappings[",
					i,
					"]=",
					m,
					"vertices[",
					i,
					"]=",
					state.vertices[m],
				)

			}
		}
	}

	le, re := delaunay(state.mappings)

	apply_constraints(end_contours, on_curve)

	when ODIN_DEBUG {
		log.debug("mappings", state.mappings)
		log.debug("edges", state.edges[:state.next_edge])
		log.debug("data", state.data[:state.next_edge / 2])
		log.debug("edge_mapp", state.origin_to_edge_mapping)
		log.debug("next_avail", state.available_edge)
	}

	triangles_dyn := make([dynamic]Triangle)
	uvs_dyn := make([dynamic]TriangleUV)
	traverse_triangles(&triangles_dyn, &uvs_dyn, end_contours, on_curve)
	resize_dynamic_array(&triangles_dyn, len(triangles_dyn))
	resize_dynamic_array(&uvs_dyn, len(triangles_dyn))

	return triangles_dyn[:], uvs_dyn[:]
}

import "core:fmt"
import "core:log"
import "core:os"
import "core:testing"

@(test)
math_test :: proc(t: ^testing.T) {
	a, b: i128
	test_values := [?][3]i128 {
		{110, 0, 12100},
		{110, 730, 545000},
		{160, 46, 27716},
		{160, 648, 445504},
	}

	for values in test_values {
		x := values.x
		y := values.y
		z := values.z
		log.infof("testing: %d * %d + %d * %d == %d", x, x, y, y, z)
		ok := testing.expect(t, x * x + y * y == z)
		if !ok {
			log.errorf("%d * %d + %d * %d == %d", x, x, y, y, x * x + y * y)
		}
	}

	{
		matrix_value: matrix[4, 4]i128
		matrix_value[0] = {110, 110, 160, 160}
		matrix_value[1] = {0, 730, 46, 648}
		matrix_value[2] = {12100, 545000, 27716, 445504}
		matrix_value[3] = {1, 1, 1, 1}
		log.info(matrix_value)
		log.info(la.transpose(matrix_value))
		log.info(la.determinant(matrix_value))
	}

	{
		matrix_value: matrix[4, 4]i128
		matrix_value[0] = {110, 0, 12100, 1}
		matrix_value[1] = {110, 730, 545000, 1}
		matrix_value[2] = {160, 46, 27716, 1}
		matrix_value[3] = {160, 684, 445504, 1}
		log.info(matrix_value)
		log.info(la.determinant(matrix_value))
	}

	{
		matrix_3_3: matrix[3, 3]i128
		matrix_3_3[0] = {110, 0, 12100}
		matrix_3_3[1] = {110, 730, 545000}
		matrix_3_3[2] = {160, 46, 27716}
		matrix_3_2: matrix[3, 3]i128
		matrix_3_2[0] = {110, 0, 12100}
		matrix_3_2[1] = {110, 730, 545000}
		matrix_3_2[2] = {160, 684, 493456}
		matrix_3_1: matrix[3, 3]i128
		matrix_3_1[0] = {110, 0, 12100}
		matrix_3_1[1] = {160, 46, 27716}
		matrix_3_1[2] = {160, 684, 493456}
		matrix_3_0: matrix[3, 3]i128
		matrix_3_0[0] = {110, 730, 545000}
		matrix_3_0[1] = {160, 46, 27716}
		matrix_3_0[2] = {160, 684, 493456}

		d0 := la.determinant(matrix_3_0)
		d1 := la.determinant(matrix_3_1)
		d2 := la.determinant(matrix_3_2)
		d3 := la.determinant(matrix_3_3)
		log.info(d0, d1, d2, d3)
		log.info(d0 - d1 + d2 - d3)
	}

	{
		log.info(incircle_fast({{110, 0}, {110, 730}, {160, 46}, {160, 684}}))
	}

}
