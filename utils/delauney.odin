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

MAX_VERTICES :: 200
MAX_EDGES :: (MAX_VERTICES - 1) * 3
NIL: EdgePtr : -1
EPSILON :: 0.01

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
	edges:          []EdgePtr,
	data:           []VertPtr,
	next_edge:      EdgePtr,
	available_edge: EdgePtr,
	vertices:       []Vec2f,
	mappings:       []int,
}

// @(private = "file")
@(private)
state: State

@(require_results)
get_vert :: #force_inline proc(v: VertPtr) -> Vec2f {
	// return state.vertices[state.mappings[v]]
	return state.vertices[v]
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
	e := make_edge(a, b)
	orig(e)^ = dest(a)^
	dest(e)^ = orig(b)^

	splice(e, l_next(a))
	splice(sym(e), b)
	return e
}

delete_edge :: proc(e: EdgePtr) {
	splice(e, o_prev(e))
	splice(sym(e), o_prev(sym(e)))
	free_edge(e)
}

swap :: proc(e: EdgePtr) {
	a := o_prev(e)
	b := o_prev(sym(e))
	splice(e, a)
	splice(sym(e), b)
	splice(e, l_next(a))
	splice(sym(e), l_next(b))

	orig(e)^ = orig(l_next(a))^
	// Note(fede): should not have to change the dest value since it is the same (i think)
	dest(e)^ = orig(l_next(b))^
}

import "core:math/linalg"

/*
import "base:intrinsics"
import "core:simd"

incircle_simd :: proc(pts: [4]Vec2f) {
	pts_simd_ptr: ^simd.f32x8 = cast(^#simd[8]f32)raw_data(pts)
	pts_simd: simd.f32x8 = intrinsics.unaligned_load(pts_simd_ptr)

    simd.mul(pts_simd, pts_simd)


	linalg.determinant(
		Mat4({A.x, B.x, C.x, D.x, A.y, B.y, C.y, D.y, A.y, B.y, C.y, D.y}),
	)
}
*/

@(require_results)
incircle_vec2 :: proc(pts: [4]Vec2f) -> bool {
	x2_y2: [4]f32 = {}
	for &v, i in x2_y2 {
		p := pts[i] * pts[i]
		v = p.x + p.y
	}

	m: Mat4
	m[0] = {pts[0].x, pts[1].x, pts[2].x, pts[3].x}
	m[1] = {pts[0].y, pts[1].y, pts[2].y, pts[3].y}
	m[2] = x2_y2
	m[3] = {1, 1, 1, 1}
	d := linalg.determinant(m)
	return d > EPSILON
}

@(require_results)
incircle_ptr :: proc(pts: [4]VertPtr) -> bool {
	pts_vec2: [4]Vec2f
	for &p, i in pts_vec2 {
		p = get_vert(pts[i])
		// state.vertices[state.mappings[pts[i]]]
	}
	return incircle_vec2(pts_vec2)
}

@(require_results)
incircle_expand_ptr :: #force_inline proc(p1, p2, p3, p4: VertPtr) -> bool {
	return incircle_ptr({p1, p2, p3, p4})
}

@(require_results)
ccw_vec2 :: proc(a, b, c: Vec2f) -> bool {
	m: Mat3
	m[0] = {a.x, b.x, c.x}
	m[1] = {a.y, b.y, c.y}
	m[2] = {1, 1, 1}
	return linalg.determinant(m) > EPSILON
}

@(require_results)
ccw_ptr :: proc(A, B, C: VertPtr) -> bool {
	a := get_vert(A)
	b := get_vert(B)
	c := get_vert(C)
	// a := state.vertices[state.mappings[A]]
	// b := state.vertices[state.mappings[B]]
	// c := state.vertices[state.mappings[C]]
	return ccw_vec2(a, b, c)
}

@(require_results)
right_of_vec2 :: proc(x: Vec2f, e: EdgePtr) -> bool {
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
left_of_vec2 :: proc(x: Vec2f, e: EdgePtr) -> bool {
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
	for &m, i in state.mappings do m = i
}

switch_places :: proc(#any_int i, j: int) {
	{
		temp := state.vertices[j]
		state.vertices[j] = state.vertices[i]
		state.vertices[i] = temp
	}
	{
		temp := state.mappings[j]
		state.mappings[j] = state.mappings[i]
		state.mappings[i] = temp
	}
}

partition :: proc(#any_int l, u: int) -> VertPtr {
	pivot := get_vert(VertPtr(u))
	i := l

	fmt.println(l, u)
	for &v, j in state.vertices[l:u + 1] {
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

delaunay :: proc(set: []int, offset: i32 = 0) -> (le, re: EdgePtr) {
	switch len(set) {
	case 2:
		log.info("hihi")
		a := make_edge(offset, offset + 1)
		return a, sym(a)
	case 3:
		log.info("hihi3")
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
		log.info("hihi>=4")
		L := set[0:len(set) / 2]
		R := set[len(set) / 2:]
		ldo, ldi := delaunay(L)
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
			if right_is_delaunay do base_l = connect(r_cand, sym(base_l))
			else do base_l = connect(sym(base_l), sym(l_cand)) // l_cand is delaunay
		}
		return ldo, rdo
	}
}

traverse_triangles :: proc(dest: ^[dynamic]Triangle) {
	visited_edges := make(map[EdgePtr]bool, context.temp_allocator)
    fmt.println(state.next_edge)
	main_loop: for i in 0 ..< state.next_edge / 4 {
		e := state.edges[i * 4]
        if e == state.available_edge {
            state.available_edge = o_next(state.available_edge)
            continue
        } 
		half_edge: for _ in 0 ..< 2 {
			defer e = sym(e)
			if visited_edges[e] do continue

			current_edge := e
			new_tri: Triangle
			triangle: for tri_idx in 0 ..< 3 {
				visited_edges[current_edge] = true
				new_tri[tri_idx] = i32(state.mappings[orig(current_edge)^])
				// new_tri[tri_idx] = orig(current_edge)^
				// if current_edge == l_next(current_edge) do continue half_edge
				current_edge = l_next(current_edge)
			}
			if e != current_edge do continue

			{ 	// ccw to cw
				t := new_tri[0]
				new_tri[0] = new_tri[2]
				new_tri[2] = t
			}

			append(dest, new_tri)
		}
	}
}

triangulate_vertices :: proc(
	vertices: []Vec2f,
	/*TODO: put constraints here*/
) -> (
	triangles: []Triangle,
) {
	{ 	// init state
		state.vertices = make([]Vec2f, len(vertices))
		copy_slice(state.vertices, vertices)
		state.mappings = make([]int, len(vertices))
		init_mappings()

		n_edges := len(vertices) * len(vertices)

		state.edges = make([]EdgePtr, n_edges * 4)
		state.data = make([]EdgePtr, n_edges * 2)

		state.next_edge = 0
		state.available_edge = NIL
	}

	defer { 	// de-init state
		delete(state.vertices)
		delete(state.mappings)
		delete(state.edges)
		delete(state.data)
	}

	{ 	// sort vertices
		quick_sort(0, len(vertices) - 1)
	}

	le, re := delaunay(state.mappings)

	log.info(state.edges[:state.next_edge])
	log.info(state.data[:state.next_edge / 2])
	log.info(state.mappings)

	fmt.println("edges:", state.edges[:state.next_edge])
	fmt.println("data:", state.data[:state.next_edge / 2])
	fmt.println("mappings:", state.mappings)

	triangles_dyn := make([dynamic]Triangle)
	traverse_triangles(&triangles_dyn)
	resize_dynamic_array(&triangles_dyn, len(triangles_dyn))

	return triangles_dyn[:]
}

@(test)
traversal_test :: proc(_: ^testing.T) {
	// n_vertices := 5
	// n_edges_max := 100
	vertices := [?]Vec2f{
        {0, 0},
        {1, 0},
        {2, 0},
        {3, 0},
        {0, 1},
        {1, 1},
        {2, 1},
        {3, 1},
        {0, 2},
        {1, 2},
        {2, 2},
        {3, 2},
        {0, 3},
        {1, 3},
        {2, 3},
        {3, 3},
    }

	triangles := triangulate_vertices(vertices[:])
	log.info(triangles)
}

import "core:fmt"
import "core:log"
import "core:os"
import "core:testing"
