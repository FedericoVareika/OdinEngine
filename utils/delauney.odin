package Utils

// FIST: Fast Industrial-Strength Triangulation of Polygons
// https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=e5ba1f1d8d508f7d8f0aa3133cf947cc6578d524

// https://github.com/Samson-Mano/Delaunay-triangulation
// https://www.cs.cmu.edu/afs/andrew/scs/cs/15-463/2001/pub/src/a2/quadedge.html

delauney_triangulation :: proc(
	vertices: []Vec2f,
	end_pts_of_contours: []u16be,
) -> []Vec3i {
    indices := make([dynamic]Vec3i)


}
