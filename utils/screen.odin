package utils

@(require_results)
normalize_coord :: proc(v, screen_size: Vec2f) -> Vec2f {
    res := (v / screen_size)
    // res.x *= -1
    res *= 2
    return res
}

@(require_results)
center_coord :: proc(v: Vec2f) -> Vec2f {
    res := v - {+1, +1}    
    return res
}

@(require_results)
position_rect :: proc(r: Rect, screen_size: Vec2f) -> (res: Rect) {
    res.pos = normalize_coord(r.pos, screen_size)
    res.pos = center_coord(res.pos)
    res.size = normalize_coord(r.size, screen_size)
    return
}
