package OdinEngine

import "core:fmt"
import "core:math"
import "core:math/linalg"
import "utils"
import "vendor:glfw"

CameraSpeed :: 20
AngularSpeed :: 100

change_velocity :: proc(camera: ^utils.Camera, v: utils.Vec3f) {
	camera.velocity += v
}

move_camera :: proc(state: ^State) {
	using state

	v := camera.velocity

	at := linalg.normalize0(v)
	if at == {0, 0, 0} do return

	right := linalg.normalize(linalg.cross(camera.up, camera.direction))

	delta_movement := at * (f32(CameraSpeed) * f32(time.delta_s))

	delta_movement_global := camera.direction * delta_movement.z
	delta_movement_global += camera.up * delta_movement.y
	delta_movement_global += right * delta_movement.x

	camera.position += delta_movement_global
}


rotate_camera_state :: proc(state: ^State) {
	using state

	if camera.angular_velocity == {0, 0, 0} do return

	delta_angles :=
		camera.angular_velocity *
        linalg.to_radians(f32(AngularSpeed)) * 
		f32(time.delta_s)

	camera.euler_angles.pitch += delta_angles.x
	camera.euler_angles.yaw -= delta_angles.y
	camera.euler_angles.roll += delta_angles.z

	{
		using camera.euler_angles

        pitch = math.clamp(pitch, -linalg.PI / 2 + 0.01, linalg.PI / 2 - 0.01)

		camera.direction = utils.Vec3f {
			-math.cos(yaw) * math.cos(pitch),
			-math.sin(pitch),
			-math.sin(yaw) * math.cos(pitch),
		}
	}
}

rotate_camera_mouse :: proc(state: ^State, delta_x, delta_y: f32) {
    delta_x_angle := delta_x * state.mouse.sensitivity
    delta_y_angle := delta_y * state.mouse.sensitivity 

	state.camera.euler_angles.yaw += linalg.to_radians(delta_x_angle)
	state.camera.euler_angles.pitch -= linalg.to_radians(delta_y_angle)
    
	{
		using state.camera.euler_angles

        pitch = math.clamp(pitch, -linalg.PI / 2 + 0.01, linalg.PI / 2 - 0.01)

		state.camera.direction = utils.Vec3f {
			-math.cos(yaw) * math.cos(pitch),
			-math.sin(pitch),
			-math.sin(yaw) * math.cos(pitch),
		}
	}
}

rotate_camera :: proc {rotate_camera_state, rotate_camera_mouse}
