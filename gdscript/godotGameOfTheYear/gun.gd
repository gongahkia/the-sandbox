extends Area2D

# used so that enemies can hold guns as well

func _physics_process(delta: float):
	var in_range = get_overlapping_bodies()
	if in_range.size() > 0:
		print(in_range.size())
		print(delta)
		var target_enemy = in_range[0]
		look_at(target_enemy.global_position)

func shoot():
	var shooting_point = get_node("weapon_pivot/Pistol/shooting")
	const BULLET = preload("res://bullet.tscn") # load() loads absolute filepaths when the file is instantiated, preload loads dynamically as required
	var new_bullet = BULLET.instantiate() # instantiate() is a programmatic function call to instantiate a new scene 
	new_bullet.global_position = shooting_point.global_position # move new bullet to the shooting point's original position
	new_bullet.global_rotation = shooting_point.global_rotation # move new bullet to the shooting point's original rotation
	shooting_point.add_child(new_bullet) # instantiate a child node of the bullet under the shooting point


func _on_timer_timeout() -> void:
	print("function timed out...")
	shoot()
	pass # Replace with function body.
