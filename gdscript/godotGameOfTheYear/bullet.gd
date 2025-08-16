extends Area2D

var travelled_distance = 0 # global variable 

func _physics_process(delta: float):
	
	const MAXIMUM_RANGE = 1200

	var direction = Vector2.RIGHT.rotated(rotation)
	position += direction * 1000 * delta # client-dependent
	travelled_distance += delta * 1000
	if travelled_distance > MAXIMUM_RANGE:
		queue_free()


func _on_body_entered(body: Node2D) -> void:
	print("body entered")
	queue_free()
	if body.has_method("take_damage"): # duck typing 
		body.take_damage()
	# Replace with function body.
