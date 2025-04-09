extends Node2D

func _ready(): # runs automatically whent the game instantiates
	spawn_david()
	spawn_david()
	spawn_david()
	spawn_david()
	spawn_david()

func spawn_david():
	var new_david = preload("res://david.tscn").instantiate() # simultaneous dynamic loading and memoisation and mob instantiation
	get_node("jonathan/Path2D/PathFollow2D").progress_ratio = randf()
	new_david.global_position = get_node("jonathan/Path2D/PathFollow2D").global_position
	add_child(new_david) # add new mob as child of the game mode
	


func _on_timer_timeout() -> void:
	print("timer timed out...")
	spawn_david()
	pass # Replace with function body.


func _on_jonathan_health_gone() -> void:
	print("player health gone, therefore game over...")
	var game_over = get_node("game_over")
	game_over.visible = true
	get_tree().paused = true
