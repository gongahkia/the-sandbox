extends CharacterBody2D

var player_health = 100.0
signal health_gone

func _physics_process(delta: float):
	
	var direction = Input.get_vector("move_left", "move_right", "move_up", "move_down") # all the defined function calls
	velocity = direction * 600 # value here is in pixels
	move_and_slide() # takes other colissions in the game into account

	if velocity.length() > 0.0: # player walking
		get_node("HappyBoo").play_walk_animation() # takes the relative filepath of the node allowing method calls
		# $HappyBoo works the same since $ is the shorthand function to get another node by its relative filepath
	else:
		$HappyBoo.play_idle_animation() # takes the relative filepath of the node allowing method calls
		# get_node("HappyBoo") also works, this is just proof that both are acceptable

	const DAMAGE_RATE = 50.0
	var hurt_area = get_node("hurt_area")
	var health_bar = get_node("health_bar")
	var overlapping_mobs = hurt_area.get_overlapping_bodies()
	if overlapping_mobs.size() > 0:
		player_health -= DAMAGE_RATE * overlapping_mobs.size() * delta
		health_bar.value = player_health
		if player_health <= 0.0:
			print("game over")
			health_gone.emit() 	# emit a custom signal 	
