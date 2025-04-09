extends CharacterBody2D

var health = 100

@onready var player = get_node("/root/main_game/jonathan")

func _ready(): # ready() is called whenever the scene is first instantiated in godot
	var slime = get_node("Slime")
	slime.play_walk()

func _physics_process(delta: float):
	var direction = global_position.direction_to(player.global_position)
	velocity = direction * 100
	move_and_slide()
	print(delta)

func take_damage():
	var slime = get_node("Slime")
	print("slime takes damage...")
	health -= 1
	slime.play_hurt()
	print("slime's new health is", health)
	if health == 0:
		queue_free() # destroys the current scene
		const SMOKE = preload("res://smoke_explosion/smoke_explosion.tscn")
		var smoke1 = SMOKE.instantiate()
		get_parent().add_child(smoke1) # adds smoke to the parent of the current david mob node
		smoke1.global_position = global_position # assign smoke to the mob's global position 
