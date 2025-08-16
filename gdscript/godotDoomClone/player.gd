extends CharacterBody3D

@onready var animated_sprite_2d = $UI/gunbase/AnimatedSprite2D
@onready var ray_cast_3d = $RayCast3D
@onready var shoot_sound = $shootsoudn

const SPEED = 5.0
const MOUSE_SENS = 0.1


var can_shoot = true
var dead = false

func _ready():
	Input.mouse_mode = Input.MOUSE_MODE_CAPTURED
	animated_sprite_2d.animation_finished.connect(shoot_anim_done)
	$UI/deathscreen/Panel/Button.button_up.connect(restart)
	
func _input(event):
	if dead:
		return
	elif event is InputEventMouseMotion:
		rotation_degrees.y -= event.relative.x * MOUSE_SENS
	
func _process(delta): # called every frame similar to love.update()
	if dead:
		return
	if Input.is_action_just_pressed("exit"):
		get_tree().quit()
	if Input.is_action_just_pressed("restart"):
		restart()
	if Input.is_action_just_pressed("shoot"):
		shoot()

func restart():
	get_tree().reload_current_scene()

func shoot():
	if !can_shoot:
		return
	can_shoot = false
	animated_sprite_2d.play("shoot")
	shoot_sound.play()
	if ray_cast_3d.is_colliding() and ray_cast_3d.get_collider().has_method("kill"):
		ray_cast_3d.get_collider().kill()


func shoot_anim_done():
	can_shoot = true

func kill():
	dead = true
	$UI/deathscreen.show()
	Input.mouse_mode = Input.MOUSE_MODE_VISIBLE

func _physics_process(delta): # called every frame

	if dead:
		return

	var input_dir = Input.get_vector("move_left", "move_right", "move_forwards", "move_backwards") # converts input to 2d vector
	var direction = (transform.basis * Vector3(input_dir.x, 0, input_dir.y)).normalized() # converts above to 3d vector
	if direction: # if direction not 0
		velocity.x = direction.x * SPEED
		velocity.z = direction.z * SPEED
	else:
		velocity.x = move_toward(velocity.x, 0, SPEED)
		velocity.z = move_toward(velocity.z, 0, SPEED)

	move_and_slide() # moves 3d character body, including if it hits something at an angle
