extends ColorRect

var square_id := 1

# Called when the node enters the scene tree for the first time.
func _ready():
	pass # Replace with function body.

# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(delta):
	pass

func set_square_color(desiredColor) -> void:
	color = desiredColor
