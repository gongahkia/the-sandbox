extends Node2D

# ----- REQUIRED IMPORTS -----

@onready var square_scene = preload("res://scene/square.tscn")
@onready var piece_scene = preload("res://scene/piece.tscn")
@onready var board_grid = $board/board_grid
@onready var board = $board 

var board_array := []
var piece_array := []
var icon_offset := Vector2(60, 60)

# Called when the node enters the scene tree for the first time.
func _ready():
	print("board is being instanitated")
	for i in range(64):
		create_square()
	var color_bit = 0
	for y in range(8):
		for x in range(8):
			if x % 2 == color_bit:
				board_array[y*8 + x].set_square_color(Color.DARK_SALMON)
		if color_bit == 0:
			color_bit = 1
		else:
			color_bit = 0
	piece_array.resize(64)
	piece_array.fill(0)
	print("board has been instantiated")

# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(delta):
	pass

func create_square():
	var new_square = square_scene.instantiate()
	new_square.square_id = board_array.size()
	board_grid.add_child(new_square)
	board_array.push_back(new_square)

func add_piece(piece_type, piece_location) -> void:
	var new_piece = piece_scene.instantiate()
	board.add_child(new_piece)
	new_piece.type = piece_type
	new_piece.load_icon(piece_type)
	new_piece.global_position = board_array[piece_location].global_position - icon_offset
	var stored = new_piece.global_position
	print("piece being added at %s" % [stored]) # debug statement
	piece_array[piece_location] = new_piece
	new_piece.square_id = piece_location

func _on_debug_button_pressed():
	print("button being pressed")
	# add_piece(data_handler.piece_names.WHITE_CARP, 0) # 0-indexed
	add_piece(data_handler.piece_names.WHITE_HAYAO_MIYAZAKI, 0) # 0-indexed
	add_piece(data_handler.piece_names.WHITE_HAYAO_MIYAZAKI, 1) # 0-indexed
	add_piece(data_handler.piece_names.WHITE_HAYAO_MIYAZAKI, 2) # 0-indexed
	add_piece(data_handler.piece_names.WHITE_HAYAO_MIYAZAKI, 63) # 0-indexed
