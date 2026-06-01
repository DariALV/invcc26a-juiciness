class_name WorldGenerator extends Node

@export var map_size: Vector2i

@export var decorations: Array[Decoration] = []
@export var ground_tile: Tile
@export var decoration_amount: int = 0

@export var decor_layer: TileMapLayer
@export var ground_layer: TileMapLayer

var map_center: Vector2i
var occupied: Dictionary = {}

var rng: RandomNumberGenerator

func _ready():
	map_center = map_size/2
	rng = RandomNumberGenerator.new()
	build_ground()
	build_decorations()

func build_ground():
	if ground_tile and ground_layer:
		for i in map_size.x:
			for j in map_size.y:
				if not ground_tile.tile_offset_variations.is_empty():
					ground_layer.set_cell(Vector2i(i, j) - map_center, 0, ground_tile.tile_offset_variations[0])
				else:
					ground_layer.set_cell(Vector2i(i, j) - map_center, 0, Vector2i.ONE)

func build_decorations():
	var max_place_attemps: int = 100
	for i in decoration_amount:
		var place_attempts: int = 0
		while place_attempts < max_place_attemps:
			if place_decor_tile():
				break
			place_attempts += 1

func place_decor_tile() -> bool:
	if not decor_layer:
		return false
	var pos: Vector2i = Vector2i(rng.randi() % map_size.x, rng.randi() % map_size.y)
	var chosen_decoration: Decoration = select_random_decor()
	if not chosen_decoration:
		return false
	var decor_cells = get_decor_cells(pos, chosen_decoration)
	if !cells_occupied(decor_cells) and cells_within_map(decor_cells):
		decor_layer.set_cell(pos - map_center, 0, chosen_decoration.tile_offset_variations.pick_random())
		occupy_cells(decor_cells)
		return true
	return false

func select_random_decor() -> Decoration:
	var summed_weights: int = 0
	for d in decorations:
		summed_weights += d.weight
	
	var random_number: int = rng.randi() % summed_weights
	for d in decorations:
		if random_number < d.weight:
			return d
		random_number -= d.weight
	return null

func get_decor_cells(start_cell: Vector2i, decor: Decoration) -> Array[Vector2i]:
	var cells: Array[Vector2i] = []
	for i in decor.tile_size.x:
		for j in decor.tile_size.y:
			cells.push_back(start_cell + Vector2i(i, j))
	return cells

func cells_occupied(cells: Array[Vector2i]) -> bool:
	for c in cells:
		if occupied.has(c):
			return true
	return false

func cells_within_map(cells: Array[Vector2i]) -> bool:
	var map_rect: Rect2i = Rect2i(Vector2i.ZERO, map_size)
	for c in cells:
		if not map_rect.has_point(c):
			return false
	return true

func occupy_cells(cells: Array[Vector2i]) -> void:
	for c in cells:
		occupied[c] = true
