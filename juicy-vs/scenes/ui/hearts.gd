extends Control
@onready var heart_1: TextureRect = $MarginContainer/HBoxContainer/Heart1
@onready var heart_2: TextureRect = $MarginContainer/HBoxContainer/Heart2
@onready var heart_3: TextureRect = $MarginContainer/HBoxContainer/Heart3

func _ready():
	EventBus.player_health_changed.connect(on_player_health_changed)

@warning_ignore("unused_parameter")
func on_player_health_changed(before: float, after: float):
	modify_heart_sprite(heart_1, after, 1)
	modify_heart_sprite(heart_2, after, 2)
	modify_heart_sprite(heart_3, after, 3)

func modify_heart_sprite(heart_rect: TextureRect, current_health: float, heart_num: float):
	
	var atlas: AtlasTexture = heart_rect.texture
	
	var clamped_health: float = clamp(current_health, 0, 18)
	heart_num -= 1
	# Vacio
	if clamped_health <= 2 * heart_num:
		atlas.region.position = Vector2(32, 0)
	# Medio Rojo
	elif clamped_health <= 1 + 2 * heart_num:
		atlas.region.position = Vector2(16, 0)
	# Rojo
	elif clamped_health <= 6 + 2 * heart_num:
		atlas.region.position = Vector2(0, 0)
	# Medio Azul
	elif clamped_health <= 7 + 2 * heart_num:
		atlas.region.position = Vector2(16, 16)
	# Azul
	elif clamped_health <= 12 + 2 * heart_num:
		atlas.region.position = Vector2(0, 16)
	# Amarillo
	elif clamped_health <= 13 + 2 * heart_num:
		atlas.region.position = Vector2(16, 32)
	# Amarillo
	else:
		atlas.region.position = Vector2(0, 32)
	

# Heart1: Vacio = [0, 0], Semi Rojo = [1, 1], Rojo = [2, 6], Semi Azul = [7, 7], Azul = [8, 12], Semi Amarillo = [13, 13], Amarillo = [14, 18]
# Heart2: Vacio = [0, 2], Semi Rojo = [3, 3], Rojo = [4, 8], Semi Azul = [9, 9], Azul = [10, 14], Semi Amarillo = [15, 15], Amarillo = [16, 18]
# Heart3: Vacio = [0, 4], Semi Rojo = [5, 5], Rojo = [6, 10], Semi Azul = [11, 11], Azul = [12, 16], Semi Amarillo = [17, 17], Amarillo = [18, 18]
