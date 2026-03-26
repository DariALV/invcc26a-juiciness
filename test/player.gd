extends Node2D

var speed: float = 50
var hue_color: float = 0
@onready var sprite: Sprite2D = $Sprite 
@onready var outline: Sprite2D = $Outline

func _ready() -> void:
	pass


# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(delta: float) -> void:
	hue_color += delta * 360
	sprite.modulate = Color.from_hsv(hue_color/360.0, 0.25, 1)
	outline.modulate = Color.from_hsv((hue_color)/360.0, 0.25, 1)

func _physics_process(delta):
	if Input.is_action_pressed("MoveUp"):
		position.y -= speed * delta
	if Input.is_action_pressed("MoveDown"):
		position.y += speed * delta
	if Input.is_action_pressed("MoveLeft"):
		position.x -= speed * delta
	if Input.is_action_pressed("MoveRight"):
		position.x += speed * delta
