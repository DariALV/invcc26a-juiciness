extends Camera2D
class_name JuicyCamera

@export var random_strength: float = 30
@export var shake_fade: float = 5.0
@export var target: Node2D = null

var rng = RandomNumberGenerator.new()

var shake_strength: float = 0

func _ready():
	pass

func apply_shake():
	shake_strength = random_strength
	
func _process(delta):
	
	if target:
		global_position = target.global_position.round()
	
	if shake_strength > 0:
		shake_strength = lerpf(shake_strength, 0, shake_fade * delta)
	
	offset = randomOffset()

func randomOffset() -> Vector2:
	return Vector2(rng.randf_range(-shake_strength, shake_strength), rng.randf_range(-shake_strength, shake_strength))
