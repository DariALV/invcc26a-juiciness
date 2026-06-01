extends Camera2D
class_name JuicyCamera

@export var random_strength: float = 30
@export var shake_fade: float = 5.0

@export var min_zoom_scale: float = 1.5

@export var target: Node2D = null

var rng = RandomNumberGenerator.new()

var shake_strength: float = 0

func _ready():
	EventBus.apply_camera_shake.connect(apply_shake)

func apply_shake(intensity: float):
	shake_strength = random_strength
	
func _process(delta):
	var zoom_scale = clamp(1 + Database.enemies_alive/15.0, 1, min_zoom_scale)
	
	zoom = lerp(zoom, Vector2(1, 1)/zoom_scale, delta)
	
	if target:
		global_position = target.global_position
	
	if shake_strength > 0:
		shake_strength = lerpf(shake_strength, 0, shake_fade * delta)
	
	offset = randomOffset()

func randomOffset() -> Vector2:
	return Vector2(rng.randf_range(-shake_strength, shake_strength), rng.randf_range(-shake_strength, shake_strength))
