extends CharacterBody2D
class_name Player

@onready var health : HealthComponent = $HealthComponent
@onready var stats : StatsComponent = $StatsComponent

@onready var camera : JuicyCamera = $JuicyCamera
@onready var animated_sprite : AnimatedSprite2D = $AnimatedSprite2D

func _ready() -> void:
	health.died.connect(on_died)
	health.health_changed.connect(on_health_changed)


# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(delta: float) -> void:
	pass

func on_died():
	stats.max_speed = 0
	
func _physics_process(delta):
	var input_direction = Input.get_vector("left", "right", "up", "down")
	if (input_direction.x > 0):
		animated_sprite.flip_h = false
	elif (input_direction.x < 0):
		animated_sprite.flip_h = true
	if (input_direction != Vector2.ZERO):
		animated_sprite.play("running")
	else:
		animated_sprite.play("idle")
	stats.current_speed = input_direction * stats.max_speed

func on_health_changed(health):
	camera.apply_shake()
