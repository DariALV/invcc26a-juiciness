extends CharacterBody2D
class_name Player

@onready var health : HealthComponent = $HealthComponent
@onready var movement : MovementComponent = $MovementComponent

func _ready() -> void:
	health.died.connect(on_died)


# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(delta: float) -> void:
	pass

func on_died():
	movement.speed = 0
	
func _physics_process(delta):
	var input_direction = Input.get_vector("left", "right", "up", "down")
	movement.move(input_direction, delta)
