extends Node2D

@onready var health : HealthComponent = $HealthComponent
@onready var movement : MovementComponent = $MovementComponent

func _ready() -> void:
	health.died.connect(on_died)


# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(delta: float) -> void:
	pass

func on_died():
	movement.speed = 0
