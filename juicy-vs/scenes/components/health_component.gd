extends Node
class_name HealthComponent

signal died
signal health_changed(new_health : float)

@export var current_health : float = 100
@export var max_health : float = 100
@export var min_health : float = 0

var isDead : bool = false

func _ready() -> void:
	current_health = max_health

func apply_heal(amount : float):
	if !isDead:
		current_health = min(max_health, current_health + amount)
		health_changed.emit(current_health)

func apply_damage(amount : float):
	current_health = max(min_health, current_health - amount)
	health_changed.emit(current_health)
	if current_health == 0:
		isDead = true
		died.emit()
