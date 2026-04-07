extends Node
class_name HealthComponent

signal died
signal health_changed(new_health : float)

@export var current_health : float = 0
@export var max_health : float = 100
@export var min_health : float = 0

func apply_heal(amount : float):
	current_health = min(max_health, current_health + amount)
	health_changed.emit(current_health)

func apply_damage(amount : float):
	current_health = max(min_health, current_health - amount)
	health_changed.emit(current_health)
