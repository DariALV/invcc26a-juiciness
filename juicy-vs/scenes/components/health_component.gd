extends Node
class_name HealthComponent

signal died
signal health_changed(before: float, after: float)

@export var current_health: float = 100
@export var max_health: float = 100
@export var min_health: float = 0

var isDead : bool = false

func apply_heal(amount : float):
	if !isDead:
		var before_health = current_health
		current_health = min(max_health, current_health + amount)
		if (before_health != current_health):
			health_changed.emit(before_health, current_health)

func apply_damage(amount : float):
	var before_health = current_health
	current_health = max(min_health, current_health - amount)
	if (before_health != current_health):
		health_changed.emit(before_health, current_health)
	if current_health == 0:
		isDead = true
		died.emit()
