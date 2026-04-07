extends Node

signal OnHealthIncreased(float)
signal OnHealthReduced(float)

@export var current_health : float = 0
@export var max_health : float = 100
@export var min_health : float = 0

func Heal(amount : float):
	current_health = min(max_health, current_health + amount)
	OnHealthIncreased.emit(current_health)

func Damage(amount : float):
	current_health = max(min_health, current_health - amount)
	OnHealthReduced.emit(current_health)
