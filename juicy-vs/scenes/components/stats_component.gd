extends Node
class_name StatsComponent

@export var max_speed: float = 0
@export var max_force: float = 0
@export var max_health: float = 0

var current_speed: Vector2 = Vector2.ZERO
var current_force: Vector2 = Vector2.ZERO
var current_health: float = 0
