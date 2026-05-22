class_name ForceComponent extends Node

@export var stats: StatsComponent

var behaviors = []

func _process(delta):
	if stats and owner is CharacterBody2D:
		behaviors = get_children()
		for b in behaviors:
			if b is SteeringBehavior:
				stats.current_force += b.calculate(owner, stats.max_speed)

func apply_force(force: Vector2):
	stats.current_force += force
