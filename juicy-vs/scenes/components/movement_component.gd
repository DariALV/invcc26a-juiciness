class_name MovementComponent extends Node

@export var stats: StatsComponent
@export var friction: float = 0

var parent : CharacterBody2D = null

func _ready():
	assert(get_parent() is CharacterBody2D, "MovementComponent parent must be a CharacterBody2D")
	assert(stats != null, "StatsComponent required, found null instead")
	parent = get_parent()

func _process(delta):
	if stats.current_force.length() > stats.max_force:
		stats.current_force = stats.current_force.normalized() * stats.max_force
	
	stats.current_speed += stats.current_force * delta
	
	if stats.current_speed.length() > stats.max_speed:
		stats.current_speed = stats.current_speed.normalized() * stats.max_speed
		
	if stats.current_force == Vector2.ZERO:
		stats.current_speed -= stats.current_speed * friction * delta
		if stats.current_speed.length() < 5:
			stats.current_speed = Vector2.ZERO
	
	
	parent.velocity = stats.current_speed
	parent.move_and_slide()
	stats.current_force = Vector2.ZERO
