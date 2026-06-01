#class_name MovementComponent extends Node
#
#@export var max_speed: float = 0
#@export var max_force: float = 0
#@export var friction: float = 0
#@export var top_speed_when_no_force: bool = false
#
#var current_speed: Vector2 = Vector2.ZERO
#var current_force: Vector2 = Vector2.ZERO
#var parent: CharacterBody2D = null
#
#func _ready():
	#assert(get_parent() is CharacterBody2D, "MovementComponent parent must be a CharacterBody2D")
	#parent = get_parent()
#
#func _physics_process(delta):
	#if current_force.length_squared() > max_force * max_force:
		#current_force = current_force.limit_length(max_force)
#
	#current_speed += current_force * delta
	#if current_speed.length_squared() > max_speed * max_speed:
		#current_speed = current_speed.limit_length(max_speed)
#
	#if current_force == Vector2.ZERO:
		#current_speed -= current_speed * friction * delta
		#if current_speed.length_squared() < 25.0:
			#current_speed = Vector2.ZERO
#
	#if top_speed_when_no_force and current_force == Vector2.ZERO:
		#parent.velocity = current_speed.normalized() * max_speed
	#else:
		#parent.velocity = current_speed
	#parent.move_and_slide()
	#current_force = Vector2.ZERO
	
class_name MovementComponent extends Node

@export var max_speed: float = 0
@export var max_force: float = 0
@export var friction: float = 0
@export var top_speed_when_no_force: bool = false

var current_speed: Vector2 = Vector2.ZERO
var current_force: Vector2 = Vector2.ZERO
var parent: CharacterBody2D = null

func _ready():
	assert(get_parent() is CharacterBody2D, "MovementComponent parent must be a CharacterBody2D")
	parent = get_parent()

func _physics_process(delta):
	if current_force.length_squared() > max_force * max_force:
		current_force = current_force.limit_length(max_force)

	current_speed += current_force * delta
	if current_speed.length_squared() > max_speed * max_speed:
		current_speed = current_speed.limit_length(max_speed)

	if current_force == Vector2.ZERO:
		current_speed -= current_speed * friction * delta
		if current_speed.length_squared() < 25.0:
			current_speed = Vector2.ZERO

	if top_speed_when_no_force and current_force == Vector2.ZERO:
		parent.global_position += current_speed.normalized() * max_speed * delta
	else:
		parent.global_position += current_speed * delta
	current_force = Vector2.ZERO
