extends Node
class_name KnockbackComponent

@export var knockback_duration : float = 2
@export var freeze_duration : float = 0.5
@export var min_force : float = 10
@export var max_force : float = 20
@export var max_distance : float = 200
@export var movement : MovementComponent = null

@onready var freeze_timer : Timer = $FreezeTimer

var parent : Node2D = null
var direction : Vector2 = Vector2.ZERO
var is_knockbacked : bool = false
var is_frozen : bool = false
var knockback_time_count : float = 0
var start_position : Vector2
var end_position : Vector2
var force : float = 0

func _ready():
	assert(get_parent() is Node2D, "KnockbackComponent parent must be a Node2D")
	assert(movement != null, "MovementComponent required, found null instead")
	parent = get_parent()
	freeze_timer.wait_time = freeze_duration
	freeze_timer.timeout.connect(remove_freeze)

func apply_knockback(point : Vector2):
	knockback_time_count = 0
	is_knockbacked = true
	is_frozen = false
	start_position = parent.global_position
	direction = point.direction_to(start_position)
	force = get_final_force(point)
	end_position = parent.global_position + force * direction
	movement.can_move = false
	#movement.can_be_applied_force = false

func remove_knockback():
	is_knockbacked = false
	is_frozen = true
	freeze_timer.start()

func remove_freeze():
	is_frozen = false
	movement.can_move = true
	#movement.can_be_applied_force = true

func _physics_process(delta):
	if is_knockbacked:
		knockback_time_count += delta
		var current_position = apply_lerp(
			start_position, 
			end_position, 
			knockback_time_count/knockback_duration,
			ease_out_cubic
		)
		movement.set_position(current_position)
	if knockback_time_count > knockback_duration and is_knockbacked:
		remove_knockback()

func get_final_force(point : Vector2):
	var distance : float = point.distance_to(start_position) 
	var distance_ratio : float = distance/max_distance
	return distance_ratio * (min_force - max_force) + max_force

#TODO: Llevar a Global
func apply_lerp(v1 : Vector2, v2 : Vector2, t : float, lerp_function : Callable):
	return v1 + (v2 - v1) * lerp_function.call(t)

#TODO: Llevar a Global
func ease_out_cubic(t: float):
	return 1 - pow(1 - t, 3)
