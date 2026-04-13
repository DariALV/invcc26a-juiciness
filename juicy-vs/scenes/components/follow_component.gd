extends Node
class_name FollowComponent

@export_enum("player", "enemy") var target_group: String = "enemy"
@export_enum("player", "enemy") var parent_group: String = "player"
@export var refresh_rate : float = 1
@export var repulsion_force : float = 15
@export var movement : MovementComponent = null

@onready var timer : Timer = $Timer

var target : Node2D = null
var parent : Node2D = null

func _ready():
	assert(get_parent() is Node2D, "PathfindComponent parent must be a Node2D")
	assert(movement != null, "MovementComponent required, found null instead")
	parent = get_parent()
	timer.wait_time = refresh_rate
	timer.timeout.connect(on_timer_timeout)
	search_target()

func search_target():
	var entities = get_tree().get_nodes_in_group(target_group) as Array[Node2D]
	print(entities)
	for entity in entities:
		if (target == null):
			target = entity
			continue
		if (parent.global_position.distance_squared_to(entity.global_position) < 
			parent.global_position.distance_squared_to(target.global_position)):
			target = entity
		
func follow_target(delta):
	if parent != null and target != null:
		var direction = parent.global_position.direction_to(target.global_position)
		movement.move(direction, delta)
	
func on_timer_timeout():
	search_target()

func _physics_process(delta):
	follow_target(delta)
