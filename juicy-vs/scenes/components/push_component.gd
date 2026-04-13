extends Node
class_name PushComponent

@export var repulsion_force : float = 20
@export var min_distance : float = 64
@export var movement : MovementComponent

var parent : Node2D = null
var parent_group : String = ""
var groups : Array[StringName]

func _ready():
	assert(get_parent() is Node2D, "PushComponent parent must be a Node2D")
	assert(movement != null, "MovementComponent required, found null instead")
	parent = get_parent()
	groups = parent.get_groups()

func _physics_process(delta):
	for group in groups:
		var entities = get_tree().get_nodes_in_group(group) as Array[Node2D]
		movement.apply_force(get_separation(entities), delta)

func get_separation(neighbors: Array) -> Vector2:
	var push = Vector2.ZERO
	for other in neighbors:
		var diff = parent.global_position - other.global_position
		var dist = diff.length()
		if dist < min_distance and dist > 0:
			push += repulsion_force * diff.normalized() / dist
	return push
