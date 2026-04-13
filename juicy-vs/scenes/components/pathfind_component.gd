extends Node

@export_enum("player", "enemy") var target_group: String = "enemy"
@export var refresh_rate : float = 1

@onready var timer : Timer = $Timer

var target : CharacterBody2D = null
var parent : CharacterBody2D = null

func _ready():
	assert(get_parent() is CharacterBody2D, 
	"PathfindComponent debe ser hijo de un CharacterBody2D")
	parent = get_parent()
	timer.wait_time = refresh_rate
	timer.timeout.connect(on_timer_timeout)

func search_target():
	var entities = get_tree().get_nodes_in_group(target_group) as Array[CharacterBody2D]
	for entity in entities:
		if (target == null):
			target = entity
			continue
		if (parent.global_position.distance_squared_to(entity.global_position) < 
			parent.global_position.distance_squared_to(target.global_position)):
			target = entity
		
func follow_target():
	pass

func on_timer_timeout():
	search_target()
	
