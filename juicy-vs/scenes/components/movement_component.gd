extends Node
class_name MovementComponent

@export var speed : float = 1
@export var max_speed : float = 10
@export var can_move : bool = true
@export var can_be_applied_force : bool = true

var parent : Node2D = null

func _ready():
	assert(get_parent() is Node2D, 
	"MovementComponent parent must be a Node2D")
	parent = get_parent()

func increase_speed(amount : float):
	speed = min(max_speed, speed + amount);
	
func move(direction : Vector2, delta : float = 1):
	if can_move:
		var final_direction = direction.normalized() * speed * delta
		parent.global_position += final_direction

func apply_force(force : Vector2, delta : float = 1):
	parent.global_position += force * delta

func set_position(pos : Vector2):
	parent.global_position = pos
