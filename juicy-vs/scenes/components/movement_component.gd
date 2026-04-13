extends Node
class_name MovementComponent

@export var speed : float = 1
@export var max_speed : float = 10

var parent : CharacterBody2D = null

func _ready():
	assert(get_parent() is CharacterBody2D, 
	"MovementComponent debe ser hijo de un CharacterBody2D")
	parent = get_parent()

func calculate_velocity(direction : Vector2) -> Vector2:
	return direction.normalized() * speed

func increase_speed(amount : float):
	speed = min(max_speed, speed + amount);
	
func move(direction : Vector2):
	parent.velocity = direction.normalized() * speed
	parent.move_and_slide()
