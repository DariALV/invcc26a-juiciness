extends Node
class_name MovementComponent

@export var speed : float = 1
@export var max_speed : float = 10

func calculate_velocity(direction : Vector2) -> Vector2:
	return direction.normalized() * speed

func increase_speed(amount : float):
	speed = min(max_speed, speed + amount);
