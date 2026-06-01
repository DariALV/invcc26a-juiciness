class_name ForceComponent extends Node

@export var movement: MovementComponent
@export var update_interval: int = 4

var _behaviors: Array[SteeringBehavior] = []
var _cached_force: Vector2 = Vector2.ZERO
var _offset: int = 0

func _ready():
	update_interval = maxi(1, update_interval)
	for c in get_children():
		if c is SteeringBehavior:
			_behaviors.append(c)
	_offset = get_instance_id() % update_interval

func _physics_process(_delta):
	if movement == null or not (owner is CharacterBody2D):
		return
	if (Engine.get_physics_frames() + _offset) % update_interval == 0:
		var force := Vector2.ZERO
		for b in _behaviors:
			force += b.calculate(owner, movement.max_speed)
		_cached_force = force
	movement.current_force += _cached_force

func apply_force(force: Vector2):
	movement.current_force += force
