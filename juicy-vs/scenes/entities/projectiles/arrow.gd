extends CharacterBody2D
class_name Arrow

@export var lifespan: float = 5
@export var damage: float = 1
@export var pierce: int = 1
@export var target_group: String = "enemy"

@onready var hitbox_component: HitboxComponent = $HitboxComponent
@onready var movement_component: MovementComponent = $MovementComponent

func _ready():
	if target_group == "player":
		hitbox_component.collision_layer = 4
		hitbox_component.collision_mask = 1
	hitbox_component.collision_detected.connect(on_collision_detected)
	await get_tree().create_timer(lifespan).timeout
	queue_free()

func _process(delta):
	if velocity != Vector2.ZERO:
		rotation = velocity.angle()

func on_collision_detected():
	queue_free()

func _enter_tree():
	NodeCounter.add_entity("arrow")

func _exit_tree():
	NodeCounter.remove_entity("arrow")
