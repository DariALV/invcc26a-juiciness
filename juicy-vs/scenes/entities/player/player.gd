extends CharacterBody2D
class_name Player

@onready var health : HealthComponent = $HealthComponent
@onready var movement : MovementComponent = $MovementComponent

@onready var animated_sprite : AnimatedSprite2D = $AnimatedSprite2D

@onready var bow = $Bow

func _ready() -> void:
	health.died.connect(on_died)
	health.health_changed.connect(on_health_changed)
	EventBus.add_player_health.connect(on_add_health)
	UpgradeManager.weapon_upgrade_taken.connect(on_weapon_upgrade_taken)

func on_died():
	movement.max_speed = 0

func _physics_process(delta):
	var input_direction = Input.get_vector("left", "right", "up", "down")
	if (input_direction.x > 0):
		animated_sprite.flip_h = false
	elif (input_direction.x < 0):
		animated_sprite.flip_h = true
	if (input_direction != Vector2.ZERO):
		animated_sprite.play("running")
	else:
		animated_sprite.play("idle")
	movement.current_speed = input_direction * movement.max_speed
	rotate_bow()

func rotate_bow():
	var mouse_direction = get_local_mouse_position().normalized()
	var distance = 8
	bow.rotation = mouse_direction.angle()
	bow.position = mouse_direction * distance
	bow.shoot_direction = mouse_direction
	

func on_add_health(amount: float):
	pass
	#health.apply_heal(amount)

func on_health_changed(before: float, after: float):
	EventBus.player_health_changed.emit(before, after)
	if (before > after):
		Database.hits_taken += 1
		EventBus.apply_camera_shake.emit(30)

func on_weapon_upgrade_taken(upgrade: WeaponUpgrade):
	upgrade.apply_upgrade(bow)
