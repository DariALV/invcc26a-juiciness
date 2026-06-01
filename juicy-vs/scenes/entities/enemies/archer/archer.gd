extends CharacterBody2D
class_name Archer

@onready var animated_sprite : AnimatedSprite2D = $AnimatedSprite2D
@onready var movement : MovementComponent = $MovementComponent
@onready var health: HealthComponent = $HealthComponent
@onready var flash_component = $AnimatedSprite2D/FlashComponent
@onready var hurtbox: HurtboxComponent = $HurtboxComponent
@onready var bow = $Bow

var particle_direction: Vector2 = Vector2(1, 0)

func _ready():
	health.died.connect(on_died)
	health.health_changed.connect(on_health_changed)
	hurtbox.collision.connect(on_hurtbox_collision)

func _process(delta):
	var player: Player = GlobalData.get_player()
	if player:
		var direction = global_position.direction_to(player.global_position)
		if (direction.x > 0):
			animated_sprite.flip_h = false
		elif (direction.x < 0):
			animated_sprite.flip_h = true
		if (movement.current_speed == Vector2.ZERO):
			animated_sprite.play("idle")
		else:
			animated_sprite.play("running")
		rotate_bow(direction)
	

func rotate_bow(direction: Vector2):
	var distance = 8
	bow.rotation = direction.angle()
	bow.position = direction * distance
	bow.shoot_direction = direction

func on_died():
	EventBus.add_player_health.emit(1)
	queue_free()

func on_hurtbox_collision(area: Area2D):
	particle_direction = area.global_position.direction_to(global_position)

func on_health_changed(before: float, after: float):
	if (before > after):
		flash_component.apply_flash()
		ParticleManager.spawn("hit_effect", global_position, particle_direction)
