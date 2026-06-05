extends CharacterBody2D
class_name Archer

@onready var bow = $Bow

@onready var animated_sprite : AnimatedSprite2D = $AnimatedSprite2D
@onready var movement : MovementComponent = $MovementComponent
@onready var health: HealthComponent = $HealthComponent
@onready var flash_component = $AnimatedSprite2D/FlashComponent
@onready var hurtbox: HurtboxComponent = $HurtboxComponent

## Experience granted by this enemy on death.
@export var xp_value: float = 1.0

var particle_direction: Vector2 = Vector2(1, 0)

func _ready():
	health.died.connect(on_died)
	health.health_changed.connect(on_health_changed)
	health.damaged.connect(on_damaged)
	hurtbox.collision.connect(on_hurtbox_collision)
	GameConfig.bind("archer", self)
	GameConfig.bind_steering("archer", self)
	var hitbox = get_node_or_null("HitboxComponent")
	if hitbox and GlobalData.wave_damage_multiplier != 1.0:
		hitbox.damage *= GlobalData.wave_damage_multiplier
	

func _process(delta):
	var player = LevelManager.player
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

func on_died():
	AudioManager.play_enemy_death()
	# La experiencia ya no se otorga al instante: se suelta como un orbe recogible.
	XpOrb.drop(global_position, xp_value)
	var p := LevelManager.player
	if is_instance_valid(p):
		p.on_enemy_killed()
		p.call_deferred("try_spawn_death_arrows", global_position)
	call_deferred("queue_free")

func on_hurtbox_collision(area: Area2D):
	particle_direction = area.global_position.direction_to(global_position)

func on_health_changed(before: float, after: float):
	if (before > after):
		flash_component.apply_flash()
		ParticleManager.spawn("hit_effect", global_position, particle_direction)
		AudioManager.play_hit()

## Muestra un numero de dano flotante (naranja si es fuego) sobre el arquero.
func on_damaged(amount: float, is_fire: bool):
	DamageNumber.spawn(LevelManager.y_sort_entities, global_position, amount, is_fire)

func _enter_tree():
	NodeCounter.add_entity("base enemy")

func _exit_tree():
	NodeCounter.remove_entity("base enemy")

func rotate_bow(direction: Vector2):
	var distance = 8
	bow.rotation = direction.angle()
	bow.position = direction * distance
	bow.shoot_direction = direction
