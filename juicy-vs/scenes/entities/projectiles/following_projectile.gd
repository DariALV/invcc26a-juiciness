class_name FollowingProjectile extends BaseProjectile

## Proyectil perseguidor con vida propia: ademas de dañar al jugador (HitboxComponent
## heredado de BaseProjectile), puede ser destruido por las flechas del jugador. Para
## ello tiene un HurtboxComponent (detecta proyectiles del jugador) y un
## HealthComponent; al morir se libera.

@onready var health: HealthComponent = $HealthComponent
@onready var hurtbox: HurtboxComponent = $HurtboxComponent
@onready var flash_component = get_node_or_null("AnimatedSprite2D/FlashComponent")

func _ready():
	# BaseProjectile._ready configura el hitbox, aplica config y arma el timer de vida.
	super()
	health.died.connect(_on_died)
	health.health_changed.connect(_on_health_changed)

func _on_health_changed(before: float, after: float) -> void:
	if before > after:
		if flash_component:
			flash_component.apply_flash()
		ParticleManager.spawn("hit_effect", global_position)
		AudioManager.play_hit()

func _on_died() -> void:
	AudioManager.play_enemy_death()
	queue_free()

func projectile_name() -> String:
	return "following_projectile"
