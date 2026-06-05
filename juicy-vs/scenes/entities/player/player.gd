extends CharacterBody2D
class_name Player

@onready var health : HealthComponent = $HealthComponent
@onready var movement : MovementComponent = $MovementComponent

@onready var animated_sprite : AnimatedSprite2D = $AnimatedSprite2D

@onready var bow = $Bow
@onready var hurtbox: HurtboxComponent = $HurtboxComponent

## Vida que recupera el jugador por cada enemigo derrotado. La suben las mejoras de
## "Vampirismo".
@export var heal_on_kill: float = 0.0

@export_group("Defense")
## Probabilidad [0..1] de esquivar (anular) un golpe. La suben las mejoras de Esquiva.
@export var dodge_chance: float = 0.0
## Probabilidad [0..1] de reflejar un proyectil enemigo (lo vuelve aliado). Mejoras
## de Reflexion.
@export var reflect_chance: float = 0.0
## Segundos de invulnerabilidad tras recibir un golpe. Mejoras de Invulnerabilidad.
@export var invuln_time: float = 0.0
## Fuerza (desplazamiento) del knockback a enemigos cercanos al recibir un golpe.
@export var invuln_knockback_force: float = 0.0
## Rango del knockback al recibir un golpe.
@export var invuln_knockback_range: float = 0.0

@export_group("Aura de área")
## Daño que sufre cada entidad dentro del area de recogida en cada ataque del aura.
## 0 = sin aura. Lo suben las mejoras de "Aura".
@export var area_damage: float = 0.0
## Multiplicador de frecuencia del aura. 1.0 = 1 ataque por segundo (intervalo base
## de 1s); 2.0 = 2 por segundo. Lo suben las mejoras de "Pulso".
@export var area_attack_rate: float = 1.0
## Maximo de entidades simultaneas que el aura daña por ataque (las mas cercanas).
## Base 5. Lo suben las mejoras de "Alcance".
@export var area_max_targets: int = 5

@export_group("Death Arrows")
## Probabilidad [0..1] de que un enemigo, al morir, lance flechas en circulo. La
## suben las mejoras de "Venganza".
@export var death_arrow_chance: float = 0.0
## Cantidad de flechas del circulo.
@export var death_arrow_count: int = 8
## Velocidad de las flechas del circulo.
@export var death_arrow_speed: float = 140.0
## Multiplicador de dano de las flechas del circulo respecto al dano del jugador. Subido
## 0.4 -> 0.6 con datos: Venganza era el arma mas debil (441 dmg/run vs 3492 de arrow).
@export var death_arrow_damage_mult: float = 0.6
## Multiplicador de tamano de las flechas del circulo (mas pequenas).
@export var death_arrow_scale_mult: float = 0.6

func _ready() -> void:
	health.died.connect(on_died)
	health.health_changed.connect(on_health_changed)
	EventBus.add_player_health.connect(on_add_health)
	UpgradeManager.weapon_upgrade_taken.connect(on_weapon_upgrade_taken)
	# El hurtbox consulta al jugador para esquivar/reflejar/invulnerabilidad.
	hurtbox.defense = self
	# Capturamos quien nos golpea para etiquetar el DamageTaken en on_health_changed.
	hurtbox.collision.connect(on_hurtbox_collision)
	GameConfig.bind("player", self)

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
	_accumulate_movement(input_direction)
	_process_area(delta)

# --- Telemetria de movimiento (proxies de actividad / jitter) ---------------

## Acumuladores entre snapshots: cantidad de inputs direccionales, cambios bruscos
## de direccion (reversiones = jitter/panico) y distancia recorrida.
var _snap_inputs: int = 0
var _snap_dir_changes: int = 0
var _snap_distance: float = 0.0
var _last_move_dir: Vector2 = Vector2.ZERO
var _last_pos_snap: Vector2 = Vector2.ZERO
var _snap_pos_init: bool = false

## Acumula las metricas de movimiento del frame actual.
func _accumulate_movement(input_direction: Vector2) -> void:
	if not _snap_pos_init:
		_last_pos_snap = global_position
		_snap_pos_init = true
	_snap_distance += _last_pos_snap.distance_to(global_position)
	_last_pos_snap = global_position
	if input_direction != Vector2.ZERO:
		if _last_move_dir != Vector2.ZERO:
			if input_direction.dot(_last_move_dir) < 0.0:
				_snap_dir_changes += 1  # reversion brusca (jitter)
			if not input_direction.is_equal_approx(_last_move_dir):
				_snap_inputs += 1
		else:
			_snap_inputs += 1
		_last_move_dir = input_direction

## Devuelve los acumuladores de movimiento y los reinicia (lo llama el nivel al
## emitir cada snapshot).
func take_snapshot_deltas() -> Dictionary:
	var d := {"inputs": _snap_inputs, "dir_changes": _snap_dir_changes, "distance": _snap_distance}
	_snap_inputs = 0
	_snap_dir_changes = 0
	_snap_distance = 0.0
	return d

func rotate_bow():
	var direction: Vector2
	if GlobalData.auto_aim:
		# Apunta al enemigo vivo mas cercano; si no hay, mantiene la ultima direccion.
		var target := _nearest_enemy()
		if target:
			direction = (target.global_position - global_position).normalized()
		else:
			direction = bow.shoot_direction
	else:
		direction = get_local_mouse_position().normalized()
	if direction == Vector2.ZERO:
		direction = bow.shoot_direction
	var distance = 8
	bow.rotation = direction.angle()
	bow.position = direction * distance
	bow.shoot_direction = direction

## Enemigo vivo mas cercano al jugador (para el auto-aim). null si no hay.
func _nearest_enemy() -> Node2D:
	var best: Node2D = null
	var best_d := INF
	for e in get_tree().get_nodes_in_group("enemy"):
		if not is_instance_valid(e) or not (e is Node2D):
			continue
		var d: float = global_position.distance_squared_to(e.global_position)
		if d < best_d:
			best_d = d
			best = e
	return best

# --- Rango de recogida / area ----------------------------------------------

## Bono ADITIVO al rango de recogida/area (0.0 = solo el radio base). Lo suben las
## mejoras de iman. El radio efectivo escala el CollectorComponent por (1 + bono).
var pickup_range_bonus: float = 0.0

## Suma 'amount' al bono de rango (aditivo) y reescala el CollectorComponent.
func add_pickup_range(amount: float) -> void:
	pickup_range_bonus += amount
	var col := get_node_or_null("CollectorComponent") as Node2D
	if col:
		col.scale = Vector2.ONE * (1.0 + pickup_range_bonus)

# --- Aura de área ----------------------------------------------------------

var _area_accum: float = 0.0

## Radio efectivo del area de recogida/ataque: el radio del CollectorComponent
## escalado por su 'scale' (que aumentan las mejoras de iman).
func get_pickup_radius() -> float:
	var col := get_node_or_null("CollectorComponent") as Node2D
	if col == null:
		return 0.0
	var shape := col.get_node_or_null("CollisionShape2D") as CollisionShape2D
	if shape and shape.shape is CircleShape2D:
		return (shape.shape as CircleShape2D).radius * col.scale.x
	return 0.0

## Cada intervalo (1s / area_attack_rate) aplica 'area_damage' a toda entidad
## (enemigos y proyectiles perseguidores) dentro del area. No hace nada con daño 0.
func _process_area(delta: float) -> void:
	if area_damage <= 0.0 or health.isDead:
		return
	_area_accum += delta
	var interval := 1.0 / maxf(area_attack_rate, 0.01)
	if _area_accum < interval:
		return
	_area_accum = 0.0
	var r := get_pickup_radius()
	if r <= 0.0 or area_max_targets <= 0:
		return
	var r2 := r * r
	# Recolecta las entidades dentro del area (enemigos y proyectiles perseguidores).
	var candidates: Array = []
	for grp in ["enemy", "following_projectile"]:
		for e in get_tree().get_nodes_in_group(grp):
			if not is_instance_valid(e):
				continue
			var d: float = global_position.distance_squared_to(e.global_position)
			if d <= r2:
				candidates.append({"node": e, "d": d})
	# Solo las 'area_max_targets' mas cercanas reciben daño este ataque.
	if candidates.size() > area_max_targets:
		candidates.sort_custom(func(a, b): return a.d < b.d)
		candidates.resize(area_max_targets)
	for c in candidates:
		var hc := (c.node as Node).get_node_or_null("HealthComponent")
		if hc and hc.has_method("apply_damage"):
			hc.apply_damage(area_damage, false, "aura")


func on_add_health(amount: float):
	pass
	#health.apply_heal(amount)

## Ultimo HitboxComponent que golpeo al jugador (lo fija el hurtbox justo antes de
## aplicar el dano). Sirve para clasificar enemy_type/damage_type del DamageTaken.
var _last_incoming_hitbox: Area2D = null

func on_hurtbox_collision(area: Area2D) -> void:
	_last_incoming_hitbox = area

func on_health_changed(before: float, after: float):
	EventBus.player_health_changed.emit(before, after)
	if (before > after):
		Database.hits_taken += 1
		CameraJuice.shake(30)
		AudioManager.play_player_hit()
		var info := _classify_attacker(_last_incoming_hitbox)
		Database.log_damage_taken(info.enemy, info.type, before - after, before, after)
		_last_incoming_hitbox = null
	elif after > before:
		# Curacion efectiva (vampirismo / regen): proxy de sustain del build.
		Database.run_heals += 1

## Determina (enemy_type, damage_type) a partir del HitboxComponent que golpeo.
## Proyectil perseguidor -> necromancer/following_projectile; proyectil recto ->
## archer/arrow; contacto -> config_key del enemigo (o archer) / contact.
func _classify_attacker(hitbox: Area2D) -> Dictionary:
	var src: Node = hitbox.get_parent() if hitbox else null
	if src is FollowingProjectile:
		return {"enemy": "necromancer", "type": "following_projectile"}
	if src is BaseProjectile:
		# Las flechas del Rey y del arquero son ambas BaseProjectile; el Rey marca las
		# suyas con el grupo "king_projectile" para no contarlas como daño de arquero.
		if src.is_in_group("king_projectile"):
			return {"enemy": "king", "type": "arrow"}
		return {"enemy": "archer", "type": "arrow"}
	var enemy_name := "enemy"
	if src is Archer:
		enemy_name = "archer"
	elif src and "config_key" in src:
		enemy_name = src.config_key
	return {"enemy": enemy_name, "type": "contact"}

func on_weapon_upgrade_taken(upgrade: WeaponUpgrade):
	upgrade.apply_upgrade(bow)

## La invoca un enemigo al morir. Cura al jugador 'heal_on_kill' (mejoras de Vampirismo).
func on_enemy_killed() -> void:
	if heal_on_kill > 0.0 and not health.isDead:
		health.apply_heal(heal_on_kill)

## La invoca un enemigo al morir (de forma diferida). Con probabilidad
## 'death_arrow_chance', dispara un circulo de flechas debiles desde 'pos' que dañan
## a otros enemigos (heredan las mejoras de proyectil del jugador).
func try_spawn_death_arrows(pos: Vector2) -> void:
	if health.isDead or death_arrow_chance <= 0.0 or randf() >= death_arrow_chance:
		return
	var scene: PackedScene = bow.arrow_scene
	if scene == null or not scene.can_instantiate():
		return
	var container := LevelManager.y_sort_entities
	if container == null:
		return
	for i in death_arrow_count:
		var angle := TAU * float(i) / float(death_arrow_count)
		var dir := Vector2.RIGHT.rotated(angle)
		var arrow := scene.instantiate() as BaseProjectile
		if arrow == null:
			continue
		arrow.target_group = "enemy"
		container.add_child(arrow)
		arrow.damage_source = "death_arrows"
		arrow.global_position = pos
		arrow.rotation = angle
		if death_arrow_scale_mult != 1.0:
			arrow.scale *= death_arrow_scale_mult
		arrow.movement_component.max_speed = death_arrow_speed
		arrow.movement_component.current_speed = dir * death_arrow_speed
		# Heredan las mejoras de proyectil (dano, congelar, quemar, etc.) y luego se
		# debilitan con el multiplicador.
		UpgradeManager.apply_projectile_upgrades(arrow)
		arrow.damage *= death_arrow_damage_mult

# --- Defensa (lo consulta el HurtboxComponent) -----------------------------

## Probabilidad de esquivar: anula el golpe y muestra "Esquivado" en celeste.
func try_dodge() -> bool:
	if dodge_chance > 0.0 and randf() < dodge_chance:
		DamageNumber.spawn_text(LevelManager.y_sort_entities, global_position, \
			"Esquivado", DamageNumber.DODGE_COLOR)
		Database.run_dodges += 1
		return true
	return false

## Probabilidad de reflejar el proyectil enemigo (lo vuelve aliado contra enemigos).
func try_reflect(hitbox: Area2D) -> bool:
	if reflect_chance <= 0.0 or randf() >= reflect_chance:
		return false
	var proj := hitbox.get_parent()
	if proj is BaseProjectile:
		proj.reflect()
		return true
	return false

## Tras recibir un golpe efectivo: invulnerabilidad temporal y knockback a enemigos.
func on_damage_taken() -> void:
	if invuln_time <= 0.0 or health.invincible:
		return
	if invuln_knockback_force > 0.0 and invuln_knockback_range > 0.0:
		_apply_invuln_knockback()
	health.invincible = true
	get_tree().create_timer(invuln_time).timeout.connect(_end_invuln)

func _end_invuln() -> void:
	health.invincible = false

## Empuja a los enemigos dentro del rango, mas fuerte cuanto mas cerca.
func _apply_invuln_knockback() -> void:
	for e in get_tree().get_nodes_in_group("enemy"):
		if not is_instance_valid(e):
			continue
		var diff: Vector2 = e.global_position - global_position
		var dist := diff.length()
		if dist > 0.0 and dist < invuln_knockback_range:
			var falloff := 1.0 - dist / invuln_knockback_range
			e.global_position += diff.normalized() * invuln_knockback_force * falloff
