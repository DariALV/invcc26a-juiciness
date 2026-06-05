class_name BaseProjectile extends CharacterBody2D

@export var lifespan: float = 5
## Dano del proyectil. Es el HitboxComponent quien lo aplica al impactar
## (el HurtboxComponent lee 'hitbox.damage'), asi que mantenemos ambos en sync:
## cualquier cambio de 'damage' (config global, upgrades) se refleja en el hitbox.
@export var damage: float = 1:
	set(value):
		damage = value
		if hitbox_component:
			hitbox_component.damage = value
## VESTIGIAL: el viejo contador de perforacion ya no se usa. Ahora la flecha "perfora"
## segun su DANO: atraviesa enemigos mientras le quede dano por infligir (ver
## on_collision_detected). Se conserva la propiedad para no romper config/escenas.
@export var pierce: int = 1
@export var target_group: String = "enemy"
## Canal de dano para la telemetria (UpgradeStats). Se propaga al HitboxComponent.
## Solo se atribuye en proyectiles del jugador (target_group == "enemy").
@export var damage_source: String = "arrow":
	set(value):
		damage_source = value
		if hitbox_component:
			hitbox_component.damage_source = value
## VESTIGIAL: antes reducia el dano por cada enemigo atravesado. Con la perforacion por
## dano ya NO se aplica penalti (el dano solo baja por la vida que la flecha gasta matando).
## Se conserva la propiedad para no romper config/escenas que la fijen.
@export var damage_falloff: float = 0.8

@export_group("Status Effects")
## Si es true, al impactar congela (ralentiza) al objetivo y lo tinta de celeste.
@export var freeze_enabled: bool = false
## Duracion de la congelacion en segundos.
@export var freeze_duration: float = 2.0
## Factor de velocidad mientras esta congelado (0..1; 0.4 = 40% de velocidad).
@export var freeze_slow_factor: float = 0.4
## Si es true, al impactar prende fuego al objetivo (dano por tiempo) y lo tinta
## de anaranjado.
@export var burn_enabled: bool = false
## Duracion de la quemadura en segundos.
@export var burn_duration: float = 3.0
## Dano por segundo de la quemadura.
@export var burn_dps: float = 1.0
## Intervalo entre ticks de dano de la quemadura.
@export var burn_tick: float = 0.5

@export_group("Critical")
## Probabilidad [0..1] de que la flecha sea critica (dano multiplicado). Se decide
## una vez al crearse la flecha, asi que vale para todos sus impactos.
@export var crit_chance: float = 0.0
## Multiplicador de dano cuando la flecha es critica.
@export var crit_multiplier: float = 2.0

@export_group("Chain")
## Si es true, tras cada impacto la flecha se redirige al enemigo no golpeado mas
## cercano (no agrega impactos: eso lo hace la penetracion; solo evita que la flecha
## vuele al vacio, asi cada penetracion aterriza en un blanco nuevo).
@export var chain_enabled: bool = false
## Radio maximo para buscar el siguiente enemigo al encadenar.
@export var chain_range: float = 220.0

@onready var hitbox_component: HitboxComponent = $HitboxComponent
@onready var movement_component: MovementComponent = $MovementComponent

## Enemigos ya golpeados por esta flecha (instance_id), para no re-encadenar a ellos.
var _chain_hit_ids: Dictionary = {}

func _ready():
	# Sincroniza el dano inicial de escena con el hitbox antes de cualquier ajuste.
	hitbox_component.damage = damage
	# Solo las flechas del jugador (target "enemy") toman la config global de
	# proyectil; los proyectiles enemigos conservan sus stats de escena.
	if target_group == "enemy":
		GameConfig.bind("projectile", self)
		# Solo el dano ofensivo del jugador se atribuye a un canal.
		hitbox_component.damage_source = damage_source
	if target_group == "player":
		hitbox_component.collision_layer = 4
		hitbox_component.collision_mask = 1
		# Los proyectiles enemigos escalan su dano con el multiplicador de la oleada.
		if GlobalData.wave_damage_multiplier != 1.0:
			damage *= GlobalData.wave_damage_multiplier
	hitbox_component.collision_detected.connect(on_collision_detected)
	# Conectamos siempre para las flechas del jugador (o proyectiles con efectos en
	# escena), porque freeze/burn se habilitan via upgrades DESPUES de este _ready.
	if target_group == "enemy" or freeze_enabled or burn_enabled:
		hitbox_component.hit_landed.connect(on_hit_landed)
	# process_always = false: el timer de vida se PAUSA con el juego (no corre durante
	# la pausa ni la seleccion de mejoras).
	await get_tree().create_timer(lifespan, false).timeout
	queue_free()

## Resuelve los efectos que dependen del impacto: robo de vida, estados (congelar/
## quemar) y encadenado. El critico no entra aqui: se decide al crear la flecha.
func on_hit_landed(hurtbox: Area2D):
	var entity := hurtbox.get_parent()
	if entity == null:
		return
	if freeze_enabled or burn_enabled:
		_apply_status(entity)
	if chain_enabled:
		_chain_hit_ids[entity.get_instance_id()] = true
		# Solo re-dirigimos si a la flecha aun le queda dano (sigue viva tras el impacto).
		if damage > 0.01:
			_redirect_to_next_enemy()

## Crea bajo demanda el StatusEffectComponent del enemigo y le aplica los estados.
func _apply_status(entity: Node):
	var status := entity.get_node_or_null("StatusEffectComponent") as StatusEffectComponent
	if status == null:
		status = StatusEffectComponent.new()
		status.name = "StatusEffectComponent"
		entity.add_child(status)
	if freeze_enabled:
		status.apply_freeze(freeze_duration, freeze_slow_factor)
	if burn_enabled:
		status.apply_burn(burn_duration, burn_dps, burn_tick)

## Reorienta la velocidad hacia el enemigo no golpeado mas cercano dentro del radio.
func _redirect_to_next_enemy():
	var nearest: Node2D = null
	var best := chain_range * chain_range
	for e in get_tree().get_nodes_in_group("enemy"):
		if not is_instance_valid(e) or _chain_hit_ids.has(e.get_instance_id()):
			continue
		var d: float = global_position.distance_squared_to(e.global_position)
		if d < best:
			best = d
			nearest = e
	if nearest:
		var speed := movement_component.current_speed.length()
		var dir := global_position.direction_to(nearest.global_position)
		movement_component.current_speed = dir * speed
		rotation = dir.angle()

## Convierte un proyectil enemigo en uno del jugador (reflejado): pasa a dañar
## enemigos, invierte su trayectoria y, si persigue (following projectile), su seek
## cambia de "player" a "enemy". Lo llama el jugador al reflejar una bala.
func reflect():
	target_group = "enemy"
	# Capa/mascara de las flechas del jugador: lo detectan los hurtbox de enemigos
	# (mask 8) y deja de detectarlo el del jugador (mask 6).
	hitbox_component.collision_layer = 8
	hitbox_component.collision_mask = 2
	hitbox_component.ignore_collision.clear()
	# Invierte la trayectoria (proyectiles rectos).
	movement_component.current_speed = -movement_component.current_speed
	if movement_component.current_speed != Vector2.ZERO:
		rotation = movement_component.current_speed.angle()
	# Following projectiles: el seek pasa de perseguir al jugador a perseguir enemigos.
	var force_comp := get_node_or_null("ForceComponent")
	if force_comp and force_comp.has_method("rebuild_behaviors"):
		force_comp.rebuild_behaviors([
			{"type": "seek", "targets": [{"group": "enemy", "radius": 1000.0, "force": 1.0}]},
			{"type": "flee", "targets": [{"group": "following_projectile", "radius": 20.0, "force": 1.0}]},
		])

## Perforacion por DANO: la flecha descuenta de su dano la vida que de verdad quito al
## objetivo (last_damage_dealt). Si aun le queda dano, atraviesa y sigue con ese dano
## restante; si no, desaparece. No hay penalti por atravesar: cada enemigo recibe hasta el
## dano restante de la flecha. Ej.: flecha de 10 vs enemigos de 3/4/5 de vida -> mata al de
## 3 (queda 7), mata al de 4 (queda 3), pega 3 al de 5 y desaparece.
func on_collision_detected():
	damage -= hitbox_component.last_damage_dealt
	if damage <= 0.01:
		queue_free()

func _enter_tree():
	NodeCounter.add_entity(projectile_name())

func _exit_tree():
	NodeCounter.remove_entity(projectile_name())

func projectile_name() -> String:
	return "projectile"
