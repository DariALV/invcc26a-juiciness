class_name ShootComponent extends Node

## Fires projectiles in a fan toward a direction, periodically.
##
## Holds the shooting logic so any weapon or entity (a bow, a necromancer, a
## turret) can shoot without duplicating it. The owner just sets 'shoot_direction'
## and optionally calls shoot()/start()/stop().

signal shot(projectile: BaseProjectile)

## Scene instanced per projectile. Must inherit BaseProjectile.
@export var projectile_scene: PackedScene
## Shots per second. Updating it live also updates the internal timer.
@export var shoot_speed: float = 1.0:
	set(value):
		shoot_speed = value
		if _timer and value > 0:
			_timer.wait_time = 1.0 / value
## Projectiles per shot (spread across the fan).
@export var projectile_count: int = 1
## Speed of the projectiles.
@export var projectile_speed: float = 100.0
## Tinte (modulate) aplicado al sprite del proyectil al dispararlo. Blanco = sin
## cambio. Sirve para distinguir disparos por fuente (p. ej. flechas de arquero en rojo).
@export var projectile_modulate: Color = Color.WHITE
## Target group: "enemy" = player projectile, "player" = enemy projectile.
@export var target_group: String = "enemy"
## Degrees of separation between consecutive projectiles in the fan.
@export var spread_per_projectile: float = 4.0
## Whether it fires automatically with its internal timer once ready.
@export var auto_start: bool = true
## Parent of the projectiles. If null, LevelManager.y_sort_entities is used.
@export var projectile_container: Node = null

## Direction it aims at. Set by the owner before shooting.
var shoot_direction: Vector2 = Vector2.RIGHT

var _origin: Node2D
var _timer: Timer
var _spread_circle: Circle = Circle.new()

func _ready():
	assert(get_parent() is Node2D, "ShootComponent parent must be a Node2D")
	_origin = get_parent()
	_timer = Timer.new()
	_timer.timeout.connect(shoot)
	add_child(_timer)
	if auto_start:
		start()

func start():
	if shoot_speed <= 0:
		push_warning("ShootComponent: 'shoot_speed' must be > 0 to auto-fire.")
		return
	_timer.wait_time = 1.0 / shoot_speed
	_timer.start()

func stop():
	_timer.stop()

## Fires a burst of 'projectile_count' projectiles in a fan toward shoot_direction.
func shoot():
	if projectile_scene == null or not projectile_scene.can_instantiate():
		return
	var container := _get_container()
	if container == null:
		return
	var spread := spread_per_projectile * (projectile_count - 1)
	var directions := _spread_circle.spread_points_in_edge(projectile_count, spread, shoot_direction.angle())
	for dir in directions:
		_spawn_projectile(dir, container)
	# Solo el disparo del jugador (proyectiles que dañan enemigos) suena y da el
	# retroceso de camara opuesto a la mira (respeta el flag de recoil de Supabase).
	if target_group == "enemy":
		AudioManager.play_shoot()
		CameraJuice.recoil(-shoot_direction)

func _spawn_projectile(dir: Vector2, container: Node):
	var projectile : BaseProjectile = projectile_scene.instantiate() as BaseProjectile
	if projectile == null:
		push_warning("ShootComponent: 'projectile_scene' must inherit BaseProjectile.")
		return
	projectile.target_group = target_group
	container.add_child(projectile)
	projectile.global_position = _origin.global_position
	projectile.rotation = dir.angle()
	projectile.modulate = projectile_modulate
	projectile.movement_component.max_speed = projectile_speed
	projectile.movement_component.current_speed = dir * projectile_speed
	# Only player projectiles (target enemy) inherit the upgrades.
	if target_group == "enemy":
		UpgradeManager.apply_projectile_upgrades(projectile)
		# Tirada de critico una vez por flecha, sobre el dano ya mejorado.
		if projectile.crit_chance > 0.0 and randf() < projectile.crit_chance:
			projectile.damage *= projectile.crit_multiplier
	shot.emit(projectile)

func _get_container() -> Node:
	if projectile_container:
		return projectile_container
	if LevelManager.y_sort_entities:
		return LevelManager.y_sort_entities
	return _origin.get_parent()
