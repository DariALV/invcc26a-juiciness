class_name StatusEffectComponent extends Node

## Gestiona efectos de estado temporales sobre una entidad (enemigo): congelacion
## (ralentiza via MovementComponent.speed_scale) y quemadura (dano periodico).
## Ademas tinta el sprite mientras el efecto esta activo, con tonos pastel: celeste
## para congelacion y naranja para fuego.
##
## El tinte se aplica a traves del FlashComponent (su parametro de shader 'tint'),
## NO via 'modulate': el sprite usa un shader propio que escribe COLOR directamente
## e ignora 'modulate', por lo que ese era el unico canal que se ve. El flash de
## golpe regresa a este tinte de estado en vez de a transparente.
##
## El proyectil lo crea bajo demanda como hijo de la entidad golpeada, asi que no
## hace falta agregarlo a cada escena de enemigo: encuentra MovementComponent,
## HealthComponent y el FlashComponent entre/bajo los hijos de su padre.

## Celeste pastel mientras la entidad esta congelada (el alpha es la intensidad del
## tinte sobre la textura).
const FREEZE_TINT := Color(0.6, 0.85, 1.0, 0.55)
## Naranja pastel mientras la entidad se quema.
const BURN_TINT := Color(1.0, 0.76, 0.5, 0.55)
## Sin tinte de estado.
const NO_TINT := Color(1, 1, 1, 0)

var _movement: MovementComponent
var _health: HealthComponent
var _flash: Node

var _freeze_timer: float = 0.0

var _burn_timer: float = 0.0
var _burn_tick: float = 0.5
var _burn_damage_per_tick: float = 0.0
var _burn_accum: float = 0.0

func _ready() -> void:
	var parent := get_parent()
	for child in parent.get_children():
		if child is MovementComponent:
			_movement = child
		elif child is HealthComponent:
			_health = child
	_flash = parent.get_node_or_null("AnimatedSprite2D/FlashComponent")

## Congela: ralentiza al 'slow_factor' (0..1) durante 'duration' segundos. Si ya
## habia congelacion activa, se queda con la duracion mas larga.
func apply_freeze(duration: float, slow_factor: float) -> void:
	_freeze_timer = maxf(_freeze_timer, duration)
	if _movement:
		_movement.speed_scale = clampf(slow_factor, 0.0, 1.0)
	_update_tint()

## Quema: aplica 'damage_per_second' repartido en ticks de 'tick' segundos durante
## 'duration' segundos. Se queda con la duracion mas larga y el dano mas alto.
func apply_burn(duration: float, damage_per_second: float, tick: float) -> void:
	_burn_timer = maxf(_burn_timer, duration)
	_burn_tick = maxf(0.05, tick)
	_burn_damage_per_tick = maxf(_burn_damage_per_tick, damage_per_second * _burn_tick)
	_update_tint()

func _process(delta: float) -> void:
	if _freeze_timer > 0.0:
		_freeze_timer -= delta
		if _freeze_timer <= 0.0:
			if _movement:
				_movement.speed_scale = 1.0
			_update_tint()

	if _burn_timer > 0.0:
		_burn_timer -= delta
		_burn_accum += delta
		while _burn_accum >= _burn_tick:
			_burn_accum -= _burn_tick
			if _health and not _health.isDead:
				_health.apply_damage(_burn_damage_per_tick, true, "burn")
		if _burn_timer <= 0.0:
			_burn_damage_per_tick = 0.0
			_update_tint()

## Elige el tinte segun el efecto activo (congelacion tiene prioridad visual) y lo
## restaura cuando no hay ninguno. Escribe en el FlashComponent.
func _update_tint() -> void:
	if _flash == null or not is_instance_valid(_flash):
		return
	var tint: Color
	if _freeze_timer > 0.0:
		tint = FREEZE_TINT
	elif _burn_timer > 0.0:
		tint = BURN_TINT
	else:
		tint = NO_TINT
	if _flash.has_method("set_base_tint"):
		_flash.set_base_tint(tint)
