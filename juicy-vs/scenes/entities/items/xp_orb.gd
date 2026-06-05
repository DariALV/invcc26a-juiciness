class_name XpOrb extends CharacterBody2D

## Orbe de experiencia que sueltan los enemigos al morir.
##
## El "area" del jugador (radio de recogida/ataque) define la distancia a la que el
## orbe REACCIONA: mientras el jugador esta dentro de ese radio, el orbe es atraido
## con fuerza hacia el. La recoleccion ocurre al TOCAR al jugador (no al entrar al
## area). Asi el area solo marca la zona de reaccion, y el tamano lo manda el jugador
## (get_pickup_radius, que crece con las mejoras de iman).

## Distancia (px) al jugador a la que se considera que el orbe lo "toca" y se recoge.
const COLLECT_RADIUS := 10.0

@export var xp_amount: float = 1.0

@onready var _seek := get_node_or_null("ForceComponent/SeekBehavior") as SeekBehavior

func _physics_process(_delta: float) -> void:
	var p := LevelManager.player
	if p == null or not is_instance_valid(p):
		return
	var d := global_position.distance_to(p.global_position)
	# Se recoge al tocar al jugador.
	if d <= COLLECT_RADIUS:
		ExperienceManager.add_xp(xp_amount)
		queue_free()
		return
	# El orbe reacciona (persigue con fuerza) solo mientras el jugador esta dentro de
	# su area: igualamos el radio del seek al radio de recogida/ataque del jugador.
	if _seek and not _seek.targets.is_empty():
		_seek.targets[0].radius = p.get_pickup_radius()

## Crea un orbe de XP en 'pos' que otorga 'amount' de experiencia, bajo el contenedor
## de entidades del nivel. No hace nada si 'amount' es <= 0 o no hay nivel registrado.
static func drop(pos: Vector2, amount: float) -> void:
	if amount <= 0.0:
		return
	var container := LevelManager.y_sort_entities
	if container == null:
		return
	var orb := preload("res://scenes/entities/items/xp_orb.tscn").instantiate() as XpOrb
	orb.xp_amount = amount
	container.add_child(orb)
	orb.global_position = pos
