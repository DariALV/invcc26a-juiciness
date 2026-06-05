extends Node2D

## Dibuja el contorno (sin relleno) del area de recogida/ataque del jugador, tomando
## el radio del CollectorComponent (que crece con las mejoras de iman). Se coloca en
## un z absoluto bajo para quedar SOBRE el suelo pero DEBAJO de todas las entidades
## de YSortEntities (que estan en z >= 10).

## Color y transparencia del circulo del area. El cuarto componente (alfa) controla
## la transparencia: 0 = invisible, 1 = opaco. Editable tambien por-instancia en el
## nodo AreaIndicator del jugador (player.tscn).
@export var color: Color = Color(0.45, 0.8, 1.0, 0.04)
@export var line_width: float = 1.0

var _radius: float = 0.0

func _ready() -> void:
	# z absoluto entre el suelo (0) y las entidades (10): se ve como una marca en el piso.
	z_as_relative = false
	z_index = 5

func _process(_delta: float) -> void:
	var r := _current_radius()
	if not is_equal_approx(r, _radius):
		_radius = r
		queue_redraw()

func _current_radius() -> float:
	var p := get_parent()
	if p and p.has_method("get_pickup_radius"):
		return p.get_pickup_radius()
	return 0.0

func _draw() -> void:
	if _radius > 0.0:
		draw_arc(Vector2.ZERO, _radius, 0.0, TAU, 64, color, line_width, true)
