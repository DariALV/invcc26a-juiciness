class_name DamageNumber extends Node2D

## Numero de dano flotante que aparece sobre un enemigo al recibir dano: sube,
## crece de escala, luego baja un poco y encoge a 0 antes de despawnear. El texto
## es blanco normalmente y naranja si el dano es de fuego (mismo tono que el modulate
## de fuego). Mantiene el estilo de los demas labels (outline negro).

const SCENE := preload("res://effects/damage_number.tscn")

## Naranja del dano de fuego (mismo tono que el tinte de fuego del enemigo).
const FIRE_COLOR := Color(1.0, 0.62, 0.32)
const NORMAL_COLOR := Color(1, 1, 1)
## Celeste claro (tono de congelado) para textos como "Esquivado".
const DODGE_COLOR := Color(0.6, 0.85, 1.0)

@onready var label: Label = $Label

## Crea un numero de dano en 'parent' (mundo) en 'pos'. 'amount' se muestra con hasta
## 2 decimales (1.5, 0.75) y 'is_fire' lo pinta de naranja.
static func spawn(parent: Node, pos: Vector2, amount: float, is_fire: bool) -> void:
	spawn_text(parent, pos, _format_static(amount), FIRE_COLOR if is_fire else NORMAL_COLOR)

## Crea un texto flotante arbitrario (p. ej. "Esquivado") con el mismo estilo y
## tween que los numeros de dano.
static func spawn_text(parent: Node, pos: Vector2, text: String, color: Color) -> void:
	if parent == null or not SCENE.can_instantiate():
		return
	var n: DamageNumber = SCENE.instantiate()
	parent.add_child(n)
	n.global_position = pos
	n.setup_text(text, color)

func setup(amount: float, is_fire: bool) -> void:
	setup_text(_format(amount), FIRE_COLOR if is_fire else NORMAL_COLOR)

func setup_text(text: String, color: Color) -> void:
	label.text = text
	label.add_theme_color_override("font_color", color)
	# Pequeno desplazamiento horizontal aleatorio para que no se solapen.
	position.x += randf_range(-6.0, 6.0)
	_animate()

## Version estatica del formateo, para usar antes de instanciar.
static func _format_static(amount: float) -> String:
	var s := "%.2f" % amount
	if s.contains("."):
		s = s.rstrip("0").rstrip(".")
	return s

## Formatea el dano con hasta 2 decimales, sin ceros finales (1.5, 0.75, 2, 1.33).
func _format(amount: float) -> String:
	var s := "%.2f" % amount
	if s.contains("."):
		s = s.rstrip("0").rstrip(".")
	return s

func _animate() -> void:
	var start_y := position.y
	scale = Vector2(0.4, 0.4)
	var t := create_tween()
	# Sube y crece.
	t.set_parallel(true)
	t.tween_property(self, "position:y", start_y - 18.0, 0.3) \
		.set_trans(Tween.TRANS_QUAD).set_ease(Tween.EASE_OUT)
	t.tween_property(self, "scale", Vector2(2, 2), 0.3) \
		.set_trans(Tween.TRANS_BACK).set_ease(Tween.EASE_OUT)
	# Luego baja un poquito y encoge a 0.
	t.chain().set_parallel(true)
	t.tween_property(self, "position:y", start_y - 10.0, 1) \
		.set_trans(Tween.TRANS_QUAD).set_ease(Tween.EASE_IN)
	t.tween_property(self, "scale", Vector2.ZERO, 1) \
		.set_trans(Tween.TRANS_BACK).set_ease(Tween.EASE_IN)
	t.chain().tween_callback(queue_free)
