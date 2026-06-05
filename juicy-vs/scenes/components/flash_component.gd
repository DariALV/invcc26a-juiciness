extends Node

@export var material: ShaderMaterial
@export var duration: float = 0.2

var tween: Tween
## Tinte persistente de estado (congelado=celeste, quemado=naranja). El flash de
## golpe vuelve a este color en vez de a transparente, para que el tinte de estado
## se mantenga entre golpes. (1,1,1,0) = sin tinte.
var base_tint: Color = Color(1, 1, 1, 0)

func _ready():
	material = material.duplicate()
	get_parent().material = material
	material.set_shader_parameter('tint', base_tint)

func apply_flash(start_color: Color = Color(1, 1, 1, 1)):
	material.set_shader_parameter('tint', start_color)
	reset_tween()
	# Vuelve al tinte de estado (transparente si no hay estado activo).
	tween.tween_property(material, "shader_parameter/tint", base_tint, duration)

## Fija el tinte de estado persistente al que regresa el flash. Si no hay un flash
## en curso, lo aplica de inmediato.
func set_base_tint(color: Color) -> void:
	base_tint = color
	if tween == null or not tween.is_running():
		material.set_shader_parameter('tint', base_tint)

func reset_tween():
	if tween:
		tween.kill()
	tween = create_tween()
