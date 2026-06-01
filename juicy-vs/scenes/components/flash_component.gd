extends Node

@export var material: ShaderMaterial
@export var duration: float = 0.2

var tween: Tween

func _ready():
	material = material.duplicate()
	get_parent().material = material
	material.set_shader_parameter('tint',Color(1, 1, 1, 0))

func apply_flash(start_color: Color = Color(1, 1, 1, 1), end_color: Color = Color(1, 1, 1, 0)):
	material.set_shader_parameter('tint',start_color)
	reset_tween()
	tween.tween_property(material, "shader_parameter/tint", end_color, duration)

func reset_tween():
	if tween:
		tween.kill()
	tween = create_tween()
