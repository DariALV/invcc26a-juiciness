class_name HealthBar extends ProgressBar

@onready var fill: ColorRect = $Fill

var tween: Tween

func _ready():
	step = 0
	# El relleno propio del ProgressBar esta deshabilitado (StyleBoxEmpty); el
	# llenado lo dibuja el shader del ColorRect 'Fill', que toma sub-pixel suave.
	# Sincronizamos su 'progress' con el value que tweenea.
	value_changed.connect(_update_fill)
	_update_fill(value)
	EventBus.player_health_changed.connect(on_health_changed)

func on_health_changed(before: float, after: float):
	if LevelManager.player:
		max_value = LevelManager.player.health.max_health
	reset_tween()
	# Arranca rapido y desacelera (baja de golpe y se va frenando).
	tween.tween_property(self, "value", after, 2) \
		.set_trans(Tween.TRANS_CIRC) \
		.set_ease(Tween.EASE_OUT)

func _update_fill(v: float):
	if fill and max_value > 0:
		fill.material.set_shader_parameter("progress", v / max_value)

func reset_tween():
	if tween:
		tween.kill()
	tween = create_tween()
