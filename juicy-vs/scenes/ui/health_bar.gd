class_name HealthBar extends ProgressBar

@onready var fill: ColorRect = $Fill
@onready var value_label: Label = $ValueLabel

## Si se asigna, la barra sigue a este HealthComponent en lugar del jugador. Permite
## reutilizar la misma barra en otras entidades (p. ej. el Rey). Si queda null, mantiene
## el comportamiento original: escucha EventBus.player_health_changed y lee al jugador.
@export var health_component: HealthComponent
## Color del relleno (shader + ColorRect). Por defecto el rojo del jugador; el Rey la
## pinta de dorado (rareza legendaria).
@export var fill_color: Color = Color(1, 0.34219015, 0.28635707, 1)

## Vida objetivo hacia la que se mueve la barra cada frame. En vez de lanzar un tween por
## cada cambio (que con la regen fraccional se reiniciaba 60 veces por segundo y nunca
## animaba), la barra persigue este valor con un suavizado exponencial en _process: agil
## al subir (curacion/regen -> se ve llenarse fluido) y lento al bajar (dano -> drenaje).
var _target: float = 0.0
## Velocidad de suavizado (mayor = mas rapido) al curar y al recibir dano.
const HEAL_SMOOTH := 16.0
const DAMAGE_SMOOTH := 3.5

func _ready():
	step = 0
	# El ShaderMaterial es un sub-recurso compartido entre instancias de la escena: lo
	# duplicamos para que cada barra tenga su propio 'progress' y 'fill_color' sin pisarse
	# (ahora hay mas de una barra: jugador y Rey).
	fill.material = fill.material.duplicate()
	fill.material.set_shader_parameter("fill_color", fill_color)
	fill.color = fill_color
	# El relleno propio del ProgressBar esta deshabilitado (StyleBoxEmpty); el llenado lo
	# dibuja el shader del ColorRect 'Fill'. Sincronizamos su 'progress' con el 'value'.
	value_changed.connect(_update_fill)
	if health_component:
		# Modo entidad: la barra sigue al HealthComponent asignado.
		_bind_to_component()
	else:
		# Modo jugador (comportamiento original).
		EventBus.player_health_changed.connect(on_health_changed)
		# Inicializa el texto "actual/maxima" tras un frame, cuando el jugador ya esta
		# registrado en LevelManager (la vida solo cambia, y dispara el texto, despues).
		_init_label.call_deferred()
	_target = value
	_update_fill(value)

## Engancha la barra al HealthComponent asignado (modo entidad).
func _bind_to_component():
	max_value = health_component.max_health
	value = health_component.current_health
	_target = health_component.current_health
	_update_label(health_component.current_health, health_component.max_health)
	health_component.health_changed.connect(on_health_changed)

func _init_label():
	if LevelManager.player:
		var hc: HealthComponent = LevelManager.player.health
		max_value = hc.max_health
		value = hc.current_health
		_target = hc.current_health
		_update_label(hc.current_health, hc.max_health)
	else:
		_update_label(value, max_value)

## Vida maxima de la entidad seguida (el componente asignado o, en su defecto, el jugador).
func _current_max() -> float:
	if health_component:
		return health_component.max_health
	if LevelManager.player:
		return LevelManager.player.health.max_health
	return max_value

func on_health_changed(before: float, after: float):
	max_value = _current_max()
	_target = after
	_update_label(after, max_value)

## Mueve la barra hacia la vida objetivo con suavizado exponencial (rapido al inicio y
## desacelera). Mas agil al subir que al bajar, asi la regen se ve fluida frame a frame.
func _process(delta: float) -> void:
	if is_equal_approx(value, _target):
		return
	var rate := DAMAGE_SMOOTH if value > _target else HEAL_SMOOTH
	value = lerp(value, _target, 1.0 - exp(-rate * delta))
	# Engancha al objetivo cuando ya esta practicamente encima (evita arrastrar decimales).
	if absf(value - _target) < 0.05:
		value = _target

## Texto "actual/maxima". Muestra la vida real con 2 decimales cuando es fraccional (asi se
## ve subir la regen con precision), y como entero cuando es exacta (p. ej. "75/75").
func _update_label(current: float, maxv: float):
	if value_label:
		value_label.text = "%s/%s" % [_fmt_hp(current), _fmt_hp(maxv)]

func _fmt_hp(v: float) -> String:
	if is_equal_approx(v, round(v)):
		return str(int(round(v)))
	return "%.2f" % v

func _update_fill(v: float):
	if fill and max_value > 0:
		fill.material.set_shader_parameter("progress", v / max_value)
