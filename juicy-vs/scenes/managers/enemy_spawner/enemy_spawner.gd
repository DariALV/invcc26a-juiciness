class_name EnemySpawner extends Node

signal wave_started(index: int)
signal wave_completed(index: int)
signal waves_completed

## Node whose children the spawned enemies are added to.
@export var node_spawn_position: Node2D = null
## Waves played in order.
@export var waves: Array[Wave] = []
## Radius around the player at which enemies appear.
@export var spawn_distance: float = 50.0
## Pause in seconds between the end of a wave and the start of the next one.
@export var delay_between_waves: float = 0.0
## If true, loops back to the first wave after the last one finishes.
@export var loop_waves: bool = false
## Duracion maxima de cada oleada (s). Al agotarse, la oleada avanza si o si, de modo
## que las oleadas comienzan "cada minuto" (la 2 en el minuto 1, la 3 en el 2, ...).
## Una oleada tambien puede avanzar antes (ver _should_advance).
@export var wave_time_limit: float = 60.0
## Whether to start spawning automatically on _ready.
@export var auto_start: bool = true
## Crecimiento aditivo del multiplicador de dano de enemigos por oleada. Oleada 0 =
## x1, oleada 1 = x(1+esto), etc. Afecta el dano cuerpo a cuerpo y de proyectiles.
@export var damage_multiplier_per_wave: float = 0.25
## Crecimiento aditivo del multiplicador GLOBAL de vida de enemigos por oleada.
## Oleada 0 = x1, oleada 1 = x(1+esto), etc. Se aplica a todos los enemigos.
@export var health_multiplier_per_wave: float = 1.0
## Crecimiento aditivo del multiplicador de XP por oleada. Oleada 0 = x1, oleada 1 =
## x(1+esto), etc. (aditivo, no multiplicativo).
@export var xp_multiplier_per_wave: float = 0.25

var current_wave: int = 0

var _running: bool = false
var _elapsed: float = 0.0
var _accumulator: float = 0.0
var _threshold: float = 1.0
var _intermission: float = 0.0

func _ready():
	# Registra y captura las oleadas autoradas en GameConfig (fuente unica de
	# verdad) y luego usa las que devuelva la config.
	GameConfig.bind("spawner", self)
	GameConfig.bind_waves(self)
	waves = GameConfig.build_wave_resources()
	if auto_start:
		start()

func start():
	if waves.is_empty() or node_spawn_position == null:
		push_warning("EnemySpawner: missing 'waves' or 'node_spawn_position'.")
		return
	current_wave = 0
	_intermission = 0.0
	_begin_wave()
	_running = true

func stop():
	_running = false

## Reconstruye las oleadas desde GameConfig y las reinicia desde la primera.
func rebuild_and_restart() -> void:
	waves = GameConfig.build_wave_resources()
	start()

## Salta inmediatamente a la siguiente oleada (o termina si era la ultima).
func debug_skip_wave() -> void:
	if _running:
		_advance()

func _process(delta: float):
	if not _running:
		return
	if _intermission > 0.0:
		_intermission -= delta
		return

	var wave: Wave = waves[current_wave]
	_elapsed += delta
	var t: float = clampf(_elapsed / wave.duration, 0.0, 1.0)

	# La tasa de spawn depende de la poblacion viva (min/max de la oleada), no del
	# tiempo: a min_alive va al maximo, a max_alive baja a 0 (ver Wave.spawn_rate_for).
	_accumulator += wave.spawn_rate_for(GlobalData.enemies_alive) * delta
	while _accumulator >= _threshold:
		_accumulator -= _threshold
		_threshold = _roll_threshold(wave)
		if _is_full(wave):
			break
		for i in wave.batch_size(t):
			if _is_full(wave):
				break
			wave.spawn_one(node_spawn_position, spawn_distance, t)

	if _should_advance(wave):
		_advance()

## Avanza de oleada cuando se agota el tiempo limite (cadencia por minuto) o cuando ya
## se spawnearon todos los enemigos y la poblacion viva bajo al minimo de la oleada
## (inicio anticipado).
func _should_advance(wave: Wave) -> bool:
	# Nunca avanzar hasta haber spawneado TODA la oleada. Antes la oleada avanzaba solo
	# por tiempo, asi que si la poblacion estaba al tope (jugador demorado) los enemigos
	# nuevos -incluido el Rey, cuya oleada tiene max_alive=1- no llegaban a spawnear y la
	# partida se "ganaba" matando enemigos de oleadas viejas.
	if not wave.all_spawned():
		return false
	if _elapsed >= wave_time_limit:
		return true
	return GlobalData.enemies_alive <= wave.min_alive

func _is_full(wave: Wave) -> bool:
	return wave.max_alive > 0 and GlobalData.enemies_alive >= wave.max_alive

func _roll_threshold(wave: Wave) -> float:
	return randf_range(1.0 - wave.interval_jitter, 1.0 + wave.interval_jitter)

func _begin_wave():
	_elapsed = 0.0
	_accumulator = 0.0
	# Multiplicadores aditivos por oleada (dano y vida de enemigos, XP ganada).
	GlobalData.wave_damage_multiplier = 1.0 + damage_multiplier_per_wave * current_wave
	GlobalData.wave_health_multiplier = 1.0 + health_multiplier_per_wave * current_wave
	ExperienceManager.wave_xp_multiplier = 1.0 + xp_multiplier_per_wave * current_wave
	waves[current_wave].reset()
	_threshold = _roll_threshold(waves[current_wave])
	wave_started.emit(current_wave)

func _advance():
	wave_completed.emit(current_wave)
	if current_wave < waves.size() - 1:
		current_wave += 1
		_intermission = delay_between_waves
		_begin_wave()
	elif loop_waves:
		current_wave = 0
		_intermission = delay_between_waves
		_begin_wave()
	else:
		_running = false
		waves_completed.emit()
