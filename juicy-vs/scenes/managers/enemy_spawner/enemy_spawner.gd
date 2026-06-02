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
## Whether to start spawning automatically on _ready.
@export var auto_start: bool = true

var current_wave: int = 0

var _running: bool = false
var _elapsed: float = 0.0
var _accumulator: float = 0.0
var _threshold: float = 1.0
var _intermission: float = 0.0

func _ready():
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

func _process(delta: float):
	if not _running:
		return
	if _intermission > 0.0:
		_intermission -= delta
		return

	var wave: Wave = waves[current_wave]
	_elapsed += delta
	var t: float = clampf(_elapsed / wave.duration, 0.0, 1.0)

	_accumulator += wave.current_rate(t) * delta
	while _accumulator >= _threshold:
		_accumulator -= _threshold
		_threshold = _roll_threshold(wave)
		if _is_full(wave):
			break
		for i in wave.batch_size(t):
			if _is_full(wave):
				break
			wave.spawn_one(node_spawn_position, spawn_distance, t)

	while wave.min_alive > 0 and GlobalData.enemies_alive < wave.min_alive:
		if not wave.spawn_one(node_spawn_position, spawn_distance, t):
			break

	if wave.is_completed(_elapsed):
		_advance()

func _is_full(wave: Wave) -> bool:
	return wave.max_alive > 0 and GlobalData.enemies_alive >= wave.max_alive

func _roll_threshold(wave: Wave) -> float:
	return randf_range(1.0 - wave.interval_jitter, 1.0 + wave.interval_jitter)

func _begin_wave():
	_elapsed = 0.0
	_accumulator = 0.0
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
