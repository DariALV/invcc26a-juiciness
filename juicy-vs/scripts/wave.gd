class_name Wave extends Resource

@export_group("Composition")
## Enemy entries that make up this wave.
@export var enemies: Array[WaveEnemy] = []

@export_group("Timing")
## Wave length in seconds. Also the time axis sampled by the curves.
@export var duration: float = 30.0
## Base spawns per second. The rate curve multiplies this moment to moment.
@export var base_spawn_rate: float = 2.0
## Multiplies base_spawn_rate over progress (X = 0..1, Y = factor). Flat = constant,
## ramp = rising, bell = mid climax, late spike = overwhelms near the end. Null = flat.
@export var spawn_rate_curve: Curve
## Enemies per spawn tick over progress (Y = count). Null = 1 per tick.
@export var spawn_amount_curve: Curve
## Random jitter of the interval between spawns (0 = metronomic, 0.5 = +-50%).
@export_range(0.0, 1.0) var interval_jitter: float = 0.2

@export_group("Population")
## If fewer enemies than this are alive, extra spawns are forced to keep pressure. 0 = off.
@export var min_alive: int = 0
## Cap of enemies alive at once. <= 0 = no cap.
@export var max_alive: int = 0

@export_group("Progression")
## If true, the wave ends only once its duration elapses AND all enemies have spawned.
## If false, elapsing the duration is enough (anything unspawned is dropped).
@export var require_all_spawned: bool = false

func reset() -> void:
	for e in enemies:
		e.reset()

func current_rate(t: float) -> float:
	var factor := spawn_rate_curve.sample(t) if spawn_rate_curve else 1.0
	return base_spawn_rate * factor

## Tasa de spawn segun la cantidad de enemigos vivos, interpolando linealmente:
##   alive = min_alive  -> base_spawn_rate (maximo)
##   alive = max_alive  -> 0
## Si hay menos de min_alive, extrapola (t > 1): la tasa sigue creciendo por encima
## del maximo para arrancar la oleada mas rapido. Si no hay tope util (max_alive <=
## min_alive) se usa base_spawn_rate constante.
func spawn_rate_for(alive: int) -> float:
	if max_alive <= 0 or max_alive <= min_alive:
		return base_spawn_rate
	var rate := base_spawn_rate * float(max_alive - alive) / float(max_alive - min_alive)
	return maxf(0.0, rate)

func batch_size(t: float) -> int:
	if spawn_amount_curve:
		return maxi(1, roundi(spawn_amount_curve.sample(t)))
	return 1

func all_spawned() -> bool:
	for e in enemies:
		if e.can_spawn():
			return false
	return true

func is_completed(elapsed: float) -> bool:
	var time_done := elapsed >= duration
	if require_all_spawned:
		return time_done and all_spawned()
	return time_done

func spawn_one(parent_node: Node2D, spawn_distance: float, t: float) -> bool:
	var pool: Array[WaveEnemy] = []
	var total_weight := 0.0
	for e in enemies:
		if e.can_spawn() and e.is_in_window(t):
			pool.append(e)
			total_weight += e.weight
	if pool.is_empty() or total_weight <= 0.0:
		return false
	var roll := randf() * total_weight
	for e in pool:
		roll -= e.weight
		if roll <= 0.0:
			e.spawn(parent_node, spawn_distance)
			return true
	return false
