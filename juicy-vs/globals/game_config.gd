extends Node

## Fuente unica de verdad para TODOS los stats balanceables del juego.
##
## Centraliza salud, velocidad, fuerza, stats de flechas/proyectiles, steering
## behaviors, oleadas, experiencia y spawner en un solo autoload, de modo que se
## puedan leer y editar desde un unico lugar (incluido el menu de debug).
##
## Como funciona:
##  - Los stats escalares viven en `data` (group -> { field -> valor }) y se
##    inicializan desde GROUPS (defaults). Cada entidad, en su _ready(), llama a
##    `bind(group, self)` para aplicarse la config y quedar registrada; cuando la
##    config cambia (set_value) se re-aplica a todas las instancias vivas.
##  - Los steering behaviors son estructuras mas complejas (lista de behaviors,
##    cada uno con varios targets) y se capturan una vez desde la primera
##    instancia viva de cada tipo; luego la config manda y puede reconstruirlos.
##  - Las oleadas se capturan una vez desde el EnemySpawner del nivel.
##
## El menu de debug (autoload DebugMenu) genera su UI a partir de GROUPS y de
## estas estructuras, por lo que agregar un stat aqui lo expone automaticamente.

signal changed(group: String)
signal steering_changed(group: String)
signal waves_changed

## Catalogo de escenas de enemigos disponibles (para el editor de oleadas y para
## spawnear manualmente desde el menu de debug).
const ENEMY_CATALOG := {
	"Enemy": "res://scenes/entities/enemies/enemy/enemy.tscn",
	"Archer": "res://scenes/entities/enemies/archer/archer.tscn",
	"Necromancer": "res://scenes/entities/enemies/necromancer/necromancer.tscn",
	"Zombie": "res://scenes/entities/enemies/zombie/zombie.tscn",
	"Knight Attacker": "res://scenes/entities/enemies/knight_attacker/knight_attacker.tscn",
	"Knight Defender": "res://scenes/entities/enemies/knight_defender/knight_defender.tscn",
	"King (boss)": "res://scenes/entities/enemies/king/king.tscn",
}

## Definicion declarativa de cada grupo de stats escalares.
## Cada field: key, label, type ("float"/"int"/"bool"), node (ruta relativa a la
## entidad o "." para la raiz), prop (propiedad a escribir), default, min, max,
## step y opcionalmente "sync" (otras props que toman el mismo valor, p. ej.
## current_health = max_health).
const GROUPS := {
	"player": {
		"label": "Player",
		"fields": [
			{"key": "max_health", "label": "Max Health", "type": "float", "node": "HealthComponent", "prop": "max_health", "default": 50.0, "min": 1.0, "max": 9999.0, "step": 1.0, "sync": ["current_health"]},
			{"key": "max_speed", "label": "Move Speed", "type": "float", "node": "MovementComponent", "prop": "max_speed", "default": 100.0, "min": 0.0, "max": 2000.0, "step": 5.0},
			{"key": "max_force", "label": "Max Force", "type": "float", "node": "MovementComponent", "prop": "max_force", "default": 100.0, "min": 0.0, "max": 5000.0, "step": 5.0},
			{"key": "friction", "label": "Friction", "type": "float", "node": "MovementComponent", "prop": "friction", "default": 0.0, "min": 0.0, "max": 1.0, "step": 0.01},
			{"key": "bow_shoot_speed", "label": "Bow Shots/s", "type": "float", "node": "Bow", "prop": "shoot_speed", "default": 1.0, "min": 0.05, "max": 50.0, "step": 0.05},
			{"key": "bow_arrow_count", "label": "Arrows/Shot", "type": "int", "node": "Bow", "prop": "arrow_count", "default": 1, "min": 1, "max": 64, "step": 1},
			{"key": "bow_arrow_speed", "label": "Arrow Speed", "type": "float", "node": "Bow", "prop": "arrow_speed", "default": 500.0, "min": 10.0, "max": 4000.0, "step": 10.0},
		],
	},
	"enemy": {
		"label": "Enemy (basic)",
		"fields": [
			{"key": "max_health", "label": "Max Health", "type": "float", "node": "HealthComponent", "prop": "max_health", "default": 1.0, "min": 1.0, "max": 9999.0, "step": 1.0, "sync": ["current_health"]},
			{"key": "max_speed", "label": "Move Speed", "type": "float", "node": "MovementComponent", "prop": "max_speed", "default": 60.0, "min": 0.0, "max": 2000.0, "step": 5.0},
			{"key": "max_force", "label": "Max Force", "type": "float", "node": "MovementComponent", "prop": "max_force", "default": 160.0, "min": 0.0, "max": 5000.0, "step": 5.0},
			{"key": "friction", "label": "Friction", "type": "float", "node": "MovementComponent", "prop": "friction", "default": 0.99, "min": 0.0, "max": 1.0, "step": 0.01},
			{"key": "xp_value", "label": "XP Value", "type": "float", "node": ".", "prop": "xp_value", "default": 1.0, "min": 0.0, "max": 9999.0, "step": 1.0},
		],
	},
	"archer": {
		"label": "Archer",
		"fields": [
			{"key": "max_health", "label": "Max Health", "type": "float", "node": "HealthComponent", "prop": "max_health", "default": 1.0, "min": 1.0, "max": 9999.0, "step": 1.0, "sync": ["current_health"]},
			{"key": "max_speed", "label": "Move Speed", "type": "float", "node": "MovementComponent", "prop": "max_speed", "default": 60.0, "min": 0.0, "max": 2000.0, "step": 5.0},
			{"key": "max_force", "label": "Max Force", "type": "float", "node": "MovementComponent", "prop": "max_force", "default": 160.0, "min": 0.0, "max": 5000.0, "step": 5.0},
			{"key": "friction", "label": "Friction", "type": "float", "node": "MovementComponent", "prop": "friction", "default": 0.99, "min": 0.0, "max": 1.0, "step": 0.01},
			{"key": "xp_value", "label": "XP Value", "type": "float", "node": ".", "prop": "xp_value", "default": 1.0, "min": 0.0, "max": 9999.0, "step": 1.0},
			{"key": "bow_shoot_speed", "label": "Bow Shots/s", "type": "float", "node": "Bow", "prop": "shoot_speed", "default": 0.25, "min": 0.05, "max": 50.0, "step": 0.05},
		],
	},
	"necromancer": {
		"label": "Necromancer",
		"fields": [
			{"key": "max_health", "label": "Max Health", "type": "float", "node": "HealthComponent", "prop": "max_health", "default": 5.0, "min": 1.0, "max": 9999.0, "step": 1.0, "sync": ["current_health"]},
			{"key": "max_speed", "label": "Move Speed", "type": "float", "node": "MovementComponent", "prop": "max_speed", "default": 60.0, "min": 0.0, "max": 2000.0, "step": 5.0},
			{"key": "max_force", "label": "Max Force", "type": "float", "node": "MovementComponent", "prop": "max_force", "default": 160.0, "min": 0.0, "max": 5000.0, "step": 5.0},
			{"key": "friction", "label": "Friction", "type": "float", "node": "MovementComponent", "prop": "friction", "default": 0.99, "min": 0.0, "max": 1.0, "step": 0.01},
			{"key": "xp_value", "label": "XP Value", "type": "float", "node": ".", "prop": "xp_value", "default": 1.0, "min": 0.0, "max": 9999.0, "step": 1.0},
			{"key": "shoot_speed", "label": "Shots/s", "type": "float", "node": "ShootComponent", "prop": "shoot_speed", "default": 0.05, "min": 0.01, "max": 50.0, "step": 0.01},
			{"key": "projectile_speed", "label": "Projectile Speed", "type": "float", "node": "ShootComponent", "prop": "projectile_speed", "default": 300.0, "min": 10.0, "max": 4000.0, "step": 10.0},
			{"key": "projectile_count", "label": "Projectiles/Shot", "type": "int", "node": "ShootComponent", "prop": "projectile_count", "default": 1, "min": 1, "max": 64, "step": 1},
			{"key": "spawner_speed", "label": "Summon/s", "type": "float", "node": "SpawnerComponent", "prop": "speed", "default": 0.067, "min": 0.0, "max": 50.0, "step": 0.01},
			{"key": "spawner_count", "label": "Summon Count", "type": "int", "node": "SpawnerComponent", "prop": "spawn_count", "default": 3, "min": 0, "max": 64, "step": 1},
			{"key": "spawner_max", "label": "Summon Cap", "type": "int", "node": "SpawnerComponent", "prop": "max_entities", "default": 20, "min": 0, "max": 999, "step": 1},
		],
	},
	"zombie": {
		"label": "Zombie",
		"fields": [
			{"key": "max_health", "label": "Max Health", "type": "float", "node": "HealthComponent", "prop": "max_health", "default": 1.0, "min": 1.0, "max": 9999.0, "step": 1.0, "sync": ["current_health"]},
			{"key": "max_speed", "label": "Move Speed", "type": "float", "node": "MovementComponent", "prop": "max_speed", "default": 125.0, "min": 0.0, "max": 2000.0, "step": 5.0},
			{"key": "max_force", "label": "Max Force", "type": "float", "node": "MovementComponent", "prop": "max_force", "default": 125.0, "min": 0.0, "max": 5000.0, "step": 5.0},
			{"key": "friction", "label": "Friction", "type": "float", "node": "MovementComponent", "prop": "friction", "default": 0.99, "min": 0.0, "max": 1.0, "step": 0.01},
			{"key": "xp_value", "label": "XP Value", "type": "float", "node": ".", "prop": "xp_value", "default": 1.0, "min": 0.0, "max": 9999.0, "step": 1.0},
		],
	},
	"projectile": {
		"label": "Player Arrow",
		"fields": [
			{"key": "damage", "label": "Damage", "type": "float", "node": ".", "prop": "damage", "default": 1.0, "min": 0.0, "max": 9999.0, "step": 1.0},
			{"key": "pierce", "label": "Pierce", "type": "int", "node": ".", "prop": "pierce", "default": 1, "min": 1, "max": 999, "step": 1},
			{"key": "lifespan", "label": "Lifespan (s)", "type": "float", "node": ".", "prop": "lifespan", "default": 5.0, "min": 0.1, "max": 60.0, "step": 0.5},
		],
	},
	"experience": {
		"label": "Experience",
		"fields": [
			{"key": "max_level", "label": "Max Level", "type": "int", "node": ".", "prop": "max_level", "default": 50, "min": 1, "max": 999, "step": 1},
			{"key": "base_xp", "label": "Base XP", "type": "float", "node": ".", "prop": "base_xp", "default": 5.0, "min": 0.1, "max": 9999.0, "step": 0.5},
			{"key": "growth_factor", "label": "Growth Factor", "type": "float", "node": ".", "prop": "growth_factor", "default": 1.2, "min": 1.0, "max": 5.0, "step": 0.05},
			{"key": "base_luck", "label": "Base Luck", "type": "float", "node": ".", "prop": "base_luck", "default": 0.25, "min": 0.0, "max": 100.0, "step": 0.5},
		],
	},
	"spawner": {
		"label": "Spawner / Level",
		"fields": [
			{"key": "spawn_distance", "label": "Spawn Distance", "type": "float", "node": ".", "prop": "spawn_distance", "default": 500.0, "min": 10.0, "max": 4000.0, "step": 10.0},
			{"key": "delay_between_waves", "label": "Delay Between Waves", "type": "float", "node": ".", "prop": "delay_between_waves", "default": 0.0, "min": 0.0, "max": 60.0, "step": 0.5},
			{"key": "loop_waves", "label": "Loop Waves", "type": "bool", "node": ".", "prop": "loop_waves", "default": false},
		],
	},
}

## Stats escalares vigentes: group -> { field_key -> value }.
var data: Dictionary = {}

## Steering behaviors vigentes: group -> Array de
## { "type": "seek"/"flee", "targets": [ { "group", "radius", "force" } ] }.
var steering: Dictionary = {}
var _steering_captured: Dictionary = {}

## Oleadas vigentes: Array de
## { "duration", "base_spawn_rate", "max_alive", "min_alive", "interval_jitter",
##   "enemies": [ { "scene_path", "amount", "health_multiplier", "weight" } ] }.
var waves: Array = []
var _waves_captured: bool = false

## Flag global de invencibilidad del jugador (lo consulta HealthComponent).
var player_invincible: bool = false

## Registros de instancias vivas por grupo (para re-aplicar en caliente).
var _registry: Dictionary = {}
var _steering_registry: Dictionary = {}
var _spawner: Node = null

func _ready() -> void:
	for group in GROUPS:
		var d := {}
		for f in GROUPS[group].fields:
			d[f.key] = f.default
		data[group] = d

# --- Stats escalares -------------------------------------------------------

## Aplica la config del grupo a la entidad y la deja registrada para futuras
## re-aplicaciones en caliente. La llaman las entidades en su _ready().
func bind(group: String, node: Node) -> void:
	if not data.has(group):
		return
	apply(group, node)
	_register(_registry, group, node)
	node.tree_exiting.connect(_unregister.bind(_registry, group, node))

func apply(group: String, node: Node) -> void:
	var d: Dictionary = data.get(group, {})
	for f in GROUPS[group].fields:
		if not d.has(f.key):
			continue
		var target := _resolve(node, f.node)
		if target == null:
			continue
		var value = d[f.key]
		if f.type == "int":
			value = int(value)
		target.set(f.prop, value)
		if f.has("sync"):
			for sp in f.sync:
				target.set(sp, value)

func get_value(group: String, key: String):
	return data.get(group, {}).get(key)

func set_value(group: String, key: String, value) -> void:
	if not data.has(group):
		return
	data[group][key] = value
	changed.emit(group)
	for n in _registry.get(group, []):
		if is_instance_valid(n):
			apply(group, n)

func _resolve(node: Node, path: String) -> Object:
	if path == ".":
		return node
	return node.get_node_or_null(NodePath(path))

# --- Steering --------------------------------------------------------------

## Captura (una sola vez por grupo) los behaviors de la primera instancia viva,
## aplica la config vigente y registra la entidad para re-aplicar en caliente.
func bind_steering(group: String, node: Node) -> void:
	var force_comp := _find_force_component(node)
	if force_comp == null:
		return
	if not _steering_captured.get(group, false):
		steering[group] = _capture_steering(force_comp)
		_steering_captured[group] = true
		steering_changed.emit(group)
	else:
		force_comp.rebuild_behaviors(steering[group])
	_register(_steering_registry, group, node)
	node.tree_exiting.connect(_unregister.bind(_steering_registry, group, node))

func set_steering(group: String, defs: Array) -> void:
	steering[group] = defs
	_steering_captured[group] = true
	steering_changed.emit(group)
	for n in _steering_registry.get(group, []):
		if is_instance_valid(n):
			var fc := _find_force_component(n)
			if fc:
				fc.rebuild_behaviors(defs)

func has_steering(group: String) -> bool:
	return steering.has(group)

## Garantiza que exista una lista de behaviors para el grupo (para autorar desde
## cero en el menu de debug aunque aun no se haya spawneado ese enemigo).
func ensure_steering(group: String) -> void:
	if not steering.has(group):
		steering[group] = []
		_steering_captured[group] = true

func _capture_steering(force_comp: Node) -> Array:
	var result: Array = []
	for child in force_comp.get_children():
		if child is SteeringBehavior:
			var beh := child as SteeringBehavior
			var entry := {
				"type": "flee" if beh is FleeBehavior else "seek",
				"targets": [],
			}
			for t in beh.targets:
				entry.targets.append({
					"group": t.target_group,
					"radius": t.radius,
					"force": t.force_multiplier,
				})
			result.append(entry)
	return result

func _find_force_component(node: Node) -> ForceComponent:
	for child in node.get_children():
		if child is ForceComponent:
			return child as ForceComponent
	return null

# --- Oleadas ---------------------------------------------------------------

## Captura las oleadas autoradas en el EnemySpawner (una vez) y lo registra para
## poder reiniciarlas / reconstruirlas desde el menu de debug.
func bind_waves(spawner) -> void:
	_spawner = spawner
	if not _waves_captured:
		waves = _capture_waves(spawner.waves)
		_waves_captured = true
		waves_changed.emit()

func _capture_waves(source: Array) -> Array:
	var result: Array = []
	for w in source:
		var entry := {
			"duration": w.duration,
			"base_spawn_rate": w.base_spawn_rate,
			"max_alive": w.max_alive,
			"min_alive": w.min_alive,
			"interval_jitter": w.interval_jitter,
			"require_all_spawned": w.require_all_spawned,
			"enemies": [],
		}
		for e in w.enemies:
			entry.enemies.append({
				"scene_path": e.scene.resource_path if e.scene else "",
				"amount": e.amount,
				"health_multiplier": e.health_multiplier,
				"move_speed_multiplier": e.move_speed_multiplier,
				"weight": e.weight,
				"spawn_window_start": e.spawn_window_start,
				"spawn_window_end": e.spawn_window_end,
			})
		result.append(entry)
	return result

## Reconstruye recursos Wave a partir de la config (para que el EnemySpawner los use).
func build_wave_resources() -> Array[Wave]:
	var result: Array[Wave] = []
	for entry in waves:
		var w := Wave.new()
		w.duration = entry.duration
		w.base_spawn_rate = entry.base_spawn_rate
		w.max_alive = entry.max_alive
		w.min_alive = entry.min_alive
		w.interval_jitter = entry.interval_jitter
		w.require_all_spawned = entry.get("require_all_spawned", false)
		var enemies: Array[WaveEnemy] = []
		for ed in entry.enemies:
			var we := WaveEnemy.new()
			if ed.scene_path != "" and ResourceLoader.exists(ed.scene_path):
				we.scene = load(ed.scene_path) as PackedScene
			we.amount = ed.amount
			we.health_multiplier = ed.health_multiplier
			we.move_speed_multiplier = ed.get("move_speed_multiplier", 1.0)
			we.weight = ed.weight
			we.spawn_window_start = ed.get("spawn_window_start", 0.0)
			we.spawn_window_end = ed.get("spawn_window_end", 1.0)
			enemies.append(we)
		w.enemies = enemies
		result.append(w)
	return result

func get_spawner() -> Node:
	return _spawner if is_instance_valid(_spawner) else null

## Agrega una oleada nueva con valores por defecto (editor de debug).
func add_wave() -> void:
	waves.append({
		"duration": 30.0,
		"base_spawn_rate": 2.0,
		"max_alive": 25,
		"min_alive": 0,
		"interval_jitter": 0.2,
		"require_all_spawned": false,
		"enemies": [],
	})
	waves_changed.emit()

func remove_wave(index: int) -> void:
	if index >= 0 and index < waves.size():
		waves.remove_at(index)
		waves_changed.emit()

## Agrega una entrada de enemigo a una oleada (editor de debug).
func add_wave_enemy(wave_index: int) -> void:
	if wave_index < 0 or wave_index >= waves.size():
		return
	var first_path: String = ENEMY_CATALOG.values()[0]
	waves[wave_index].enemies.append({
		"scene_path": first_path,
		"amount": 10,
		"health_multiplier": 1.0,
		"weight": 1.0,
		"spawn_window_start": 0.0,
		"spawn_window_end": 1.0,
	})
	waves_changed.emit()

func remove_wave_enemy(wave_index: int, enemy_index: int) -> void:
	if wave_index < 0 or wave_index >= waves.size():
		return
	var list: Array = waves[wave_index].enemies
	if enemy_index >= 0 and enemy_index < list.size():
		list.remove_at(enemy_index)
		waves_changed.emit()

# --- Registro interno ------------------------------------------------------

func _register(reg: Dictionary, group: String, node: Node) -> void:
	if not reg.has(group):
		reg[group] = []
	if node not in reg[group]:
		reg[group].append(node)

func _unregister(reg: Dictionary, group: String, node: Node) -> void:
	if reg.has(group):
		reg[group].erase(node)
