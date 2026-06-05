class_name King extends BaseEnemy

## JEFE FINAL (ronda 5). Mucha vida, el único enemigo de la oleada: genera todas las
## demás unidades él mismo (5 SpawnerComponents, uno por tipo) y ataca al jugador con
## patrones de flechas que escalan por fases según su vida.
##
## Mecánicas:
##  - ESCUDO: es invulnerable mientras viva al menos un Knight Defender (grupo
##    "knight_defender"). Hay que limpiarlos para abrir ventanas de daño.
##  - LLAMADO A LAS ARMAS: al cruzar a fase 2 y 3 invoca de golpe un grupo de caballeros.
##  - ENRAGE (fase 3, <33% vida): se pone rojo y crece (tween no lineal), ataca más
##    seguido e invoca más rápido.
##  - LLUVIA DE FLECHAS (fase 3): caen flechas sobre la zona del jugador (telegrafiadas
##    por la flecha entrante).
##  - CADENA DE MUERTE: al morir, 9999 de daño a todo "enemy" -> mueren todos -> victoria.
##
## Los spawners y patrones se construyen por CÓDIGO, así king.tscn solo necesita la
## estructura visual base + buenos stats. Las flechas vuelan rectas (se les vacía el
## steering) para que abanico/círculo/lluvia mantengan su forma.

# --- Invocación: un SpawnerComponent por entrada (5 en total) ---
const SPAWN_DEFS := [
	{"key": "attacker", "path": "res://scenes/entities/enemies/knight_attacker/knight_attacker.tscn", "count": 3, "max": 5,  "interval": 7.0},
	{"key": "defender", "path": "res://scenes/entities/enemies/knight_defender/knight_defender.tscn", "count": 10, "max": 25,  "interval": 15},
	{"key": "zombie",   "path": "res://scenes/entities/enemies/zombie/zombie.tscn",                   "count": 15, "max": 30, "interval": 5.0},
	{"key": "archer",   "path": "res://scenes/entities/enemies/archer/archer.tscn",                   "count": 5, "max": 10, "interval": 7.0},
	{"key": "necromancer", "path": "res://scenes/entities/enemies/necromancer/necromancer.tscn",      "count": 2, "max": 5,  "interval": 13.0},
]

const HEALTH_BAR_SCENE: PackedScene = preload("res://scenes/ui/health_bar.tscn")
## Dorado de rareza legendaria (mismo tono que el contorno de las mejoras legendarias).
const HEALTH_BAR_COLOR := Color(1, 0.78, 0.25, 1)
## Mitad del ancho nativo de la barra (150 px) para centrarla, y separacion sobre la cabeza.
const HEALTH_BAR_HALF_W := 75.0
const HEALTH_BAR_GAP := 16.0
## Mitad del alto del sprite del Rey (24 px) en coords locales; con la escala da el tope real.
const KING_SPRITE_HALF_H := 12.0

const ARROW_SCENE_PATH := "res://scenes/entities/projectiles/arrow.tscn"
const ARROW_SPEED := 260.0
const DEATH_CHAIN_DAMAGE := 9999.0
# Enrage por fase: tamaño (sobre la escala BASE), tinte (vía el FlashComponent, porque el
# shader ignora 'modulate') e impulso de invocación. Fase 4 = todo al máximo.
const PHASE3_SCALE := 1.30
const PHASE4_SCALE := 1.65
const PHASE3_TINT := Color(1.0, 0.28, 0.22, 0.5)    # rojo (enfurecido)
const PHASE4_TINT := Color(1.0, 0.07, 0.12, 0.74)   # carmesí intenso (frenético)
const PHASE3_SPAWN_MULT := 1.6
const PHASE4_SPAWN_MULT := 2.4

enum Pattern { BURST, FAN, CIRCLE, RAIN, NOVA }

var _arrow_scene: PackedScene
var _attacker_scene: PackedScene
var _defender_scene: PackedScene
var _spawners: Array = []

var _ability_timer: float = 2.5
var _bursting: bool = false
var _burst_time: float = 0.0
var _burst_accum: float = 0.0

var _last_phase: int = 1
## Escala original del Rey (antes de crecer por fase), para escalar relativo a ella.
var _base_scale: Vector2 = Vector2.ONE

var _health_bar: HealthBar = null

func _ready() -> void:
	super()
	add_to_group("king")
	_base_scale = scale  # escala original, antes de cualquier crecimiento por fase
	if ResourceLoader.exists(ARROW_SCENE_PATH):
		_arrow_scene = load(ARROW_SCENE_PATH)
	_attacker_scene = _load_scene(SPAWN_DEFS[0].path)
	_defender_scene = _load_scene(SPAWN_DEFS[1].path)
	_build_spawners()
	_add_health_bar()

## Barra de vida dorada (rareza legendaria) sobre la cabeza del Rey. Reutiliza la barra
## del jugador, apuntada al HealthComponent del Rey. Se marca top_level para que NO herede
## la escala x2.2 del Rey (ni el crecimiento del enrage): asi se ve a tamano nativo, nitida
## y sin distorsion, igual que la del jugador. La seguimos a mano en _follow_health_bar.
func _add_health_bar() -> void:
	var bar: HealthBar = HEALTH_BAR_SCENE.instantiate()
	bar.health_component = health
	bar.fill_color = HEALTH_BAR_COLOR
	bar.top_level = true
	add_child(bar)
	_health_bar = bar
	_follow_health_bar()  # la coloca ya en el primer frame (evita un parpadeo en el origen)

## Centra la barra (en espacio global, a escala nativa) justo encima de la cabeza del Rey.
## La cabeza sube cuando el Rey crece (escala base + enrage), asi que el tope se calcula
## con la escala actual.
func _follow_health_bar() -> void:
	if not is_instance_valid(_health_bar):
		return
	var head_top := global_position.y - KING_SPRITE_HALF_H * scale.y
	_health_bar.global_position = Vector2(global_position.x - HEALTH_BAR_HALF_W, head_top - HEALTH_BAR_GAP)

func _load_scene(path: String) -> PackedScene:
	return load(path) if ResourceLoader.exists(path) else null

## Crea los 5 SpawnerComponents (uno por tipo de unidad) alrededor del Rey.
func _build_spawners() -> void:
	for d in SPAWN_DEFS:
		if not ResourceLoader.exists(d.path):
			push_warning("King: escena de invocación no encontrada: %s" % d.path)
			continue
		var sp := SpawnerComponent.new()
		sp.scene = load(d.path)
		sp.spawn_circle = Circle.new(Vector2.ZERO, 90.0)
		sp.spawn_area = SpawnerComponent.SpawnArea.EDGE
		sp.speed = 1.0 / float(d.interval)
		sp.spawn_count = int(d.count)
		sp.max_entities = int(d.max)
		sp.auto_start = true
		sp.set_meta("base_speed", sp.speed)  # para escalar la frecuencia por fase
		add_child(sp)
		_spawners.append(sp)

func _process(delta: float) -> void:
	super(delta)  # animación / flip del BaseEnemy
	_follow_health_bar()  # la barra es top_level: la mantenemos sobre la cabeza a mano
	if not health.isDead:
		# Escudo: invulnerable mientras viva algún defensor.
		health.invincible = _defenders_alive() > 0
		# Cambios de fase (la vida solo baja, así que solo sube de fase).
		var p := _phase()
		if p != _last_phase:
			_last_phase = p
			_on_phase_changed(p)
	_update_abilities(delta)

# --- Fases -----------------------------------------------------------------

## Fase según vida restante: 1 (>70%), 2 (45-70%), 3 (20-45%), 4 (<=20%, frenético).
func _phase() -> int:
	if health.max_health <= 0.0:
		return 1
	var ratio := health.current_health / health.max_health
	if ratio <= 0.20:
		return 4
	elif ratio <= 0.45:
		return 3
	elif ratio <= 0.70:
		return 2
	return 1

func _on_phase_changed(p: int) -> void:
	_summon_group(30 if p < 4 else 45)  # llamado a las armas (más en fase 4)
	if p >= 3:
		_apply_phase_enrage(p)

## Aplica el enrage de la fase: color (tinte del FlashComponent), tamaño y frecuencia de
## invocación. Es ABSOLUTO (no acumulativo), así que sirve igual aunque se salte una fase
## de un golpe fuerte (p. ej. 2 -> 4). El shader del flash ignora 'modulate', por eso el
## color se cambia con set_base_tint en vez de animated_sprite.modulate (ese era el bug).
func _apply_phase_enrage(p: int) -> void:
	# Color.
	var tint := Color(1, 1, 1, 0)
	if p >= 4:
		tint = PHASE4_TINT
	elif p == 3:
		tint = PHASE3_TINT
	flash_component.set_base_tint(tint)
	# Tamaño (relativo a la escala base, con tween elástico para juice).
	var target_scale := _base_scale
	if p >= 4:
		target_scale = _base_scale * PHASE4_SCALE
	elif p == 3:
		target_scale = _base_scale * PHASE3_SCALE
	var tw := create_tween()
	tw.tween_property(self, "scale", target_scale, 0.6) \
		.set_trans(Tween.TRANS_ELASTIC).set_ease(Tween.EASE_OUT)
	# Frecuencia de invocación (absoluta, sobre la velocidad base de cada spawner).
	var mult := 1.0
	if p >= 4:
		mult = PHASE4_SPAWN_MULT
	elif p == 3:
		mult = PHASE3_SPAWN_MULT
	for sp in _spawners:
		sp.speed = float(sp.get_meta("base_speed", sp.speed)) * mult
		sp.start()  # re-aplica la nueva frecuencia al timer

## Invoca de golpe 'n' caballeros (alternando atacante/defensor) en anillo.
func _summon_group(n: int) -> void:
	var container := LevelManager.y_sort_entities
	if container == null:
		return
	for i in n:
		var scn: PackedScene = _attacker_scene if i % 2 == 0 else _defender_scene
		if scn == null or not scn.can_instantiate():
			continue
		var k = scn.instantiate()
		container.add_child(k)
		if k is Node2D:
			k.global_position = global_position + Vector2.RIGHT.rotated(TAU * float(i) / float(n)) * 110.0

# --- Ataques ---------------------------------------------------------------

func _update_abilities(delta: float) -> void:
	if health.isDead:
		return
	var player := LevelManager.player
	if player == null or not is_instance_valid(player) or player.health.isDead:
		return

	if _bursting:
		_burst_time -= delta
		_burst_accum += delta
		# En fase 4 la ráfaga es más densa y se abre en abanico (más violenta).
		var heavy := _phase() >= 4
		var per_tick := 2 if heavy else 1
		var spread := 14.0 if heavy else 0.0
		while _burst_accum >= 0.1:        # 10 ticks por segundo
			_burst_accum -= 0.1
			_fire_toward_player(per_tick, spread)
		if _burst_time <= 0.0:
			_bursting = false
		return

	_ability_timer -= delta
	if _ability_timer <= 0.0:
		var phase := _phase()
		_ability_timer = _cooldown_for_phase(phase)
		_do_ability(phase)

func _cooldown_for_phase(phase: int) -> float:
	match phase:
		4: return 0.5
		3: return 0.75
		2: return 2
		_: return 3.2

func _do_ability(phase: int) -> void:
	var options: Array = []
	match phase:
		1: options = [Pattern.BURST]
		2: options = [Pattern.BURST, Pattern.FAN]
		3: options = [Pattern.BURST, Pattern.FAN, Pattern.CIRCLE, Pattern.RAIN]
		_: options = [Pattern.BURST, Pattern.FAN, Pattern.CIRCLE, Pattern.RAIN, Pattern.NOVA]
	# Fase 4: spamea DOS patrones a la vez.
	var casts := 2 if phase >= 4 else 1
	for _i in casts:
		_cast_pattern(options.pick_random(), phase)

## Lanza un patrón de ataque (más cargado en fase 4).
func _cast_pattern(pattern: int, phase: int) -> void:
	var heavy := phase >= 4
	match pattern:
		Pattern.BURST:
			_bursting = true
			_burst_time = 2.0
			_burst_accum = 0.0
		Pattern.FAN:
			_fire_fan(28 if heavy else 20, 140.0 if heavy else 120.0)
		Pattern.CIRCLE:
			_fire_circle(28 if heavy else 20)
		Pattern.RAIN:
			_fire_arrow_rain(14 if heavy else 8, 160.0 if heavy else 140.0)
		Pattern.NOVA:
			_fire_nova()

## NOVA (fase 4): doble anillo denso de flechas en todas direcciones (ataque "estallido").
func _fire_nova() -> void:
	_fire_circle(32)
	var inner := 22
	for i in inner:
		_spawn_arrow(Vector2.RIGHT.rotated(TAU * (float(i) + 0.5) / float(inner)))

func _fire_fan(count: int, spread_deg: float) -> void:
	var base := _dir_to_player().angle()
	var spread := deg_to_rad(spread_deg)
	var step := spread / float(maxi(count - 1, 1))
	var start := -spread / 2.0
	for i in count:
		_spawn_arrow(Vector2.RIGHT.rotated(base + start + i * step))

func _fire_circle(count: int) -> void:
	for i in count:
		_spawn_arrow(Vector2.RIGHT.rotated(TAU * float(i) / float(count)))

func _fire_toward_player(count: int, spread_deg: float) -> void:
	var base := _dir_to_player().angle()
	for i in count:
		var off := 0.0
		if spread_deg > 0.0:
			off = deg_to_rad(randf_range(-spread_deg, spread_deg))
		_spawn_arrow(Vector2.RIGHT.rotated(base + off))

## Lluvia: caen flechas hacia abajo sobre puntos aleatorios cerca del jugador. La
## propia flecha entrante telegrafia dónde caerá.
func _fire_arrow_rain(count: int, radius: float) -> void:
	var player := LevelManager.player
	if player == null or not is_instance_valid(player):
		return
	var center: Vector2 = player.global_position
	for i in count:
		var point := center + Vector2.RIGHT.rotated(randf() * TAU) * randf_range(0.0, radius)
		_spawn_arrow_at(point + Vector2(0, -320), Vector2.DOWN, ARROW_SPEED)

func _dir_to_player() -> Vector2:
	var player := LevelManager.player
	if player and is_instance_valid(player):
		return global_position.direction_to(player.global_position)
	return Vector2.RIGHT

func _spawn_arrow(dir: Vector2) -> void:
	_spawn_arrow_at(global_position, dir, ARROW_SPEED)

## Instancia una flecha enemiga recta desde 'start' en la dirección 'dir' (mismo patrón
## probado que las "death arrows" del jugador). Le vacía el steering para que vuele recta.
func _spawn_arrow_at(start: Vector2, dir: Vector2, speed: float) -> void:
	if _arrow_scene == null or not _arrow_scene.can_instantiate():
		return
	var container := LevelManager.y_sort_entities
	if container == null:
		return
	var arrow := _arrow_scene.instantiate()
	arrow.target_group = "player"  # daña al jugador (antes de add_child)
	arrow.add_to_group("king_projectile")  # para distinguirlas del arquero en la telemetría
	container.add_child(arrow)
	if arrow is Node2D:
		arrow.global_position = start
		arrow.rotation = dir.angle()
	if arrow is CanvasItem:
		arrow.modulate = Color(1.0, 0.82, 0.2)  # dorado: distingue visualmente las del Rey
	var mv = arrow.get_node_or_null("MovementComponent")
	if mv:
		mv.max_speed = speed
		mv.current_speed = dir * speed
	for c in arrow.get_children():
		if c is ForceComponent:
			c.rebuild_behaviors([])

# --- Muerte: cadena que limpia el mapa -------------------------------------

func on_died() -> void:
	_kill_all_enemies()
	super()  # XP orb, audio, queue_free del BaseEnemy

func _kill_all_enemies() -> void:
	for e in get_tree().get_nodes_in_group("enemy"):
		if e == self or not is_instance_valid(e):
			continue
		var hc = e.get_node_or_null("HealthComponent")
		if hc and hc.has_method("apply_damage"):
			hc.apply_damage(DEATH_CHAIN_DAMAGE)

# --- Helpers ---------------------------------------------------------------

func _defenders_alive() -> int:
	return get_tree().get_nodes_in_group("knight_defender").size()
