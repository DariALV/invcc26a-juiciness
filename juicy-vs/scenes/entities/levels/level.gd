extends Node2D

@onready var label = $CanvasLayer/Label
@onready var round_label: Label = $CanvasLayer/RoundLabel
@onready var enemies_label: Label = $CanvasLayer/EnemiesLabel
@onready var id = $CanvasLayer/Stats/VBoxContainer/ID
@onready var survival_time = $CanvasLayer/Stats/VBoxContainer/SurvivalTime
@onready var hits_taken = $CanvasLayer/Stats/VBoxContainer/HitsTaken
@onready var enemies_defeated = $CanvasLayer/Stats/VBoxContainer/EnemiesDefeated
@onready var restart_button: Button = $CanvasLayer/Stats/VBoxContainer/RestartButton
@onready var player: Player = $YSortEntities/Player
@onready var stats = $CanvasLayer/Stats
@onready var stats_box: VBoxContainer = $CanvasLayer/Stats/VBoxContainer
@onready var y_sort_entities = $YSortEntities
@onready var enemy_spawner: EnemySpawner = $EnemySpawner
@onready var game_timer: Timer = $GameTimer

@onready var ground_layer: TileMapLayer = $GroundLayer

## Cada cuanto se emite un snapshot de telemetria (segundos). 0.1 = 10 por segundo.
const SNAPSHOT_INTERVAL: float = 0.1
## Acumulador para disparar el snapshot periodico.
var _snapshot_accum: float = 0.0

## Margen (px) hacia adentro del borde del mapa que no puede cruzar el jugador.
const EDGE_MARGIN: float = 40.0
## Limites del area jugable en coordenadas globales (se calculan tras generar el mapa).
var _bounds_min: Vector2 = Vector2.ZERO
var _bounds_max: Vector2 = Vector2.ZERO
var _bounds_ready: bool = false

## Tiempo de supervivencia acumulado (segundos).
var elapsed_time: float = 0.0
## Se activa cuando el EnemySpawner termina la ultima oleada.
var waves_done: bool = false
## Evita finalizar la partida mas de una vez.
var finished: bool = false

func _ready():
	GameTimer.label = label
	LevelManager.register_level(y_sort_entities, player)
	AudioManager.play_music()
	player.health.died.connect(on_player_died)
	enemy_spawner.waves_completed.connect(on_waves_completed)
	restart_button.pressed.connect(on_restart_pressed)
	# Arranca la telemetria: genera el run_id y limpia buffers para esta partida.
	Database.start_run(Database.selected_id)
	# Calcula los limites del mapa (ya generado por WorldGenerator) para no salirse.
	_compute_bounds()
	# Boton de auto-aim en la esquina inferior derecha.
	#_add_autoaim_button()

## Crea el boton para alternar auto-aim (esquina inferior derecha del HUD).
func _add_autoaim_button() -> void:
	var btn := Button.new()
	btn.focus_mode = Control.FOCUS_NONE  # get_tree().call_deferred("change_scene_to_file", "res://scenes/entities/levels/level.tscn")no robar foco al teclado de movimiento
	btn.set_anchors_preset(Control.PRESET_BOTTOM_RIGHT)
	btn.offset_left = -168.0
	btn.offset_top = -52.0
	btn.offset_right = -12.0
	btn.offset_bottom = -12.0
	btn.text = _autoaim_label()
	btn.pressed.connect(func():
		GlobalData.auto_aim = not GlobalData.auto_aim
		btn.text = _autoaim_label()
	)
	$CanvasLayer.add_child(btn)

func _autoaim_label() -> String:
	return "Auto-aim: ON" if GlobalData.auto_aim else "Auto-aim: OFF"

func _process(delta):
	if finished:
		return
	# Mantiene al jugador dentro del mapa (el movimiento no usa fisica, asi que se
	# acota la posicion directamente).
	_clamp_player()
	elapsed_time += delta
	# Contexto para etiquetar los eventos de telemetria (tiempo y ronda actuales).
	Database.run_game_time = int(elapsed_time)
	Database.run_round = mini(enemy_spawner.current_wave + 1, enemy_spawner.waves.size())
	round_label.text = "Ronda %d/%d" % [enemy_spawner.current_wave + 1, enemy_spawner.waves.size()]
	# Conteo real de enemigos vivos (incluye invocados y arqueros), no solo los de oleada.
	var alive := NodeCounter.get_count("base enemy")
	enemies_label.text = "Enemigos: %d" % alive
	# Snapshot periodico de telemetria (heatmaps, dificultad, proxies de estres).
	_snapshot_accum += delta
	if _snapshot_accum >= SNAPSHOT_INTERVAL:
		_snapshot_accum = 0.0
		_emit_snapshot(alive)
	# Victoria: ultima oleada completada y sin NINGUN enemigo vivo (ni invocados).
	if waves_done and alive <= 0:
		win_level()

## Toma una foto del estado de la partida y la bufferiza en Database (cada 100ms).
func _emit_snapshot(enemies_alive: int) -> void:
	if player == null or not is_instance_valid(player):
		return
	var deltas := player.take_snapshot_deltas()
	var data := {
		"game_time_ms": int(elapsed_time * 1000.0),
		# Posicion y estado.
		"player_x": player.global_position.x,
		"player_y": player.global_position.y,
		"hp": player.health.current_health,
		"max_hp": player.health.max_health,
		"level": ExperienceManager.current_level,
		"xp": ExperienceManager.total_xp_earned,
		"round": Database.run_round,
		"kills_so_far": GlobalData.enemies_dead,
		# Presion y densidad.
		"enemies_alive": enemies_alive,
		"nearest_enemy_dist": _nearest_enemy_dist(),
		"projectiles_alive": NodeCounter.get_count("projectile"),
		"xp_orbs_alive": get_tree().get_nodes_in_group("xp_orb").size(),
		# Movimiento y apuntado.
		"speed": player.velocity.length(),
		"aim_angle": player.bow.rotation,
		"inputs_delta": deltas.inputs,
		"dir_changes_delta": deltas.dir_changes,
		"distance_moved": deltas.distance,
		# Rendimiento y modo de apuntado.
		"fps": Engine.get_frames_per_second(),
		"auto_aim": GlobalData.auto_aim,
		# Stats del build (las del menu de pausa).
		"damage": _current_damage(),
		"pierce": _current_pierce(),
		"regen": player.health.regen_per_second,
		"move_speed": player.movement.max_speed,
		"arrows": player.bow.arrow_count,
		"fire_rate": player.bow.shoot_speed,
		"arrow_speed": player.bow.arrow_speed,
		"heal_on_kill": player.heal_on_kill,
		"dodge_chance": player.dodge_chance,
		"reflect_chance": player.reflect_chance,
		"death_arrow_chance": player.death_arrow_chance,
		"area_damage": player.area_damage,
		"area_attack_rate": player.area_attack_rate,
		"area_max_targets": player.area_max_targets,
		"luck": ExperienceManager.luck,
		"xp_multiplier": ExperienceManager.xp_multiplier,
	}
	Database.log_snapshot(data)

## Dano de proyectil vigente: base de GameConfig + mejoras de proyectil acumuladas
## (misma logica que el menu de pausa).
func _current_damage() -> float:
	var dmg: float = float(GameConfig.get_value("projectile", "damage"))
	for u in UpgradeManager.projectile_upgrades:
		if u is DamageProjectileUpgrade:
			dmg = dmg * u.multiplier + u.flat_bonus
	return dmg

## Penetracion vigente: base + mejoras de penetracion acumuladas.
func _current_pierce() -> float:
	var pierce: float = float(GameConfig.get_value("projectile", "pierce"))
	for u in UpgradeManager.projectile_upgrades:
		if u is PierceProjectileUpgrade:
			pierce += u.extra_pierce
	return pierce

## Calcula el rectangulo jugable (en coords globales) a partir de las celdas que el
## WorldGenerator ya pinto en el GroundLayer. Se llama una vez tras generar el mapa.
func _compute_bounds() -> void:
	if ground_layer == null:
		return
	var r := ground_layer.get_used_rect()
	if r.size.x <= 0 or r.size.y <= 0:
		return
	# Centros de la celda min y la celda max, llevados a coordenadas globales.
	var c_min: Vector2 = ground_layer.to_global(ground_layer.map_to_local(r.position))
	var c_max: Vector2 = ground_layer.to_global(ground_layer.map_to_local(r.position + r.size - Vector2i.ONE))
	_bounds_min = Vector2(minf(c_min.x, c_max.x), minf(c_min.y, c_max.y))
	_bounds_max = Vector2(maxf(c_min.x, c_max.x), maxf(c_min.y, c_max.y))
	_bounds_ready = (_bounds_max.x - _bounds_min.x > 2.0 * EDGE_MARGIN) \
		and (_bounds_max.y - _bounds_min.y > 2.0 * EDGE_MARGIN)

## Acota la posicion del jugador al area jugable (con un margen hacia adentro).
func _clamp_player() -> void:
	if not _bounds_ready or player == null or not is_instance_valid(player):
		return
	var p := player.global_position
	p.x = clampf(p.x, _bounds_min.x + EDGE_MARGIN, _bounds_max.x - EDGE_MARGIN)
	p.y = clampf(p.y, _bounds_min.y + EDGE_MARGIN, _bounds_max.y - EDGE_MARGIN)
	player.global_position = p

## Distancia al enemigo vivo mas cercano (presion espacial). -1 si no hay enemigos.
func _nearest_enemy_dist() -> float:
	var best := -1.0
	var pp: Vector2 = player.global_position
	for e in get_tree().get_nodes_in_group("enemy"):
		if not is_instance_valid(e):
			continue
		var d: float = pp.distance_to(e.global_position)
		if best < 0.0 or d < best:
			best = d
	return best

## Retira todos los orbes de XP del nivel (al ganar o perder) para que no se sigan
## recogiendo despues de terminar la partida.
func _clear_xp_orbs():
	for o in get_tree().get_nodes_in_group("xp_orb"):
		o.queue_free()

## Formatea segundos como "mm:ss" (p. ej. 75.0 -> "01:15").
func _format_time(secs: float) -> String:
	var total := int(secs)
	return "%02d:%02d" % [total / 60, total % 60]
func on_waves_completed():
	waves_done = true

func win_level():
	if finished:
		return
	# El jugador deja de disparar (el combate se detiene), pero sigue en pantalla.
	finish_level(true)

func on_player_died():
	if finished:
		return
	# El jugador muere: desaparece y los enemigos dejan de perseguirlo/atacar.
	finish_level(false)

## Termina la ronda sin congelar el motor: solo se desactiva todo lo que genera
## movimiento o ataques (spawns, disparos, summons y steering). Con su friction,
## los enemigos se detienen solos. Luego se muestran los stats con un tween.
func finish_level(player_won: bool):
	finished = true
	var surv_time := int(round(elapsed_time))
	# Capturamos el run_id antes de end_run (que lo limpia) para el cuestionario.
	var fb_run_id := Database.current_run_id

	# Avisa a la seleccion de mejoras para que no tape la pantalla de fin, y retira
	# los orbes de XP para que no se sigan recogiendo tras terminar.
	EventBus.game_finished.emit()
	_clear_xp_orbs()

	stop_combat()
	if not player_won:
		hide_player()

	id.text = "ID: " + Database.selected_id
	survival_time.text = "Survival Time: " + _format_time(elapsed_time)
	hits_taken.text = "Hits Taken: " + str(Database.hits_taken)
	enemies_defeated.text = "Enemies Defeated: " + str(GlobalData.enemies_dead)
	restart_button.visible = not GameTimer.timer_finished
	animate_stats()

	# Cierra la run: inserta Run y vacia los buffers (DamageTaken, UpgradeChoice),
	# RunBuild y UpgradeStats. final_round se topa al numero de oleadas.
	var final_round := mini(enemy_spawner.current_wave + 1, enemy_spawner.waves.size())
	Database.end_run(player_won, surv_time, final_round, ExperienceManager.current_level, \
		GlobalData.enemies_dead, int(ExperienceManager.total_xp_earned), \
		"win" if player_won else "death")

	# Boton para abrir el cuestionario post-run (menu temporal de pruebas).
	# Se apaga por completo con FeedbackForm.ENABLED.
	#_add_feedback_button(fb_run_id)

## Agrega un boton "Responder cuestionario" sobre el de reinicio. Solo si esta activado.
func _add_feedback_button(fb_run_id: String) -> void:
	if not FeedbackForm.ENABLED or fb_run_id == "":
		return
	var btn := Button.new()
	btn.text = "📝 Responder cuestionario"
	stats_box.add_child(btn)
	stats_box.move_child(btn, restart_button.get_index())  # justo encima de Reiniciar
	btn.pressed.connect(_open_feedback.bind(fb_run_id, btn))

## Abre el formulario; al cerrarse reactiva el boton por si quieren reabrirlo.
func _open_feedback(fb_run_id: String, btn: Button) -> void:
	var form := FeedbackForm.new()
	add_child(form)
	form.setup(fb_run_id)
	btn.disabled = true
	form.done.connect(func(): if is_instance_valid(btn): btn.disabled = false)

## Corta la ronda en seco: no se generan mas oleadas y se apagan disparos,
## summons y steering de todo lo que haya vivo en el nivel.
func stop_combat():
	enemy_spawner.stop()
	disable_combat_recursive(y_sort_entities)

func disable_combat_recursive(node: Node):
	for child in node.get_children():
		if child is ShootComponent:
			child.stop()
		elif child is SpawnerComponent:
			child.stop()
		elif child is ForceComponent:
			# Vacia los steering behaviors: sin fuerza, el friction los frena.
			child.rebuild_behaviors([])
		disable_combat_recursive(child)

## El jugador se desvanece y encoge, y deja de procesar (input, disparo, etc.).
func hide_player():
	player.set_process(false)
	player.set_physics_process(false)
	var tween := create_tween()
	tween.set_parallel(true)
	tween.tween_property(player, "scale", Vector2.ZERO, 0.3) \
		.set_trans(Tween.TRANS_BACK).set_ease(Tween.EASE_IN)
	tween.tween_property(player, "modulate:a", 0.0, 0.3)

## Aparece el panel de stats + boton con un fade y un pop, en vez de un corte seco.
func animate_stats():
	stats.visible = true
	stats.modulate.a = 0.0
	# Esperamos un frame para que el VBox tenga su tamano y el pivote quede centrado.
	await get_tree().process_frame
	stats_box.pivot_offset = stats_box.size / 2.0
	stats_box.scale = Vector2(0.8, 0.8)
	var tween := create_tween()
	tween.set_parallel(true)
	tween.tween_property(stats, "modulate:a", 1.0, 0.4) \
		.set_trans(Tween.TRANS_SINE).set_ease(Tween.EASE_OUT)
	tween.tween_property(stats_box, "scale", Vector2.ONE, 0.5) \
		.set_trans(Tween.TRANS_BACK).set_ease(Tween.EASE_OUT)

func on_restart_pressed():
	reset_run_state()
	get_tree().reload_current_scene()

## Reinicia el estado global persistente entre escenas para empezar de cero.
func reset_run_state():
	Engine.time_scale = 1.0
	get_tree().paused = false
	GlobalData.enemies_alive = 0
	GlobalData.enemies_dead = 0
	Database.enemies_alive = 0
	Database.hits_taken = 0
	Database.enemies_defeated = 0
	ExperienceManager.current_level = 1
	ExperienceManager.current_xp = 0.0
	ExperienceManager.total_xp_earned = 0.0
	ExperienceManager.xp_multiplier = 1.0
	ExperienceManager.wave_xp_multiplier = 1.0
	ExperienceManager.luck = ExperienceManager.base_luck
	GlobalData.wave_damage_multiplier = 1.0
	GlobalData.wave_health_multiplier = 1.0
	UpgradeManager.projectile_upgrades.clear()
	UpgradeManager.weapon_upgrades.clear()
	UpgradeManager.bonus_damage_mult = 1.0
	UpgradeManager.times_taken.clear()
	UpgradeManager.upgrade_weights.clear()
	UpgradeManager.rerolls_available = 0
	UpgradeManager.rerolls_per_level = 0
