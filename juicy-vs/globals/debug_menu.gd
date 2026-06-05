extends Node

## Menu de debug global para tweaking de balanceo.
##
## Se construye 100% por codigo (sin escenas) y se activa tecleando una palabra
## secreta en cualquier momento (sin nodos de input ni acciones del proyecto):
## un listener de teclas mantiene un buffer rodante y, al detectar la secuencia
## "debug", muestra/oculta el menu.
##
## El menu tiene varias pestanas que exponen TODA la config de GameConfig
## (player, enemigos, proyectiles, progresion, steering, oleadas) mas acciones
## de nivel (reiniciar, subir de nivel, saltar/reiniciar oleadas, invencibilidad,
## matar a todos, spawnear, escala de tiempo, pausa).

const SECRET := "debug"
const STEERABLE := ["enemy", "archer", "necromancer", "zombie"]

var _buffer := ""
var _open := false

var _canvas: CanvasLayer
var _root: Control
var _tabs: TabContainer
var _invincible := false

func _ready() -> void:
	process_mode = Node.PROCESS_MODE_ALWAYS
	_build_shell()

# --- Deteccion del codigo secreto -----------------------------------------

func _unhandled_key_input(event: InputEvent) -> void:
	if not (event is InputEventKey) or not event.pressed or event.echo:
		return
	if _open and event.keycode == KEY_ESCAPE:
		_set_open(false)
		return
	if event.unicode > 0:
		_buffer += char(event.unicode).to_lower()
		if _buffer.length() > 24:
			_buffer = _buffer.substr(_buffer.length() - 24)
		if _buffer.ends_with(SECRET):
			_buffer = ""
			_set_open(not _open)

# --- Estructura base del menu ---------------------------------------------

func _build_shell() -> void:
	_canvas = CanvasLayer.new()
	_canvas.layer = 128
	add_child(_canvas)

	_root = Control.new()
	_root.set_anchors_and_offsets_preset(Control.PRESET_FULL_RECT)
	_root.mouse_filter = Control.MOUSE_FILTER_STOP
	_canvas.add_child(_root)

	var dim := ColorRect.new()
	dim.color = Color(0, 0, 0, 0.55)
	dim.set_anchors_and_offsets_preset(Control.PRESET_FULL_RECT)
	dim.mouse_filter = Control.MOUSE_FILTER_IGNORE
	_root.add_child(dim)

	var margin := MarginContainer.new()
	margin.set_anchors_and_offsets_preset(Control.PRESET_FULL_RECT)
	for side in ["margin_left", "margin_right", "margin_top", "margin_bottom"]:
		margin.add_theme_constant_override(side, 12)
	_root.add_child(margin)

	var panel := PanelContainer.new()
	margin.add_child(panel)

	var vb := VBoxContainer.new()
	panel.add_child(vb)

	var header := Label.new()
	header.text = "DEBUG MENU"
	header.horizontal_alignment = HORIZONTAL_ALIGNMENT_CENTER
	vb.add_child(header)

	_tabs = TabContainer.new()
	_tabs.size_flags_vertical = Control.SIZE_EXPAND_FILL
	_tabs.size_flags_horizontal = Control.SIZE_EXPAND_FILL
	vb.add_child(_tabs)

	var footer := HBoxContainer.new()
	vb.add_child(footer)
	var hint := Label.new()
	hint.text = "Teclea 'debug' para abrir/cerrar  -  ESC para ocultar"
	hint.size_flags_horizontal = Control.SIZE_EXPAND_FILL
	footer.add_child(hint)
	var close_btn := Button.new()
	close_btn.text = "Cerrar"
	close_btn.pressed.connect(func(): _set_open(false))
	footer.add_child(close_btn)

	_root.visible = false

func _set_open(value: bool) -> void:
	_open = value
	if value:
		_populate()
	_root.visible = value
	# Pausa el juego mientras el menu esta abierto y lo reanuda al cerrarlo.
	# Los cambios de config se aplican igual (no dependen de _process), asi que
	# se ven reflejados al reanudar.
	get_tree().paused = value

# --- Construccion de pestanas ---------------------------------------------

func _populate() -> void:
	var previous := _tabs.current_tab
	for c in _tabs.get_children():
		_tabs.remove_child(c)
		c.queue_free()

	var player_tab := _make_tab("Player")
	_add_scalar_section(player_tab, "player")

	var enemies_tab := _make_tab("Enemies")
	for group in ["enemy", "archer", "necromancer", "zombie"]:
		_add_scalar_section(enemies_tab, group)

	var proj_tab := _make_tab("Projectiles")
	_add_scalar_section(proj_tab, "projectile")

	var prog_tab := _make_tab("Progression")
	_add_scalar_section(prog_tab, "experience")

	var steering_tab := _make_tab("Steering")
	_build_steering(steering_tab)

	var waves_tab := _make_tab("Waves")
	_build_waves(waves_tab)

	var actions_tab := _make_tab("Actions")
	_build_actions(actions_tab)

	if previous >= 0 and previous < _tabs.get_tab_count():
		_tabs.current_tab = previous

func _make_tab(title: String) -> VBoxContainer:
	var scroll := ScrollContainer.new()
	scroll.name = title
	scroll.horizontal_scroll_mode = ScrollContainer.SCROLL_MODE_DISABLED
	var vb := VBoxContainer.new()
	vb.size_flags_horizontal = Control.SIZE_EXPAND_FILL
	scroll.add_child(vb)
	_tabs.add_child(scroll)
	return vb

# --- Secciones escalares (data-driven desde GameConfig.GROUPS) -------------

func _add_scalar_section(parent: Control, group: String) -> void:
	var title := Label.new()
	title.text = "[ %s ]" % GameConfig.GROUPS[group].label
	parent.add_child(title)
	for field in GameConfig.GROUPS[group].fields:
		_add_scalar_field(parent, group, field)
	parent.add_child(HSeparator.new())

func _add_scalar_field(parent: Control, group: String, field: Dictionary) -> void:
	var row := HBoxContainer.new()
	var lbl := Label.new()
	lbl.text = field.label
	lbl.custom_minimum_size = Vector2(170, 0)
	row.add_child(lbl)
	var key: String = field.key
	if field.type == "bool":
		var cb := CheckBox.new()
		cb.button_pressed = bool(GameConfig.get_value(group, key))
		cb.toggled.connect(func(p): GameConfig.set_value(group, key, p))
		row.add_child(cb)
	else:
		var is_int: bool = field.type == "int"
		var sb := SpinBox.new()
		sb.min_value = field.min
		sb.max_value = field.max
		sb.step = field.step
		sb.rounded = is_int
		sb.custom_minimum_size = Vector2(120, 0)
		sb.value = float(GameConfig.get_value(group, key))
		sb.value_changed.connect(func(v): GameConfig.set_value(group, key, int(v) if is_int else v))
		row.add_child(sb)
	parent.add_child(row)

# --- Pestana de Steering ---------------------------------------------------

func _build_steering(parent: Control) -> void:
	var note := Label.new()
	note.text = "Agrega/edita/quita behaviors y targets. Los cambios afectan a los enemigos vivos y a los nuevos."
	note.autowrap_mode = TextServer.AUTOWRAP_WORD_SMART
	parent.add_child(note)
	for group in STEERABLE:
		_build_steering_group(parent, group)

func _build_steering_group(parent: Control, group: String) -> void:
	parent.add_child(HSeparator.new())
	var head := HBoxContainer.new()
	var title := Label.new()
	title.text = "[ %s ]" % group
	title.custom_minimum_size = Vector2(170, 0)
	head.add_child(title)
	var add_seek := Button.new()
	add_seek.text = "+ Seek"
	add_seek.pressed.connect(func(): _add_behavior(group, "seek"))
	head.add_child(add_seek)
	var add_flee := Button.new()
	add_flee.text = "+ Flee"
	add_flee.pressed.connect(func(): _add_behavior(group, "flee"))
	head.add_child(add_flee)
	parent.add_child(head)

	var defs: Array = GameConfig.steering.get(group, [])
	if defs.is_empty():
		var empty := Label.new()
		empty.text = "  (sin behaviors capturados todavia)"
		parent.add_child(empty)
	for i in defs.size():
		_build_behavior_row(parent, group, i, defs[i])

func _build_behavior_row(parent: Control, group: String, index: int, entry: Dictionary) -> void:
	var box := VBoxContainer.new()
	var top := HBoxContainer.new()
	var type_opt := OptionButton.new()
	type_opt.add_item("Seek")
	type_opt.add_item("Flee")
	type_opt.selected = 1 if entry.type == "flee" else 0
	type_opt.item_selected.connect(func(idx):
		entry.type = "flee" if idx == 1 else "seek"
		_apply_steering(group))
	top.add_child(type_opt)
	var add_t := Button.new()
	add_t.text = "+ Target"
	add_t.pressed.connect(func():
		entry.targets.append({"group": "player", "radius": 200.0, "force": 2.0})
		_apply_steering(group, true))
	top.add_child(add_t)
	var del_b := Button.new()
	del_b.text = "x Behavior"
	del_b.pressed.connect(func():
		GameConfig.steering[group].remove_at(index)
		_apply_steering(group, true))
	top.add_child(del_b)
	box.add_child(top)

	for j in entry.targets.size():
		_build_target_row(box, group, entry, j)
	parent.add_child(box)

func _build_target_row(parent: Control, group: String, entry: Dictionary, j: int) -> void:
	var target: Dictionary = entry.targets[j]
	var row := HBoxContainer.new()
	var gl := Label.new()
	gl.text = "  group"
	row.add_child(gl)
	var le := LineEdit.new()
	le.text = str(target.group)
	le.custom_minimum_size = Vector2(110, 0)
	le.text_changed.connect(func(t):
		target.group = t
		_apply_steering(group))
	row.add_child(le)
	var rl := Label.new()
	rl.text = "radius"
	row.add_child(rl)
	var rsb := SpinBox.new()
	rsb.min_value = 0.0
	rsb.max_value = 4000.0
	rsb.step = 1.0
	rsb.value = float(target.radius)
	rsb.custom_minimum_size = Vector2(100, 0)
	rsb.value_changed.connect(func(v):
		target.radius = v
		_apply_steering(group))
	row.add_child(rsb)
	var fl := Label.new()
	fl.text = "force"
	row.add_child(fl)
	var fsb := SpinBox.new()
	fsb.min_value = 0.0
	fsb.max_value = 100.0
	fsb.step = 0.1
	fsb.value = float(target.force)
	fsb.custom_minimum_size = Vector2(90, 0)
	fsb.value_changed.connect(func(v):
		target.force = v
		_apply_steering(group))
	row.add_child(fsb)
	var del_t := Button.new()
	del_t.text = "x"
	del_t.pressed.connect(func():
		entry.targets.remove_at(j)
		_apply_steering(group, true))
	row.add_child(del_t)
	parent.add_child(row)

func _add_behavior(group: String, type: String) -> void:
	GameConfig.ensure_steering(group)
	GameConfig.steering[group].append({
		"type": type,
		"targets": [{"group": "player", "radius": 200.0, "force": 2.0}],
	})
	_apply_steering(group, true)

## Propaga la config de steering del grupo y, si hubo cambio estructural,
## reconstruye la pestana para reflejar filas agregadas/quitadas.
func _apply_steering(group: String, rebuild: bool = false) -> void:
	GameConfig.set_steering(group, GameConfig.steering.get(group, []))
	if rebuild:
		_populate()

# --- Pestana de Oleadas ----------------------------------------------------

func _build_waves(parent: Control) -> void:
	var bar := HBoxContainer.new()
	var add_w := Button.new()
	add_w.text = "+ Wave"
	add_w.pressed.connect(func():
		GameConfig.add_wave()
		_populate())
	bar.add_child(add_w)
	var apply := Button.new()
	apply.text = "Aplicar y reiniciar oleadas"
	apply.pressed.connect(_apply_waves)
	bar.add_child(apply)
	parent.add_child(bar)

	for i in GameConfig.waves.size():
		_build_wave_block(parent, i, GameConfig.waves[i])

func _build_wave_block(parent: Control, index: int, wave: Dictionary) -> void:
	parent.add_child(HSeparator.new())
	var head := HBoxContainer.new()
	var title := Label.new()
	title.text = "Wave %d" % (index + 1)
	title.custom_minimum_size = Vector2(170, 0)
	head.add_child(title)
	var del := Button.new()
	del.text = "x Wave"
	del.pressed.connect(func():
		GameConfig.remove_wave(index)
		_populate())
	head.add_child(del)
	parent.add_child(head)

	_add_wave_num(parent, wave, "duration", "Duration (s)", 1.0, 600.0, 1.0)
	_add_wave_num(parent, wave, "base_spawn_rate", "Base Spawn Rate", 0.0, 100.0, 0.1)
	_add_wave_num(parent, wave, "max_alive", "Max Alive (0=inf)", 0.0, 999.0, 1.0)
	_add_wave_num(parent, wave, "min_alive", "Min Alive", 0.0, 999.0, 1.0)
	_add_wave_num(parent, wave, "interval_jitter", "Interval Jitter", 0.0, 1.0, 0.05)

	var req_row := HBoxContainer.new()
	var req_lbl := Label.new()
	req_lbl.text = "  Require All Spawned"
	req_lbl.custom_minimum_size = Vector2(170, 0)
	req_row.add_child(req_lbl)
	var req_cb := CheckBox.new()
	req_cb.button_pressed = bool(wave.get("require_all_spawned", false))
	req_cb.toggled.connect(func(p): wave["require_all_spawned"] = p)
	req_row.add_child(req_cb)
	parent.add_child(req_row)

	var en_head := HBoxContainer.new()
	var en_lbl := Label.new()
	en_lbl.text = "  Enemies:"
	en_lbl.size_flags_horizontal = Control.SIZE_EXPAND_FILL
	en_head.add_child(en_lbl)
	var add_e := Button.new()
	add_e.text = "+ Enemy"
	add_e.pressed.connect(func():
		GameConfig.add_wave_enemy(index)
		_populate())
	en_head.add_child(add_e)
	parent.add_child(en_head)

	for j in wave.enemies.size():
		_build_wave_enemy_row(parent, index, wave.enemies[j], j)

func _add_wave_num(parent: Control, wave: Dictionary, key: String, label: String, mn: float, mx: float, step: float) -> void:
	var row := HBoxContainer.new()
	var lbl := Label.new()
	lbl.text = "  " + label
	lbl.custom_minimum_size = Vector2(170, 0)
	row.add_child(lbl)
	var sb := SpinBox.new()
	sb.min_value = mn
	sb.max_value = mx
	sb.step = step
	sb.rounded = step >= 1.0
	sb.value = float(wave[key])
	sb.custom_minimum_size = Vector2(120, 0)
	sb.value_changed.connect(func(v):
		wave[key] = int(v) if step >= 1.0 else v)
	row.add_child(sb)
	parent.add_child(row)

func _build_wave_enemy_row(parent: Control, wave_index: int, enemy: Dictionary, j: int) -> void:
	var row := HBoxContainer.new()
	var opt := OptionButton.new()
	var paths: Array = GameConfig.ENEMY_CATALOG.values()
	var names: Array = GameConfig.ENEMY_CATALOG.keys()
	for n in names:
		opt.add_item(n)
	var current_idx: int = paths.find(enemy.scene_path)
	if current_idx >= 0:
		opt.selected = current_idx
	opt.item_selected.connect(func(idx):
		enemy.scene_path = paths[idx])
	row.add_child(opt)

	row.add_child(_mini_label("amt"))
	var amt := SpinBox.new()
	amt.min_value = 0; amt.max_value = 9999; amt.step = 1; amt.rounded = true
	amt.value = float(enemy.amount); amt.custom_minimum_size = Vector2(90, 0)
	amt.value_changed.connect(func(v): enemy.amount = int(v))
	row.add_child(amt)

	row.add_child(_mini_label("hp x"))
	var hp := SpinBox.new()
	hp.min_value = 0.1; hp.max_value = 100.0; hp.step = 0.1
	hp.value = float(enemy.health_multiplier); hp.custom_minimum_size = Vector2(80, 0)
	hp.value_changed.connect(func(v): enemy.health_multiplier = v)
	row.add_child(hp)

	row.add_child(_mini_label("wgt"))
	var wgt := SpinBox.new()
	wgt.min_value = 0.0; wgt.max_value = 100.0; wgt.step = 0.5
	wgt.value = float(enemy.weight); wgt.custom_minimum_size = Vector2(80, 0)
	wgt.value_changed.connect(func(v): enemy.weight = v)
	row.add_child(wgt)

	row.add_child(_mini_label("win"))
	var ws := SpinBox.new()
	ws.min_value = 0.0; ws.max_value = 1.0; ws.step = 0.05
	ws.value = float(enemy.get("spawn_window_start", 0.0)); ws.custom_minimum_size = Vector2(70, 0)
	ws.value_changed.connect(func(v): enemy["spawn_window_start"] = v)
	row.add_child(ws)
	var we_end := SpinBox.new()
	we_end.min_value = 0.0; we_end.max_value = 1.0; we_end.step = 0.05
	we_end.value = float(enemy.get("spawn_window_end", 1.0)); we_end.custom_minimum_size = Vector2(70, 0)
	we_end.value_changed.connect(func(v): enemy["spawn_window_end"] = v)
	row.add_child(we_end)

	var del := Button.new()
	del.text = "x"
	del.pressed.connect(func():
		GameConfig.remove_wave_enemy(wave_index, j)
		_populate())
	row.add_child(del)
	parent.add_child(row)

func _mini_label(text: String) -> Label:
	var l := Label.new()
	l.text = " " + text
	return l

func _apply_waves() -> void:
	var spawner = GameConfig.get_spawner()
	if spawner:
		spawner.rebuild_and_restart()

# --- Pestana de Acciones ---------------------------------------------------

func _build_actions(parent: Control) -> void:
	# Stats de spawner/nivel (escalares, se aplican en vivo al EnemySpawner).
	_add_scalar_section(parent, "spawner")

	var grid := VBoxContainer.new()
	parent.add_child(grid)

	_action_button(grid, "Reiniciar nivel", func():
		Engine.time_scale = 1.0
		get_tree().paused = false
		_set_open(false)
		get_tree().reload_current_scene())

	_action_button(grid, "Subir de nivel (+1)", func():
		ExperienceManager.force_level_up())

	_action_button(grid, "Saltar a siguiente oleada", func():
		var s = GameConfig.get_spawner()
		if s: s.debug_skip_wave())

	_action_button(grid, "Reiniciar oleadas", func():
		var s = GameConfig.get_spawner()
		if s: s.rebuild_and_restart())

	_action_button(grid, "Matar a todos los enemigos", _kill_all_enemies)

	# Invencibilidad del jugador.
	var inv_row := HBoxContainer.new()
	var inv_lbl := Label.new()
	inv_lbl.text = "Jugador invencible"
	inv_lbl.custom_minimum_size = Vector2(170, 0)
	inv_row.add_child(inv_lbl)
	var inv_cb := CheckBox.new()
	inv_cb.button_pressed = _invincible
	inv_cb.toggled.connect(_set_invincible)
	inv_row.add_child(inv_cb)
	grid.add_child(inv_row)

	# Escala de tiempo (se aplica al reanudar tras cerrar el menu; util para slow-mo).
	var ts_row := HBoxContainer.new()
	var ts_lbl := Label.new()
	ts_lbl.text = "Escala de tiempo"
	ts_lbl.custom_minimum_size = Vector2(170, 0)
	ts_row.add_child(ts_lbl)
	var ts_val := Label.new()
	ts_val.text = "%.2f" % Engine.time_scale
	ts_val.custom_minimum_size = Vector2(50, 0)
	var ts_slider := HSlider.new()
	ts_slider.min_value = 0.0
	ts_slider.max_value = 3.0
	ts_slider.step = 0.05
	ts_slider.value = Engine.time_scale
	ts_slider.custom_minimum_size = Vector2(180, 0)
	ts_slider.value_changed.connect(func(v):
		Engine.time_scale = v
		ts_val.text = "%.2f" % v)
	ts_row.add_child(ts_slider)
	ts_row.add_child(ts_val)
	grid.add_child(ts_row)

	# Spawnear enemigo manualmente.
	parent.add_child(HSeparator.new())
	var spawn_row := HBoxContainer.new()
	var spawn_opt := OptionButton.new()
	var names: Array = GameConfig.ENEMY_CATALOG.keys()
	var paths: Array = GameConfig.ENEMY_CATALOG.values()
	for n in names:
		spawn_opt.add_item(n)
	spawn_row.add_child(spawn_opt)
	var spawn_btn := Button.new()
	spawn_btn.text = "Spawnear enemigo"
	spawn_btn.pressed.connect(func(): _spawn_enemy(paths[spawn_opt.selected]))
	spawn_row.add_child(spawn_btn)
	parent.add_child(spawn_row)

func _action_button(parent: Control, text: String, callback: Callable) -> void:
	var b := Button.new()
	b.text = text
	b.pressed.connect(callback)
	parent.add_child(b)

func _kill_all_enemies() -> void:
	for e in get_tree().get_nodes_in_group("enemy"):
		var h = e.get_node_or_null("HealthComponent")
		if h:
			h.apply_damage(1e9)

func _set_invincible(on: bool) -> void:
	_invincible = on
	GameConfig.player_invincible = on
	var p = LevelManager.player
	if p and is_instance_valid(p) and p.health:
		p.health.invincible = on

func _spawn_enemy(path: String) -> void:
	if not ResourceLoader.exists(path):
		return
	var player = LevelManager.player
	if player == null or not is_instance_valid(player):
		return
	var container = LevelManager.y_sort_entities
	if container == null or not is_instance_valid(container):
		container = get_tree().current_scene
	var scene = load(path)
	var e = scene.instantiate()
	container.add_child(e)
	var ang := randf() * TAU
	e.global_position = player.global_position + Vector2(cos(ang), sin(ang)) * 90.0
	GlobalData.enemies_alive += 1
	var h = e.get_node_or_null("HealthComponent")
	if h:
		h.died.connect(_on_manual_enemy_died)

func _on_manual_enemy_died() -> void:
	GlobalData.enemies_alive -= 1
	GlobalData.enemies_dead += 1
