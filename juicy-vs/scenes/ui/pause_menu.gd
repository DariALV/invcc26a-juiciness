class_name PauseMenu extends Control

## Menu de pausa del nivel.
##
## - Pausa/reanuda el juego (get_tree().paused) con la accion "pause" (Esc / P).
## - A la derecha: sliders de volumen para los buses Master, Musica y Efectos (SFX),
##   que delegan en AudioManager.
## - A la izquierda: una lista de estadisticas afectadas por las mejoras (Daño,
##   Penetracion, Vida Maxima, Flechas, Suerte, ...). Si el valor actual supera al
##   base (mejoria), el numero se pinta de verde.
##
## Procesa siempre (PROCESS_MODE_ALWAYS) para poder abrirse/cerrarse con el arbol
## pausado. No se abre si otra cosa ya pauso el juego (p. ej. la seleccion de mejoras).

## Verde de "mejora positiva" (igual que UpgradeCard.POSITIVE_COLOR).
const STAT_IMPROVED := Color("74c47f")
const STAT_NORMAL := Color("e0e0e0")

@onready var overlay: Control = $Overlay
@onready var pause_button: Button = %PauseButton
@onready var stats_list: VBoxContainer = %StatsList
@onready var master_slider: HSlider = %MasterSlider
@onready var music_slider: HSlider = %MusicSlider
@onready var sfx_slider: HSlider = %SfxSlider
@onready var resume_button: Button = %ResumeButton

var is_open := false
## True tras victoria/derrota: bloquea la pausa para no tapar la pantalla de fin ni
## robarle la tecla "P" al campo de texto de la encuesta.
var game_over := false

func _ready() -> void:
	process_mode = Node.PROCESS_MODE_ALWAYS
	# La raiz permanece visible (alberga el boton de pausa); solo el overlay se oculta.
	overlay.visible = false
	pause_button.pressed.connect(toggle)
	resume_button.pressed.connect(close)
	EventBus.game_finished.connect(_on_game_finished)
	# Inicializa los sliders desde el volumen actual de cada bus y conecta sus cambios.
	# (Debe ir aqui, no en _on_game_finished, para que funcionen durante la partida.)
	master_slider.value = AudioManager.get_bus_volume(AudioManager.MASTER_BUS)
	music_slider.value = AudioManager.get_bus_volume(AudioManager.MUSIC_BUS)
	sfx_slider.value = AudioManager.get_bus_volume(AudioManager.SFX_BUS)
	master_slider.value_changed.connect(_on_master_changed)
	music_slider.value_changed.connect(_on_music_changed)
	sfx_slider.value_changed.connect(_on_sfx_changed)

## Al terminar la partida: desactiva la pausa y cierra el menu si estaba abierto.
func _on_game_finished() -> void:
	game_over = true
	if is_open:
		close()

func _on_master_changed(v: float) -> void:
	AudioManager.set_bus_volume(AudioManager.MASTER_BUS, v)

func _on_music_changed(v: float) -> void:
	AudioManager.set_bus_volume(AudioManager.MUSIC_BUS, v)

func _on_sfx_changed(v: float) -> void:
	AudioManager.set_bus_volume(AudioManager.SFX_BUS, v)

func _input(event: InputEvent) -> void:
	if event.is_action_pressed("pause"):
		# Tras el fin de partida no se pausa: deja pasar la tecla (p. ej. escribir "p"
		# en la encuesta) sin consumir el evento.
		if game_over:
			return
		toggle()
		get_viewport().set_input_as_handled()

func toggle() -> void:
	if is_open:
		close()
	elif not get_tree().paused:
		open()

func open() -> void:
	is_open = true
	_refresh_stats()
	overlay.visible = true
	get_tree().paused = true

func close() -> void:
	is_open = false
	overlay.visible = false
	get_tree().paused = false

# --- Estadisticas ----------------------------------------------------------

func _refresh_stats() -> void:
	for c in stats_list.get_children():
		stats_list.remove_child(c)
		c.queue_free()
	for row in _collect_stats():
		var rt := RichTextLabel.new()
		rt.bbcode_enabled = true
		rt.fit_content = true
		rt.scroll_active = false
		rt.autowrap_mode = TextServer.AUTOWRAP_OFF
		rt.custom_minimum_size = Vector2(190, 0)
		var hex: String = (STAT_IMPROVED if row.improved else STAT_NORMAL).to_html(false)
		rt.text = "%s: [color=#%s]%s[/color]" % [row.name, hex, row.text]
		stats_list.add_child(rt)

## Reune las estadisticas vigentes vs su valor base. Los stats de proyectil (daño,
## penetracion) se calculan partiendo de la config base y acumulando las mejoras de
## proyectil tomadas; el resto se leen de las instancias vivas (jugador, arco,
## ExperienceManager).
func _collect_stats() -> Array:
	var rows: Array = []

	# Daño: base de GameConfig + mejoras de proyectil acumuladas. (La perforacion ya no es
	# una stat: ahora la flecha atraviesa segun su dano, no hay numero de penetracion.)
	var dmg: float = float(GameConfig.get_value("projectile", "damage"))
	var dmg_base: float = dmg
	for u in UpgradeManager.projectile_upgrades:
		if u is DamageProjectileUpgrade:
			dmg = dmg * u.multiplier + u.flat_bonus
	rows.append(_stat_row("Daño", dmg, dmg_base, "num"))

	var player := LevelManager.player
	if is_instance_valid(player):
		rows.append(_stat_row("Vida Máxima", player.health.max_health, \
			GameConfig.get_value("player", "max_health"), "int"))
		rows.append(_stat_row("Regeneración", player.health.regen_per_second, 0.0, "num"))
		rows.append(_stat_row("Velocidad", player.movement.max_speed, \
			GameConfig.get_value("player", "max_speed"), "int"))
		rows.append(_stat_row("Flechas", player.bow.arrow_count, \
			GameConfig.get_value("player", "bow_arrow_count"), "int"))
		rows.append(_stat_row("Cadencia", player.bow.shoot_speed, \
			GameConfig.get_value("player", "bow_shoot_speed"), "num"))
		rows.append(_stat_row("Vel. Flecha", player.bow.arrow_speed, \
			GameConfig.get_value("player", "bow_arrow_speed"), "int"))
		rows.append(_stat_row("Vampirismo", player.heal_on_kill, 0.0, "num"))
		rows.append(_stat_row("Esquiva", player.dodge_chance, 0.0, "percent"))
		rows.append(_stat_row("Reflexión", player.reflect_chance, 0.0, "percent"))
		rows.append(_stat_row("Venganza", player.death_arrow_chance, 0.0, "percent"))
		rows.append(_stat_row("Daño de área", player.area_damage, 0.0, "num"))
		rows.append(_stat_row("Frec. área", player.area_attack_rate, 1.0, "num"))
		rows.append(_stat_row("Objetivos área", player.area_max_targets, 5, "int"))

	rows.append(_stat_row("Suerte", ExperienceManager.luck, ExperienceManager.base_luck, "num"))
	rows.append(_stat_row("Mult. XP", ExperienceManager.xp_multiplier, 1.0, "num"))

	return rows

func _stat_row(stat_name: String, value, base_value, fmt: String) -> Dictionary:
	return {
		"name": stat_name,
		"text": _fmt(value, fmt),
		# Verde solo cuando el valor actual supera al base (mejoria).
		"improved": float(value) > float(base_value),
	}

func _fmt(v, fmt: String) -> String:
	var f := float(v)
	match fmt:
		"percent":
			return "%d%%" % int(round(f * 100.0))
		"int":
			return str(int(round(f)))
		_:
			if is_equal_approx(f, round(f)):
				return str(int(round(f)))
			return "%.2f" % f
