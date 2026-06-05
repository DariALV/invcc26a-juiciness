class_name UpgradeSelection extends Control

@export var card_scene: PackedScene
@export var enter_duration: float = 0.45
@export var exit_duration: float = 0.3

## Gris (rerolls disponibles) y gris oscuro (sin rerolls) del boton de reroll.
const REROLL_AVAILABLE := Color(1, 1, 1, 1)
const REROLL_EMPTY := Color(0.5, 0.5, 0.52, 1)

@onready var dim: ColorRect = $Dim
@onready var card_holder: Control = $CardHolder
@onready var cards_box: HBoxContainer = $CardHolder/Center/Layout/Cards
@onready var reroll_button: Button = $CardHolder/Center/Layout/RerollButton

var pending: int = 0
var active: bool = false
var hidden_y: float = 0.0
## Ultimas 3 mejoras ofrecidas (tras rerolls), para registrar la eleccion.
var _last_choices: Array[Upgrade] = []
## Momento (ms) en que se mostro la seleccion actual, para medir el tiempo de decision.
var _choice_started_ms: int = 0
## Rerolls usados en la seleccion actual.
var _rerolls_this_choice: int = 0
## True cuando la partida termino (victoria/derrota): se ignoran subidas de nivel y
## la seleccion se cierra para no tapar la pantalla de fin.
var game_over: bool = false

func _ready():
	process_mode = Node.PROCESS_MODE_ALWAYS
	visible = false
	ExperienceManager.leveled_up.connect(on_leveled_up)
	EventBus.game_finished.connect(_on_game_finished)
	reroll_button.pressed.connect(_on_reroll_pressed)

func on_leveled_up(_level: int):
	if game_over:
		return
	AudioManager.play_level_up()
	pending += 1
	if not active:
		_show_next()

## Al terminar la partida: cierra cualquier seleccion activa y reanuda el arbol, para
## que la pantalla de fin (que anima con tweens) no quede bloqueada ni tapada.
func _on_game_finished():
	game_over = true
	pending = 0
	active = false
	visible = false
	get_tree().paused = false

func _show_next():
	active = true
	visible = true
	get_tree().paused = true
	# Cada subida de nivel renueva los rerolls a la cuota por nivel (mejoras de Reroll).
	UpgradeManager.replenish_rerolls()
	# Telemetria: arranca el cronometro de decision y reinicia el conteo de rerolls.
	_choice_started_ms = Time.get_ticks_msec()
	_rerolls_this_choice = 0
	_populate_cards()
	hidden_y = -get_viewport_rect().size.y
	card_holder.position.y = hidden_y
	dim.modulate.a = 0.0
	var t := create_tween()
	t.set_parallel(true)
	t.tween_property(card_holder, "position:y", 0.0, enter_duration) \
		.set_trans(Tween.TRANS_BACK) \
		.set_ease(Tween.EASE_OUT)
	t.tween_property(dim, "modulate:a", 1.0, enter_duration * 0.6)

func _populate_cards():
	for c in cards_box.get_children():
		c.queue_free()
	var choices := UpgradeManager.get_upgrade_choices(3)
	_last_choices = choices
	for u in choices:
		var card: UpgradeCard = card_scene.instantiate()
		cards_box.add_child(card)
		card.setup(u)
		card.selected.connect(_on_card_selected)
	_update_reroll_button()

## Vuelve a tirar las cartas si quedan rerolls disponibles.
func _on_reroll_pressed():
	if not active:
		return
	if UpgradeManager.consume_reroll():
		_rerolls_this_choice += 1
		AudioManager.play_upgrade_chosen()
		_populate_cards()

## Muestra el boton de reroll solo si quedan rerolls; si no, lo oculta.
func _update_reroll_button():
	var left: int = UpgradeManager.rerolls_available
	reroll_button.visible = left > 0
	reroll_button.disabled = left <= 0
	reroll_button.text = "Rerolls restantes: %d" % left
	reroll_button.modulate = REROLL_AVAILABLE

func _on_card_selected(upgrade: Upgrade):
	if not active:
		return
	active = false
	_set_buttons_disabled(true)
	AudioManager.play_upgrade_chosen()
	# Telemetria: registra las 3 opciones mostradas y la elegida.
	var opts := ["", "", ""]
	for i in mini(_last_choices.size(), 3):
		opts[i] = _last_choices[i].title
	var decision_ms := Time.get_ticks_msec() - _choice_started_ms
	Database.log_upgrade_choice(opts[0], opts[1], opts[2], upgrade.title, decision_ms, _rerolls_this_choice)
	UpgradeManager.take_upgrade(upgrade)
	var t := create_tween()
	t.set_parallel(true)
	t.tween_property(card_holder, "position:y", hidden_y, exit_duration) \
		.set_trans(Tween.TRANS_BACK) \
		.set_ease(Tween.EASE_IN)
	t.tween_property(dim, "modulate:a", 0.0, exit_duration)
	t.set_parallel(false)
	t.tween_callback(_on_hidden)

func _on_hidden():
	pending -= 1
	if pending > 0:
		_show_next()
	else:
		visible = false
		get_tree().paused = false

func _set_buttons_disabled(d: bool):
	for card in cards_box.get_children():
		if card is UpgradeCard:
			card.button.disabled = d
