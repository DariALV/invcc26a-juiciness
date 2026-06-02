class_name UpgradeSelection extends Control

@export var card_scene: PackedScene
@export var enter_duration: float = 0.45
@export var exit_duration: float = 0.3

@onready var dim: ColorRect = $Dim
@onready var card_holder: Control = $CardHolder
@onready var cards_box: HBoxContainer = $CardHolder/Center/Cards

var pending: int = 0
var active: bool = false
var hidden_y: float = 0.0

func _ready():
	process_mode = Node.PROCESS_MODE_ALWAYS
	visible = false
	ExperienceManager.leveled_up.connect(on_leveled_up)

func on_leveled_up(_level: int):
	pending += 1
	if not active:
		_show_next()

func _show_next():
	active = true
	visible = true
	get_tree().paused = true
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
	for u in choices:
		var card: UpgradeCard = card_scene.instantiate()
		cards_box.add_child(card)
		card.setup(u)
		card.selected.connect(_on_card_selected)

func _on_card_selected(upgrade: Upgrade):
	if not active:
		return
	active = false
	_set_buttons_disabled(true)
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
