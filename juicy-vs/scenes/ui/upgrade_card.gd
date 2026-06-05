class_name UpgradeCard extends Control

signal selected(upgrade: Upgrade)

@onready var rarity_bg: ColorRect = $RarityBG
@onready var background: NinePatchRect = $Background
@onready var title_label: Label = $Content/Title
@onready var description_label: RichTextLabel = $Content/Description
@onready var button: Button = $Button

const POSITIVE_COLOR := Color("74c47fff")
const NEGATIVE_COLOR := Color("f28d92ff")

var upgrade: Upgrade

func _ready():
	button.pressed.connect(func(): selected.emit(upgrade))

func setup(u: Upgrade) -> void:
	upgrade = u
	title_label.text = u.title
	var color := UpgradeManager.get_upgrade_color(u)
	if color:
		rarity_bg.color = color.text_color
		title_label.add_theme_color_override("font_color", color.outline_color)
	if color.texture:
		background.texture = color.texture
		background.visible = true
	else:
		background.visible = false
	var desc := _colorize(u.description)
	# Las mejoras con niveles muestran "Nivel N/M" (o "Nivel N" si son infinitas)
	# sobre la descripcion; las de un solo uso no muestran nada.
	var level_label := UpgradeManager.get_level_label(u)
	if level_label != "":
		var level_color := color.outline_color if color else Color.WHITE
		desc = "[color=#%s]%s[/color]\n%s" % [level_color.to_html(false), level_label, desc]
	description_label.text = desc

func _colorize(text: String) -> String:
	var re := RegEx.new()
	re.compile("[+\\-]\\d+(?:\\.\\d+)?%?")
	var result := ""
	var last := 0
	for m in re.search_all(text):
		result += text.substr(last, m.get_start() - last)
		var token := m.get_string()
		var col := POSITIVE_COLOR if token.begins_with("+") else NEGATIVE_COLOR
		result += "[color=#%s]%s[/color]" % [col.to_html(false), token]
		last = m.get_end()
	result += text.substr(last)
	return result
