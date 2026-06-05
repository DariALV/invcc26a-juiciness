class_name StudentIDMenu extends Control

## Menu inicial: el participante escribe su carne y, al buscar, se consulta en Supabase
## (tabla PlayerIDConfig). Si existe, un popup pide confirmacion; al aceptar se carga su
## config de camara y se entra al nivel. Si no existe, se avisa y no pasa nada.

const LEVEL_SCENE := "res://scenes/entities/levels/level.tscn"

@onready var search_bar: LineEdit = %LineEdit
@onready var status_label: Label = %Status
@onready var search_button: Button = %SearchButton
@onready var popup: Control = %Popup
@onready var popup_message: Label = %PopupMessage
@onready var yes_button: Button = %YesButton
@onready var no_button: Button = %NoButton

## Carne confirmado en el popup (el que entrara al nivel si se acepta).
var _pending_id: String = ""
## Evita lanzar dos busquedas simultaneas.
var _searching: bool = false

func _ready() -> void:
	popup.visible = false
	status_label.text = ""
	search_button.pressed.connect(_on_search_pressed)
	# Enter en el campo tambien dispara la busqueda.
	search_bar.text_submitted.connect(func(_t: String): _on_search_pressed())
	yes_button.pressed.connect(_on_yes_pressed)
	no_button.pressed.connect(_on_no_pressed)

func _on_search_pressed() -> void:
	if _searching:
		return
	var id := search_bar.text.strip_edges()
	if id == "":
		status_label.text = "Ingrese un carné."
		return
	_searching = true
	search_button.disabled = true
	status_label.text = "Buscando..."
	# La config se devuelve por callback; le adjuntamos el id buscado.
	Database.get_player_config(id, _on_config_received.bind(id))

## Callback de la consulta: 'config' vacio significa que el carne no existe.
func _on_config_received(config: Dictionary, id: String) -> void:
	_searching = false
	search_button.disabled = false
	if config.is_empty():
		status_label.text = "Carné no encontrado."
		return
	status_label.text = ""
	# Usa el id canonico de Supabase (no el texto tecleado): asi, aunque el match sea
	# case-insensitive, la run se guarda con la forma oficial (p. ej. "C20413").
	_pending_id = str(config.get("id", id))
	popup_message.text = "Carné encontrado: %s.\n¿Desea continuar?" % _pending_id
	_show_popup()

func _on_yes_pressed() -> void:
	if _pending_id == "":
		return
	Database.selected_id = _pending_id
	# Carga los flags de efectos de camara del participante (persisten al cambiar de
	# escena). Luego entra al nivel.
	CameraJuice.load_config(_pending_id)
	get_tree().call_deferred("change_scene_to_file", LEVEL_SCENE)

func _on_no_pressed() -> void:
	_pending_id = ""
	_hide_popup()

## Muestra el popup con un pequeno pop de escala (juice).
func _show_popup() -> void:
	popup.visible = true
	popup.modulate.a = 0.0
	var panel := popup.get_node("PopupPanel") as Control
	panel.pivot_offset = panel.size / 2.0
	panel.scale = Vector2(0.85, 0.85)
	var tween := create_tween()
	tween.set_parallel(true)
	tween.tween_property(popup, "modulate:a", 1.0, 0.15)
	tween.tween_property(panel, "scale", Vector2.ONE, 0.25) \
		.set_trans(Tween.TRANS_BACK).set_ease(Tween.EASE_OUT)

func _hide_popup() -> void:
	popup.visible = false
