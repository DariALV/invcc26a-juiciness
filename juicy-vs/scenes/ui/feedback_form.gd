extends CanvasLayer
class_name FeedbackForm

## Cuestionario post-run de auto-evaluacion (menu TEMPORAL de pruebas con amigos).
## Se abre desde el boton "Responder cuestionario" de la pantalla de fin y vincula
## las respuestas a la run.
##
## INTERRUPTOR UNICO: pon ENABLED en false para que ni el boton ni el cuestionario
## aparezcan cuando el juego vaya a los sujetos de prueba reales.
const ENABLED := true

## Se emite cuando el formulario se cierra (enviar o cancelar).
signal done

## Preguntas Likert 1-7: [campo_en_db, texto corto].
const QUESTIONS := [
	["difficulty", "Dificultad  (1 fácil · 7 dificilísimo)"],
	["fun", "Diversión  (1 nada · 7 muchísimo)"],
	["chaos", "Caos  (1 nada · 7 mucho)"],
	["monotony", "Monotonía / repetitivo  (1 nada · 7 mucho)"],
	["boredom", "Aburrimiento  (1 nada · 7 mucho)"],
	["stress", "Estrés  (1 nada · 7 mucho)"],
	["style_liking", "Estilo visual / juicy  (1 nada · 7 me encantó)"],
]

# --- Paleta pastel ---
const C_BG_DIM := Color(0.10, 0.09, 0.15, 0.55)
const C_PANEL := Color("#FBFAFD")
const C_BORDER := Color("#A8C7E7")
const C_TITLE := Color("#5E7BA6")
const C_TEXT := Color("#3A3A4A")
const C_SUBMIT := Color("#B5E0C6")   # verde menta
const C_CANCEL := Color("#F7B7C2")   # rosa
const C_TRACK := Color("#E7E1F0")
const C_GRAB := Color("#A8C7E7")

var run_id: String = ""
var _sliders: Dictionary = {}
var _unnecessary: CheckBox
var _comments: TextEdit
var _submit_btn: Button
var _cancel_btn: Button
var _thanks: Label

## Lo llama el nivel justo despues de instanciar, con el run_id de la partida que termino.
func setup(p_run_id: String) -> void:
	run_id = p_run_id

func _ready() -> void:
	layer = 100
	process_mode = Node.PROCESS_MODE_ALWAYS
	_build_ui()

func _build_ui() -> void:
	# Fondo oscuro semitransparente que bloquea clics a lo de atras.
	var bg := ColorRect.new()
	bg.color = C_BG_DIM
	bg.set_anchors_preset(Control.PRESET_FULL_RECT)
	bg.mouse_filter = Control.MOUSE_FILTER_STOP
	add_child(bg)

	# Panel anclado a pantalla completa con margenes proporcionales: asi se adapta a
	# cualquier resolucion (incluido el viewport pequeno del juego) y los botones de
	# abajo SIEMPRE quedan visibles.
	var vp := get_viewport().get_visible_rect().size
	var mx: float = maxf(14.0, vp.x * 0.12)
	var my: float = maxf(14.0, vp.y * 0.07)

	var panel := PanelContainer.new()
	panel.set_anchors_preset(Control.PRESET_FULL_RECT)
	panel.offset_left = mx
	panel.offset_top = my
	panel.offset_right = -mx
	panel.offset_bottom = -my
	panel.add_theme_stylebox_override("panel", _panel_style())
	add_child(panel)

	var margin := MarginContainer.new()
	for side in ["left", "right", "top", "bottom"]:
		margin.add_theme_constant_override("margin_" + side, 16)
	panel.add_child(margin)

	var vbox := VBoxContainer.new()
	vbox.add_theme_constant_override("separation", 8)
	margin.add_child(vbox)

	var title := Label.new()
	title.text = "🧃 ¿Cómo te fue?"
	title.add_theme_font_size_override("font_size", 22)
	title.add_theme_color_override("font_color", C_TITLE)
	title.horizontal_alignment = HORIZONTAL_ALIGNMENT_CENTER
	vbox.add_child(title)

	var note := Label.new()
	note.text = "Arrastra cada barra del 1 al 7  ·  opcional"
	note.add_theme_font_size_override("font_size", 11)
	note.add_theme_color_override("font_color", C_TEXT)
	note.modulate = Color(1, 1, 1, 0.65)
	note.horizontal_alignment = HORIZONTAL_ALIGNMENT_CENTER
	vbox.add_child(note)

	# Las preguntas viven en un scroll que se ESTIRA para ocupar el espacio sobrante,
	# dejando los botones fijos abajo.
	var scroll := ScrollContainer.new()
	scroll.horizontal_scroll_mode = ScrollContainer.SCROLL_MODE_DISABLED
	scroll.size_flags_vertical = Control.SIZE_EXPAND_FILL
	vbox.add_child(scroll)

	var qbox := VBoxContainer.new()
	qbox.add_theme_constant_override("separation", 8)
	qbox.size_flags_horizontal = Control.SIZE_EXPAND_FILL
	scroll.add_child(qbox)

	for q in QUESTIONS:
		_add_likert(qbox, q[0], q[1])

	_unnecessary = CheckBox.new()
	_unnecessary.text = "Vi mejoras innecesarias o inútiles"
	_unnecessary.add_theme_color_override("font_color", C_TEXT)
	qbox.add_child(_unnecessary)

	var clabel := Label.new()
	clabel.text = "Comentarios / ideas:"
	clabel.add_theme_font_size_override("font_size", 12)
	clabel.add_theme_color_override("font_color", C_TEXT)
	qbox.add_child(clabel)

	_comments = TextEdit.new()
	_comments.placeholder_text = "Opcional…"
	_comments.custom_minimum_size = Vector2(0, 56)
	_comments.size_flags_horizontal = Control.SIZE_EXPAND_FILL
	qbox.add_child(_comments)

	# "Gracias" oculto, se muestra al enviar.
	_thanks = Label.new()
	_thanks.text = "¡Gracias por tu feedback! 💖"
	_thanks.add_theme_color_override("font_color", C_TITLE)
	_thanks.horizontal_alignment = HORIZONTAL_ALIGNMENT_CENTER
	_thanks.visible = false
	vbox.add_child(_thanks)

	# Botones (fijos abajo).
	var buttons := HBoxContainer.new()
	buttons.alignment = BoxContainer.ALIGNMENT_CENTER
	buttons.add_theme_constant_override("separation", 14)
	vbox.add_child(buttons)

	_submit_btn = _make_button("Enviar", C_SUBMIT)
	_submit_btn.pressed.connect(_on_submit)
	buttons.add_child(_submit_btn)

	_cancel_btn = _make_button("Cerrar", C_CANCEL)
	_cancel_btn.pressed.connect(_finish)
	buttons.add_child(_cancel_btn)

## Crea una fila: etiqueta + slider pastel 1-7 + valor actual.
func _add_likert(parent: VBoxContainer, field: String, text: String) -> void:
	var row := HBoxContainer.new()
	row.add_theme_constant_override("separation", 10)
	row.size_flags_horizontal = Control.SIZE_EXPAND_FILL

	var lbl := Label.new()
	lbl.text = text
	lbl.add_theme_font_size_override("font_size", 13)
	lbl.add_theme_color_override("font_color", C_TEXT)
	lbl.autowrap_mode = TextServer.AUTOWRAP_WORD_SMART
	lbl.custom_minimum_size = Vector2(180, 0)
	lbl.size_flags_horizontal = Control.SIZE_EXPAND_FILL
	row.add_child(lbl)

	var slider := HSlider.new()
	slider.min_value = 1
	slider.max_value = 7
	slider.step = 1
	slider.value = 4
	slider.custom_minimum_size = Vector2(140, 0)
	slider.size_flags_vertical = Control.SIZE_SHRINK_CENTER
	_style_slider(slider)
	row.add_child(slider)

	var val := Label.new()
	val.text = "4"
	val.add_theme_color_override("font_color", C_TITLE)
	val.custom_minimum_size = Vector2(16, 0)
	val.horizontal_alignment = HORIZONTAL_ALIGNMENT_CENTER
	row.add_child(val)

	slider.value_changed.connect(func(v): val.text = str(int(v)))
	_sliders[field] = slider
	parent.add_child(row)

func _on_submit() -> void:
	var data := {}
	for field in _sliders:
		data[field] = int((_sliders[field] as HSlider).value)
	data["unnecessary_upgrades"] = _unnecessary.button_pressed
	data["comments"] = _comments.text.strip_edges()
	if run_id != "":
		Database.submit_feedback(run_id, data)
	# Feedback visual y cierre breve.
	_thanks.visible = true
	_submit_btn.disabled = true
	_cancel_btn.disabled = true
	await get_tree().create_timer(0.9).timeout
	_finish()

func _finish() -> void:
	done.emit()
	queue_free()

# --- Estilos pastel --------------------------------------------------------

func _panel_style() -> StyleBoxFlat:
	var sb := StyleBoxFlat.new()
	sb.bg_color = C_PANEL
	sb.set_border_width_all(2)
	sb.border_color = C_BORDER
	sb.set_corner_radius_all(18)
	sb.shadow_color = Color(0, 0, 0, 0.25)
	sb.shadow_size = 8
	return sb

func _make_button(text: String, color: Color) -> Button:
	var b := Button.new()
	b.text = text
	b.custom_minimum_size = Vector2(120, 36)
	b.add_theme_color_override("font_color", C_TEXT)
	b.add_theme_color_override("font_hover_color", C_TEXT)
	b.add_theme_color_override("font_pressed_color", C_TEXT)
	b.add_theme_stylebox_override("normal", _btn_style(color))
	b.add_theme_stylebox_override("hover", _btn_style(color.lightened(0.10)))
	b.add_theme_stylebox_override("pressed", _btn_style(color.darkened(0.10)))
	b.add_theme_stylebox_override("disabled", _btn_style(color.lightened(0.25)))
	return b

func _btn_style(color: Color) -> StyleBoxFlat:
	var sb := StyleBoxFlat.new()
	sb.bg_color = color
	sb.set_corner_radius_all(12)
	sb.set_content_margin_all(8)
	return sb

func _style_slider(slider: HSlider) -> void:
	var track := StyleBoxFlat.new()
	track.bg_color = C_TRACK
	track.set_corner_radius_all(4)
	track.content_margin_top = 4
	track.content_margin_bottom = 4
	slider.add_theme_stylebox_override("slider", track)

	var area := StyleBoxFlat.new()
	area.bg_color = C_GRAB
	area.set_corner_radius_all(4)
	area.content_margin_top = 4
	area.content_margin_bottom = 4
	slider.add_theme_stylebox_override("grabber_area", area)
	slider.add_theme_stylebox_override("grabber_area_highlight", area)
