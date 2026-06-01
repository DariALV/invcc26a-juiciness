class_name StudentIDMenu extends Control

@export var ids: Array[String] = []

@export var student_id_scene: PackedScene

@onready var v_box_container = $NinePatchRect/MarginContainer/VBoxContainer/PanelContainer/MarginContainer/ScrollContainer/VBoxContainer
@onready var search_bar: LineEdit = $NinePatchRect/MarginContainer/VBoxContainer/LineEdit

func _ready():
	search_bar.text_changed.connect(on_search_text_changed)
	for id in ids:
		var student_id_container: StudentIDContainer = student_id_scene.instantiate() as StudentIDContainer
		student_id_container.text = id
		v_box_container.add_child(student_id_container)

func on_search_text_changed(new_text: String):
	for child in v_box_container.get_children():
		if child is StudentIDContainer:
			var id_container: StudentIDContainer = child as StudentIDContainer
			id_container.visible = new_text.is_empty() or id_container.text.contains(new_text)
