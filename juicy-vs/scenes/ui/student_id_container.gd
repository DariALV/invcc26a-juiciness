class_name StudentIDContainer extends Control

@export var text: String = ""

@onready var label = $NinePatchRect/Label
@onready var button: Button = $NinePatchRect/Button

var tween: Tween

func _ready():
	label.text = text
	button.action_mode = BaseButton.ACTION_MODE_BUTTON_PRESS
	
	button.mouse_entered.connect(on_mouse_entered)
	button.mouse_exited.connect(on_mouse_exited)
	button.pressed.connect(on_button_pressed)
	
	

func on_mouse_entered():
	if tween:
		tween.kill()
	tween = create_tween()
	tween.tween_property(self, "scale", Vector2(1.1, 1.1), 0.1)

func on_mouse_exited():
	if tween:
		tween.kill()
	tween = create_tween()
	tween.tween_property(self, "scale", Vector2(1, 1), 0.1)

func on_button_pressed():
	Database.selected_id = text
	get_tree().call_deferred("change_scene_to_file", "res://scenes/entities/levels/level.tscn")
	
	
