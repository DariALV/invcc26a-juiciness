class_name UpgradeDescription extends Label

var tween: Tween

func _ready():
	reset_tween()
	var modulate_color = modulate
	modulate = Color(modulate.r, modulate.g, modulate.b, 0)
	tween.tween_property(self, "modulate", modulate_color, 1)
	tween.finished.connect(on_appeared_tween_finished)

func on_appeared_tween_finished():
	reset_tween()
	var modulate_color = Color(modulate.r, modulate.g, modulate.b, 0)
	tween.tween_property(self, "modulate", modulate_color, 1)
	tween.finished.connect(on_disappeared_tween_finished)

func on_disappeared_tween_finished():
	queue_free()

func reset_tween():
	if tween:
		tween.kill()
	tween = create_tween()
