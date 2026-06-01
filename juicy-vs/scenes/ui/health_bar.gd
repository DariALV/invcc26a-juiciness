class_name HealthBar extends ProgressBar

var tween: Tween

func _ready():
	EventBus.player_health_changed.connect(on_health_changed)

func on_health_changed(before: float, after: float):
	reset_tween()
	tween.tween_property(self, "value", after, 2)

func reset_tween():
	if tween:
		tween.kill()
	tween = create_tween()
