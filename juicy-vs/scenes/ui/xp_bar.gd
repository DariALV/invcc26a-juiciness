class_name XPBar extends ProgressBar

@onready var level_label: Label = $LevelLabel

var tween: Tween

func _ready():
	min_value = 0.0
	show_percentage = false
	ExperienceManager.xp_changed.connect(on_xp_changed)
	ExperienceManager.level_changed.connect(on_level_changed)
	on_level_changed(ExperienceManager.current_level)
	on_xp_changed(ExperienceManager.current_xp, ExperienceManager.xp_to_next_level())

func on_level_changed(level: int):
	level_label.text = "Lv " + str(level)

func on_xp_changed(current_xp: float, xp_to_next: float):
	max_value = max(xp_to_next, 1.0)
	if ExperienceManager.is_max_level():
		value = max_value
		return
	reset_tween()
	tween.tween_property(self, "value", current_xp, 0.3) \
		.set_trans(Tween.TRANS_CIRC) \
		.set_ease(Tween.EASE_OUT)

func reset_tween():
	if tween:
		tween.kill()
	tween = create_tween()
