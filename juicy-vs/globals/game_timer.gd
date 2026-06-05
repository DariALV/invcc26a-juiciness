extends Timer

@onready var label: Label

var timer_finished: bool = false

var last_runs_time: float = 0
var current_run_time: float = 0

func _ready():
	timeout.connect(on_timer_timeout)

func set_label(new_label: Label):
	label = label

func _process(delta):
	if label:
		label.text = _format_time(wait_time - time_left)
		current_run_time = wait_time - time_left - last_runs_time

## Formatea segundos como "mm:ss" (p. ej. 75.0 -> "01:15").
func _format_time(secs: float) -> String:
	var total := int(secs)
	return "%02d:%02d" % [total / 60, total % 60]

func on_timer_timeout():
	timer_finished = true
	if LevelManager.player:
		LevelManager.player.health.apply_damage(200000000000)
