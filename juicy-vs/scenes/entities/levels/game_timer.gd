extends Timer

@onready var label = $"../CanvasLayer/Label"

func _process(delta):
	label.text = _format_time(wait_time - time_left)

## Formatea segundos como "mm:ss" (p. ej. 75.0 -> "01:15").
func _format_time(secs: float) -> String:
	var total := int(secs)
	return "%02d:%02d" % [total / 60, total % 60]
