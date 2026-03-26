extends Camera2D


# Called when the node enters the scene tree for the first time.
func _ready() -> void:
	pass # Replace with function body.


# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(delta: float) -> void:
	pass
	
func _input(event: InputEvent) -> void:
	if event is InputEventMouseButton:
		if event.is_pressed():
			if event.button_index == MOUSE_BUTTON_WHEEL_UP:
				if (zoom.x <= 1):
					zoom /= 2
				elif (zoom.x <= 3):
					zoom -= Vector2.ONE
				print("Zoom hacia arriba")
				print(zoom)

			if event.button_index == MOUSE_BUTTON_WHEEL_DOWN:
				if (zoom.x <= 1):
					zoom *= 2
				elif (zoom.x >= 0.1):
					zoom += Vector2.ONE
				print("Zoom hacia abajo")
				print(zoom)
