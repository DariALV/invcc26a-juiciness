class_name HowToPlay extends CanvasLayer

## Pantalla inicial de controles. Aparece con el juego en PAUSA al empezar el nivel:
## muestra la textura de controles (Moverse / Apuntar / Pausar) y un boton
## "Continuar" que reanuda el juego y se cierra.
##
## Procesa siempre (PROCESS_MODE_ALWAYS) para poder responder al boton con el arbol
## pausado.

@onready var continue_button: Button = %ContinueButton

func _ready() -> void:
	process_mode = Node.PROCESS_MODE_ALWAYS
	continue_button.pressed.connect(_on_continue_pressed)
	# Arranca el nivel en pausa hasta que el jugador lea los controles.
	get_tree().paused = true
	continue_button.grab_focus()

func _on_continue_pressed() -> void:
	get_tree().paused = false
	queue_free()
