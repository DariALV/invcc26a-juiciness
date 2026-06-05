extends Node

## Punto de acceso global a los efectos de la JuicyCamera (shake / zoom / recoil).
##
## La camara activa se registra aqui en su _ready, de modo que cualquier parte del
## juego pueda disparar un efecto sin tener una referencia a la camara:
##     CameraJuice.shake(30)
##     CameraJuice.recoil(-dir)
##     CameraJuice.zoom()
##
## Cada efecto se reproduce SOLO si su flag esta activo. Los flags se cargan por
## participante desde la tabla PlayerIDConfig de Supabase (columnas camera_shake,
## camera_zoom, camera_recoil): asi el estudio de "juiciness" enciende o apaga cada
## efecto segun el ID seleccionado. Si no hay config (o aun no cargo), por defecto
## los tres estan activos.

var _camera: JuicyCamera = null

var shake_enabled: bool = true
var zoom_enabled: bool = true
var recoil_enabled: bool = true

func _ready() -> void:
	# El zoom notable se dispara al subir de nivel (momento de progresion, separado
	# del disparo y del dano). Si suben varios niveles de golpe, apply_zoom reinicia el
	# tween, asi que se ve un unico punch.
	ExperienceManager.leveled_up.connect(_on_leveled_up)

func _on_leveled_up(_level: int) -> void:
	zoom()

func register_camera(cam: JuicyCamera) -> void:
	_camera = cam

func unregister_camera(cam: JuicyCamera) -> void:
	if _camera == cam:
		_camera = null

## Pide a Supabase la config del participante y aplica sus flags (asincrono).
func load_config(player_id: String) -> void:
	if player_id == "":
		return
	Database.get_player_config(player_id, _on_config_loaded)

func _on_config_loaded(config: Dictionary) -> void:
	if config.is_empty():
		return
	shake_enabled = bool(config.get("camera_shake", true))
	zoom_enabled = bool(config.get("camera_zoom", true))
	recoil_enabled = bool(config.get("camera_recoil", true))

# --- Efectos (cada uno respeta su flag) ------------------------------------

func shake(intensity: float = 30.0) -> void:
	if shake_enabled and is_instance_valid(_camera):
		_camera.apply_shake(intensity)

func zoom(amount: float = -1.0, duration: float = -1.0) -> void:
	if zoom_enabled and is_instance_valid(_camera):
		_camera.apply_zoom(amount, duration)

func recoil(direction: Vector2 = Vector2.RIGHT) -> void:
	if recoil_enabled and is_instance_valid(_camera):
		_camera.apply_recoil(direction)
