extends Camera2D
class_name JuicyCamera

## Camara con efectos de "juiciness": shake, recoil (jolt posicional) y zoom punch.
##
## Los tres efectos son configurables desde el inspector y se disparan a traves del
## autoload CameraJuice (que respeta los flags por participante de Supabase). La
## camara se registra en CameraJuice en su _ready para que se pueda invocar desde
## cualquier lado sin tener una referencia directa.

@export var random_strength: float = 30
@export var shake_fade: float = 5.0

@export var min_zoom_scale: float = 1.5
## Velocidad del lerp del zoom base (segun la cantidad de enemigos). 1.0 conserva el
## comportamiento original (zoom lento).
@export var zoom_lerp_speed: float = 1.0

@export_group("Recoil")
## Desplazamiento (px) del golpe de retroceso de la camara.
@export var recoil_strength: float = 8.0
## Cuanto tarda el retroceso en volver a su sitio (segundos).
@export var recoil_duration: float = 0.25

@export_group("Zoom punch")
## Cuanto acerca el "punch" de zoom (sumado al zoom base). 0.15 = +15%.
@export var zoom_punch_amount: float = 0.15
## Duracion total del punch de zoom (acercar + volver), en segundos.
@export var zoom_punch_duration: float = 0.35

@export var target: Node2D = null

var rng = RandomNumberGenerator.new()

var shake_strength: float = 0

## Zoom base suavizado (segun enemigos vivos); el punch se suma encima.
var _base_zoom: Vector2 = Vector2.ONE
## Acercamiento transitorio del zoom punch (lo tweenea apply_zoom).
var _zoom_punch: float = 0.0
## Desplazamiento transitorio del recoil (lo tweenea apply_recoil).
var _recoil_offset: Vector2 = Vector2.ZERO
var _zoom_tween: Tween
var _recoil_tween: Tween

func _ready():
	CameraJuice.register_camera(self)

func _exit_tree():
	CameraJuice.unregister_camera(self)

# --- Efectos ---------------------------------------------------------------

## Sacude la camara. 'intensity' < 0 usa random_strength.
func apply_shake(intensity: float = -1.0):
	shake_strength = random_strength

## Acerca la camara de golpe y la devuelve. Sin argumentos usa los valores del
## inspector (zoom_punch_amount / zoom_punch_duration).
func apply_zoom(amount: float = -1.0, duration: float = -1.0):
	var amt := zoom_punch_amount if amount < 0.0 else amount
	var dur := zoom_punch_duration if duration < 0.0 else duration
	if _zoom_tween and _zoom_tween.is_running():
		_zoom_tween.kill()
	_zoom_tween = create_tween()
	_zoom_tween.tween_property(self, "_zoom_punch", amt, dur * 0.3) \
		.set_trans(Tween.TRANS_SINE).set_ease(Tween.EASE_OUT)
	_zoom_tween.tween_property(self, "_zoom_punch", 0.0, dur * 0.7) \
		.set_trans(Tween.TRANS_SINE).set_ease(Tween.EASE_IN)

## Da un golpe de retroceso en 'direction' (no hace falta normalizarla) y lo devuelve
## con un rebote elastico. Sin direccion patea hacia la derecha.
func apply_recoil(direction: Vector2 = Vector2.RIGHT):
	var dir := direction.normalized()
	if dir == Vector2.ZERO:
		dir = Vector2.RIGHT
	if _recoil_tween and _recoil_tween.is_running():
		_recoil_tween.kill()
	_recoil_offset = dir * recoil_strength
	_recoil_tween = create_tween()
	_recoil_tween.tween_property(self, "_recoil_offset", Vector2.ZERO, recoil_duration) \
		.set_trans(Tween.TRANS_BACK).set_ease(Tween.EASE_OUT)

func _process(delta):
	# Zoom base: se aleja segun la cantidad de enemigos vivos; el punch se suma encima.
	var crowd := clampf(1.0 + Database.enemies_alive / 15.0, 1.0, min_zoom_scale)
	_base_zoom = _base_zoom.lerp(Vector2.ONE / crowd, clampf(zoom_lerp_speed * delta, 0.0, 1.0))
	zoom = _base_zoom + Vector2.ONE * _zoom_punch

	if target:
		global_position = target.global_position

	if shake_strength > 0:
		shake_strength = lerpf(shake_strength, 0, shake_fade * delta)

	offset = randomOffset() + _recoil_offset

func randomOffset() -> Vector2:
	return Vector2(rng.randf_range(-shake_strength, shake_strength), rng.randf_range(-shake_strength, shake_strength))
