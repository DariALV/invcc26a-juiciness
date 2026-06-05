extends Node

## Autoload central de audio.
##
## Aqui se cargan (via @export, editables en audio_manager.tscn desde el inspector)
## la musica de fondo y los efectos de sonido del juego: disparo, impacto, golpe al
## jugador, subida de nivel, mejora elegida y muerte de enemigo. Cada efecto se
## reproduce SOLO si su stream fue asignado, de modo que el juego sigue funcionando
## aunque todavia no existan los audios (evita errores por recursos faltantes).
##
## En _ready() crea los buses "Music" y "SFX" (enrutados a "Master") para que el
## menu de pausa pueda regular cada volumen por separado. Procesa siempre
## (PROCESS_MODE_ALWAYS) para que los efectos de subida de nivel / mejora elegida
## suenen aunque el arbol este pausado por la seleccion de mejoras o el menu.

const MASTER_BUS := "Master"
const MUSIC_BUS := "Music"
const SFX_BUS := "SFX"

@export_group("Musica")
@export var music: AudioStream

@export_group("Efectos")
## Disparo del jugador (cada rafaga del arco).
@export var sfx_shoot: AudioStream
## Impacto de una flecha sobre un enemigo (o sobre un proyectil destruible).
@export var sfx_hit: AudioStream
## El jugador recibe dano.
@export var sfx_player_hit: AudioStream
## Subida de nivel (aparece la seleccion de mejoras).
@export var sfx_level_up: AudioStream
## Se elige una mejora en la seleccion.
@export var sfx_upgrade_chosen: AudioStream
## Muerte de un enemigo / proyectil destruible.
@export var sfx_enemy_death: AudioStream

var _music_player: AudioStreamPlayer

func _ready() -> void:
	process_mode = Node.PROCESS_MODE_ALWAYS
	_ensure_bus(MUSIC_BUS)
	_ensure_bus(SFX_BUS)
	# Arranca a la mitad de volumen (antes iniciaba muy fuerte). Música y efectos por
	# separado (no Master) para que AMBOS bajen y el slider Master conserve su rango.
	set_bus_volume(MUSIC_BUS, 0.25)
	set_bus_volume(SFX_BUS, 0.05)
	_music_player = AudioStreamPlayer.new()
	_music_player.bus = MUSIC_BUS
	_music_player.process_mode = Node.PROCESS_MODE_ALWAYS
	add_child(_music_player)
	_music_player.finished.connect(_on_music_finished)

## Garantiza que exista el bus de audio dado (enrutado a Master) y devuelve su indice.
func _ensure_bus(bus_name: String) -> int:
	var idx := AudioServer.get_bus_index(bus_name)
	if idx == -1:
		idx = AudioServer.bus_count
		AudioServer.add_bus(idx)
		AudioServer.set_bus_name(idx, bus_name)
		AudioServer.set_bus_send(idx, MASTER_BUS)
	return idx

# --- Musica ----------------------------------------------------------------

func play_music() -> void:
	if music == null or _music_player == null:
		return
	_music_player.stream = music
	_music_player.play()

func stop_music() -> void:
	if _music_player:
		_music_player.stop()

func _on_music_finished() -> void:
	# Loop simple por si el recurso de musica no trae loop propio.
	if music and _music_player:
		_music_player.play()

# --- Efectos ---------------------------------------------------------------

## Reproduce un efecto puntual en el bus SFX. Crea un reproductor temporal que se
## libera al terminar; no hace nada si 'stream' es null (efecto sin asignar).
func play_sfx(stream: AudioStream, pitch_variation: float = 0.0) -> void:
	if stream == null:
		return
	var p := AudioStreamPlayer.new()
	p.stream = stream
	p.bus = SFX_BUS
	p.process_mode = Node.PROCESS_MODE_ALWAYS
	if pitch_variation > 0.0:
		p.pitch_scale = 1.0 + randf_range(-pitch_variation, pitch_variation)
	add_child(p)
	p.finished.connect(p.queue_free)
	p.play()

func play_shoot() -> void:
	play_sfx(sfx_shoot, 0.08)

func play_hit() -> void:
	play_sfx(sfx_hit, 0.1)

func play_player_hit() -> void:
	play_sfx(sfx_player_hit)

func play_level_up() -> void:
	play_sfx(sfx_level_up)

func play_upgrade_chosen() -> void:
	play_sfx(sfx_upgrade_chosen)

func play_enemy_death() -> void:
	play_sfx(sfx_enemy_death)

# --- Volumen (lo usa el menu de pausa). 'linear' en 0..1 -------------------

func set_bus_volume(bus_name: String, linear: float) -> void:
	var idx := AudioServer.get_bus_index(bus_name)
	if idx == -1:
		return
	linear = clampf(linear, 0.0, 1.0)
	AudioServer.set_bus_mute(idx, linear <= 0.001)
	AudioServer.set_bus_volume_db(idx, linear_to_db(maxf(linear, 0.0001)))

func get_bus_volume(bus_name: String) -> float:
	var idx := AudioServer.get_bus_index(bus_name)
	if idx == -1:
		return 1.0
	if AudioServer.is_bus_mute(idx):
		return 0.0
	return clampf(db_to_linear(AudioServer.get_bus_volume_db(idx)), 0.0, 1.0)
