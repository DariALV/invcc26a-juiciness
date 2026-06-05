extends Node
class_name HealthComponent

signal died
signal health_changed(before: float, after: float)
## Emitida al recibir dano, con la cantidad real aplicada y si proviene de fuego
## (para los numeros de dano flotantes).
signal damaged(amount: float, is_fire: bool)

@export var current_health: float = 100
@export var max_health: float = 100
@export var min_health: float = 0

## Si es true, ignora todo el dano entrante (usado por el menu de debug).
@export var invincible: bool = false

## Vida regenerada por segundo. La suben las mejoras de regeneracion; 0 = sin
## regeneracion. Se acumula entre frames para no emitir cambios minusculos cada uno.
@export var regen_per_second: float = 0.0

var isDead : bool = false

func _process(delta: float) -> void:
	if regen_per_second <= 0.0 or isDead or current_health >= max_health:
		return
	# La regeneracion se aplica POR FRAME (no en saltos de 1 HP por segundo): con 60 fps
	# y 1 HP/s, suma 1/60 HP cada frame, de modo que la vida sube de forma continua y se
	# ve subir la barra suavemente.
	apply_heal(regen_per_second * delta)

func apply_heal(amount : float):
	if !isDead:
		var before_health = current_health
		current_health = min(max_health, current_health + amount)
		if (before_health != current_health):
			health_changed.emit(before_health, current_health)

## 'source' es el canal de dano del jugador (arrow, crit, burn, aura, death_arrows).
## Si llega vacio no se atribuye (p. ej. el dano que recibe el propio jugador).
func apply_damage(amount : float, is_fire: bool = false, source: String = ""):
	if invincible:
		return
	var before_health: float = current_health
	current_health = max(min_health, current_health - amount)
	var dealt: float = before_health - current_health
	if (before_health != current_health):
		health_changed.emit(before_health, current_health)
		damaged.emit(dealt, is_fire)
	var killed := false
	if current_health == 0 and not isDead:
		isDead = true
		killed = true
		died.emit()
	# Atribucion de dano por canal (solo el dano ofensivo del jugador trae 'source').
	if source != "" and dealt > 0.0:
		Database.record_channel_damage(source, dealt, killed)
