extends Node

signal xp_changed(current_xp: float, xp_to_next: float)
signal level_changed(level: int)
signal leveled_up(level: int)
signal reached_max_level

@export_group("Levels")
## Maximum reachable level; past it no XP is gained and no level-ups occur.
@export var max_level: int = 30
## XP required to go from level 1 to level 2 (base of the curve).
@export var base_xp: float = 5.0
## Per-level XP multiplier: each level needs growth_factor times more than the previous.
@export var growth_factor: float = 1.2

@export_group("Global Stats")
## Base player luck. Biases the rarity of offered upgrades toward higher tiers.
@export var base_luck: float = 0.0

var current_level: int = 1
var current_xp: float = 0.0
var luck: float = 0.0
## Multiplies all incoming XP. Raised by XP-gain upgrades.
var xp_multiplier: float = 1.0

func _ready() -> void:
	luck = base_luck
	xp_changed.emit(current_xp, xp_to_next_level())

func xp_to_next_level() -> float:
	return base_xp * pow(growth_factor, current_level - 1)

func is_max_level() -> bool:
	return current_level >= max_level

func add_xp(amount: float) -> void:
	if amount <= 0.0 or is_max_level():
		return
	current_xp += amount * xp_multiplier
	var need := xp_to_next_level()
	while not is_max_level() and current_xp >= need:
		current_xp -= need
		current_level += 1
		level_changed.emit(current_level)
		leveled_up.emit(current_level)
		need = xp_to_next_level()
	if is_max_level():
		current_xp = 0.0
		reached_max_level.emit()
	xp_changed.emit(current_xp, xp_to_next_level())
