extends Node

signal upgrade_taken(upgrade: Upgrade)
signal projectile_upgrade_taken(upgrade: ProjectileUpgrade)
signal weapon_upgrade_taken(upgrade: WeaponUpgrade)

@export var upgrade_item_scene: PackedScene
@export var available_upgrades: Array[Upgrade] = []
@export var rarity_colors: Array[UpgradeColor] = []
## Base weight per rarity when rolling upgrades, indexed by Upgrade.Rarity (COMMON, UNCOMMON, RARE, EPIC, LEGENDARY).
@export var rarity_weights: Array[float] = [70.0, 20.0, 7.0, 2.5, 0.5]

var projectile_upgrades: Array[ProjectileUpgrade] = []
var weapon_upgrades: Array[WeaponUpgrade] = []

## Multiplicador de daño global de proyectiles, aportado por mejoras híbridas (Venganza:
## además de las flechas de muerte da +% de daño base). Se aplica a TODAS las flechas
## (normales y de muerte), así Venganza también potencia sus propias flechas. Se reinicia
## en reset_run_state.
var bonus_damage_mult: float = 1.0

## Rerolls disponibles en la SELECCION actual. Se renuevan en cada subida de nivel a
## 'rerolls_per_level' (ver replenish_rerolls) y los consume el boton de reroll.
var rerolls_available: int = 0

## Rerolls que se otorgan por CADA subida de nivel. No es un total acumulado: cada
## mejora de Reroll comprada (epica / legendaria) suma su aporte aqui, y al subir de
## nivel 'rerolls_available' se restablece a este valor. Con la epica + la legendaria
## se obtienen 2 rerolls por nivel.
var rerolls_per_level: int = 0

## Registra una fuente de rerolls por nivel (lo llaman las mejoras de Reroll al tomarse).
func add_reroll_source(per_level: int) -> void:
	rerolls_per_level += per_level

## Restablece los rerolls de la seleccion a la cuota por nivel. Se llama al inicio de
## cada seleccion (una por subida de nivel).
func replenish_rerolls() -> void:
	rerolls_available = rerolls_per_level

## Consume un reroll si hay disponibles. Devuelve true si se pudo.
func consume_reroll() -> bool:
	if rerolls_available <= 0:
		return false
	rerolls_available -= 1
	return true

## Veces que se ha tomado cada mejora en la partida actual (Upgrade -> int).
## Se usa para respetar 'max_taken'. Se limpia al reiniciar la ronda.
var times_taken: Dictionary = {}

## Peso individual por mejora (Upgrade -> float), AISLADO del peso por rareza.
## La rareza se elige primero (rarity_weights); luego, dentro de esa rareza, se
## escoge una mejora segun este peso. Empieza en 1, sube un poco si la mejora no
## aparece entre las ofrecidas, baja si aparece, y se reinicia a 1 si se elige.
var upgrade_weights: Dictionary = {}

const WEIGHT_MIN := 1.0
const WEIGHT_MAX := 10.0
## Incremento cuando la mejora NO aparece entre las ofrecidas (subida ligera).
const WEIGHT_UP := 0.3
## Decremento cuando la mejora SI aparece (pero no se elige). Baja mas de golpe.
const WEIGHT_DOWN := 0.8
## Peso al que se reinicia una mejora cuando se elige.
const WEIGHT_CHOSEN := 1.0

func get_random_upgrade():
	return available_upgrades.pick_random()

func get_upgrade_choices(count: int) -> Array[Upgrade]:
	var pool := _available_pool()
	var result: Array[Upgrade] = []
	while result.size() < count and not pool.is_empty():
		# 1) Se elige la rareza segun rarity_weights (+ suerte).
		var rarity := _pick_rarity(pool)
		# 2) Dentro de esa rareza, se elige una mejora segun su peso individual.
		var pick := _pick_in_rarity(pool, rarity)
		if pick == null:
			break
		result.push_back(pick)
		pool.erase(pick)
	_adjust_weights_after_offer(result)
	return result

## Mejoras que todavia se pueden ofrecer: descarta las que alcanzaron su limite y las que
## aun no estan desbloqueadas (p. ej. las de aura sin tener daño en área).
func _available_pool() -> Array[Upgrade]:
	var pool: Array[Upgrade] = []
	for u in available_upgrades:
		if can_take(u) and u.is_unlocked():
			pool.push_back(u)
	return pool

## True si la mejora aun no agoto su 'max_taken' (o es infinita).
func can_take(upgrade: Upgrade) -> bool:
	if upgrade.infinite:
		return true
	var taken: int = times_taken.get(upgrade, 0)
	return taken < maxi(1, upgrade.max_taken)

## Etiqueta de nivel para la carta: "Nivel N/M" para apilables con tope, "Nivel N"
## para infinitas, y "" para las de un solo uso (no tienen niveles). N es el nivel
## que tendria la mejora si se toma ahora.
func get_level_label(upgrade: Upgrade) -> String:
	var next: int = times_taken.get(upgrade, 0) + 1
	if upgrade.infinite:
		return "Nivel %d" % next
	if maxi(1, upgrade.max_taken) > 1:
		return "Nivel %d/%d" % [mini(next, upgrade.max_taken), upgrade.max_taken]
	return ""

func _get_weight(u: Upgrade) -> float:
	return upgrade_weights.get(u, 1.0)

## Elige una rareza entre las presentes en el pool, segun rarity_weights y la suerte
## (que sesga hacia rarezas mas altas).
func _pick_rarity(pool: Array[Upgrade]) -> int:
	var luck: float = ExperienceManager.luck
	var rarities: Array[int] = []
	var weights: Array[float] = []
	var total := 0.0
	var seen := {}
	for u in pool:
		if seen.has(u.rarity):
			continue
		seen[u.rarity] = true
		var base_weight: float = rarity_weights[u.rarity] if u.rarity < rarity_weights.size() else 1.0
		var w: float = base_weight * pow(1.0 + luck, u.rarity)
		rarities.push_back(u.rarity)
		weights.push_back(w)
		total += w
	var r := randf() * total
	var acc := 0.0
	for i in rarities.size():
		acc += weights[i]
		if r <= acc:
			return rarities[i]
	return rarities.back()

## Dentro de una rareza, elige una mejora segun su peso individual.
func _pick_in_rarity(pool: Array[Upgrade], rarity: int) -> Upgrade:
	var candidates: Array[Upgrade] = []
	var total := 0.0
	for u in pool:
		if u.rarity == rarity:
			candidates.push_back(u)
			total += _get_weight(u)
	if candidates.is_empty():
		return null
	var r := randf() * total
	var acc := 0.0
	for u in candidates:
		acc += _get_weight(u)
		if r <= acc:
			return u
	return candidates.back()

## Tras ofrecer 'offered', sube ligeramente el peso de las que NO aparecieron y baja
## el de las que SI. Las elegidas se reinician aparte (en take_upgrade).
func _adjust_weights_after_offer(offered: Array[Upgrade]) -> void:
	for u in available_upgrades:
		if u in offered:
			upgrade_weights[u] = clampf(_get_weight(u) - WEIGHT_DOWN, WEIGHT_MIN, WEIGHT_MAX)
		else:
			upgrade_weights[u] = clampf(_get_weight(u) + WEIGHT_UP, WEIGHT_MIN, WEIGHT_MAX)

func take_upgrade(upgrade: Upgrade):
	times_taken[upgrade] = times_taken.get(upgrade, 0) + 1
	upgrade_weights[upgrade] = WEIGHT_CHOSEN
	if upgrade is ProjectileUpgrade:
		projectile_upgrades.push_back(upgrade)
		upgrade_taken.emit(upgrade)
		projectile_upgrade_taken.emit(upgrade)
	elif upgrade is WeaponUpgrade:
		weapon_upgrades.push_back(upgrade)
		upgrade_taken.emit(upgrade)
		weapon_upgrade_taken.emit(upgrade)
	elif upgrade is PlayerUpgrade:
		upgrade.apply_upgrade()
		upgrade_taken.emit(upgrade)

func apply_projectile_upgrades(projectile: BaseProjectile):
	for u in projectile_upgrades:
		u.apply_upgrade(projectile)
	# Bono de daño global de mejoras híbridas (Venganza). Se aplica al final, sobre el daño
	# ya mejorado, para que escale con el resto del build.
	if bonus_damage_mult != 1.0:
		projectile.damage *= bonus_damage_mult

func apply_weapon_upgrades(bow: Bow):
	for u in weapon_upgrades:
		u.apply_upgrade(bow)

func spawn_upgrade_item(pos: Vector2):
	if LevelManager.y_sort_entities and upgrade_item_scene.can_instantiate():
		var upgrade_item: UpgradeItem = upgrade_item_scene.instantiate()
		upgrade_item.global_position = pos
		LevelManager.y_sort_entities.call_deferred("add_child", upgrade_item)

func get_upgrade_color(upgrade: Upgrade) -> UpgradeColor:
	for c in rarity_colors:
		if upgrade.rarity == c.rarity:
			return c
	return null
