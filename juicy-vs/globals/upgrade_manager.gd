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

func get_random_upgrade():
	return available_upgrades.pick_random()

func get_upgrade_choices(count: int) -> Array[Upgrade]:
	var pool := available_upgrades.duplicate()
	var result: Array[Upgrade] = []
	while result.size() < count and not pool.is_empty():
		var pick := _weighted_pick(pool)
		result.push_back(pick)
		pool.erase(pick)
	return result

func _weighted_pick(pool: Array[Upgrade]) -> Upgrade:
	var luck: float = ExperienceManager.luck
	var weights: Array[float] = []
	var total := 0.0
	for u in pool:
		var base_weight: float = rarity_weights[u.rarity] if u.rarity < rarity_weights.size() else 1.0
		var w: float = base_weight * pow(1.0 + luck, u.rarity)
		weights.push_back(w)
		total += w
	var r := randf() * total
	var acc := 0.0
	for i in pool.size():
		acc += weights[i]
		if r <= acc:
			return pool[i]
	return pool.back()

func take_upgrade(upgrade: Upgrade):
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
