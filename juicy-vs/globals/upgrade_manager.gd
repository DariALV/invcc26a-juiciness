extends Node

signal upgrade_taken(upgrade: Upgrade)
signal projectile_upgrade_taken(upgrade: ProjectileUpgrade)
signal weapon_upgrade_taken(upgrade: WeaponUpgrade)

@export var upgrade_item_scene: PackedScene
@export var available_upgrades: Array[Upgrade] = []

var spawn_node: Node2D

var projectile_upgrades: Array[ProjectileUpgrade] = []
var weapon_upgrades: Array[WeaponUpgrade] = []

func get_random_upgrade():
	return available_upgrades.pick_random()

func take_upgrade(upgrade: Upgrade):
	if upgrade is ProjectileUpgrade:
		projectile_upgrades.push_back(upgrade)
		upgrade_taken.emit(upgrade)
		projectile_upgrade_taken.emit(upgrade)
	elif upgrade is WeaponUpgrade:
		weapon_upgrades.push_back(upgrade)
		upgrade_taken.emit(upgrade)
		weapon_upgrade_taken.emit(upgrade)

func apply_projectile_upgrades(arrow: Arrow):
	for u in projectile_upgrades:
		u.apply_upgrade(arrow)

func apply_weapon_upgrades(bow: Bow):
	for u in weapon_upgrades:
		u.apply_upgrade(bow)

func spawn_upgrade_item(pos: Vector2):
	if spawn_node and upgrade_item_scene.can_instantiate():
		var upgrade_item: UpgradeItem = upgrade_item_scene.instantiate()
		upgrade_item.global_position = pos
		spawn_node.call_deferred("add_child", upgrade_item)
