class_name UpgradesTaken extends Control

@export var upgrade_desc_scene: PackedScene

@onready var upgrades_list = $MarginContainer/VBoxContainer

func _ready():
	UpgradeManager.upgrade_taken.connect(on_upgrade_taken)

func on_upgrade_taken(upgrade: Upgrade):
	var upgrade_desc: UpgradeDescription = upgrade_desc_scene.instantiate()
	upgrade_desc.text = upgrade.description
	upgrade_desc.add_theme_color_override("font_color", UpgradeManager.get_upgrade_color(upgrade).outline_color)
	upgrades_list.add_child(upgrade_desc)
