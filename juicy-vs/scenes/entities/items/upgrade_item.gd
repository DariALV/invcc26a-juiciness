class_name UpgradeItem extends CharacterBody2D

@onready var pickup_component: PickupComponent = $PickupComponent

var upgrade: Upgrade

func _ready():
	pickup_component.collision.connect(on_pickup_collision)
	upgrade = UpgradeManager.get_random_upgrade()

func on_pickup_collision(area: Area2D):
	UpgradeManager.take_upgrade(upgrade)
	queue_free()
