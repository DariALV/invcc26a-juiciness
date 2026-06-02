class_name SpeedWeaponUpgrade extends WeaponUpgrade

@export var speed_increase: float = 0.1

func apply_upgrade(bow: Bow):
	# El setter de shoot_speed propaga el nuevo ritmo al ShootComponent.
	bow.shoot_speed += speed_increase
