class_name SpeedWeaponUpgrade extends WeaponUpgrade

@export var speed_increase: float = 0.1

func apply_upgrade(bow: Bow):
	bow.shoot_speed += speed_increase
	bow.shoot_timer.wait_time = 1 / (bow.shoot_speed)
