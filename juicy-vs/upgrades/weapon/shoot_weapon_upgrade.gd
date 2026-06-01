class_name ShootWeaponUpgrade extends WeaponUpgrade

@export var extra_arrow_count: int = 1

func apply_upgrade(bow: Bow):
	bow.arrow_count += extra_arrow_count
