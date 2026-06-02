class_name LuckUpgrade extends PlayerUpgrade

@export var luck_increase: float = 0.25

func apply_upgrade() -> void:
	ExperienceManager.luck += luck_increase
