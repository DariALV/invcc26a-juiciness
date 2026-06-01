class_name Upgrade extends Resource

enum UpgradeType {
	DAMAGE = 0,
	HEALTH,
	SPEED,
	ABILITY
}

@export var description: String = "No Description"
@export var type: UpgradeType
