class_name UpgradeColor extends Resource

enum UpgradeType {
	DAMAGE = 0,
	HEALTH,
	SPEED,
	ABILITY
}

@export var type: UpgradeType
@export var outline_color: Color
@export var text_color: Color
