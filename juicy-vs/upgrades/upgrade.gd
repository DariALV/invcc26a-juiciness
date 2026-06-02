class_name Upgrade extends Resource

enum Rarity {
	COMMON = 0,
	UNCOMMON,
	RARE,
	EPIC,
	LEGENDARY
}

@export var title: String = "Mejora"
@export_multiline var description: String = "No Description"
@export var rarity: Rarity = Rarity.COMMON
## Optional background texture for the upgrade card.
@export var background_texture: Texture2D
