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

## Cuantas veces se puede tomar esta mejora en una misma partida. Cuando se
## alcanza el limite, deja de aparecer en las cartas de seleccion. Se ignora si
## 'infinite' es true.
@export var max_taken: int = 1
## Si es true, la mejora se puede tomar sin limite (apilable infinitamente) y
## 'max_taken' se ignora.
@export var infinite: bool = false

## Si una mejora depende de otra para tener sentido (p. ej. las de aura dependen de
## tener daño en área), sobrescribe esto para que solo aparezca cuando el prerequisito
## esté cumplido. Por defecto siempre está desbloqueada.
func is_unlocked() -> bool:
	return true
