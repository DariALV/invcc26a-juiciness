class_name MagnetUpgrade extends PlayerUpgrade

## Amplia el radio de recogida del jugador escalando el CollectorComponent
## (la escala del Area2D arrastra a su forma de colision hija).
@export var scale_multiplier: float = 1.3

func apply_upgrade() -> void:
	var collector: Node = LevelManager.player.get_node_or_null("CollectorComponent")
	if collector and collector is Node2D:
		(collector as Node2D).scale *= scale_multiplier
