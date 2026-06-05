extends Area2D
class_name HurtboxComponent

signal collision(area: Area2D)

@export var health : HealthComponent
@export var one_hit_per_area: bool = true

## Manejador de defensa opcional (lo asigna el jugador). Si responde a estos metodos,
## intercepta el golpe: try_dodge() y try_reflect(hitbox) pueden anular el dano, y
## on_damage_taken() se llama tras un golpe efectivo (invulnerabilidad/knockback).
var defense: Node = null

var ignore_collision: Dictionary = {}

func _on_area_entered(area : Area2D):
	if one_hit_per_area:
		var id := area.get_instance_id()
		if ignore_collision.has(id):
			return
		ignore_collision[id] = true
	if area is HitboxComponent and health and not health.isDead:
		# Defensa del jugador: esquivar o reflejar antes de aplicar el dano.
		if defense:
			if defense.has_method("try_dodge") and defense.try_dodge():
				return
			if defense.has_method("try_reflect") and defense.try_reflect(area):
				return
		collision.emit(area)
		# Capturamos la vida realmente removida (dealt): la "perforacion por dano" del
		# proyectil descuenta ese valor de su presupuesto de dano para decidir si sigue.
		var hp_before: float = health.current_health
		health.apply_damage(area.damage, false, area.damage_source)
		var dealt: float = hp_before - health.current_health
		# Avisa al hitbox que el golpe impacto (y cuanto dano hizo), para que el
		# proyectil se consuma solo cuando de verdad hizo daño.
		area.register_hit(self, dealt)
		# Tras un golpe efectivo: invulnerabilidad temporal + knockback (jugador).
		if defense and defense.has_method("on_damage_taken"):
			defense.on_damage_taken()
