extends Area2D
class_name HitboxComponent

signal collision_detected()

@export var damage : float = 1
@export var one_hit_per_area: bool = true

var ignore_collision: Dictionary = {}

func _ready():
	area_entered.connect(_on_area_entered)

func _on_area_entered(area : Area2D):
	if one_hit_per_area:
		var id := area.get_instance_id()
		if ignore_collision.has(id):
			return
		ignore_collision[id] = true
	collision_detected.emit()
