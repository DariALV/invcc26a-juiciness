class_name CollectorComponent extends Area2D

signal collision(area: Area2D)

func _ready():
	area_entered.connect(_on_area_entered)

func _on_area_entered(area : Area2D):
	collision.emit(area)
