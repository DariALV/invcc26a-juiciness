extends Node

var _counts := {}

func add_entity(type: String) -> void:
	_counts[type] = _counts.get(type, 0) + 1
	var id := "entities/" + type
	if not Performance.has_custom_monitor(id):
		Performance.add_custom_monitor(id, Callable(self, "_get_count").bind(type))

func remove_entity(type: String) -> void:
	if _counts.has(type):
		_counts[type] = maxi(0, _counts[type] - 1)

func _get_count(type: String) -> int:
	return _counts.get(type, 0)

## Conteo publico de entidades vivas de un tipo (lo lleva _enter_tree/_exit_tree, asi
## que es simetrico y fiable: cuenta TODOS los enemigos, de oleada e invocados).
func get_count(type: String) -> int:
	return _counts.get(type, 0)
