extends Node

## Referencias globales a nodos especiales del nivel actual.
##
## El nivel las registra en su _ready() mediante register_level(); desde
## cualquier nodo se puede acceder a ellas (p. ej. para instanciar proyectiles
## bajo YSortEntities en vez de bajo la entidad que dispara, que se mueve).

var y_sort_entities: Node2D
var player: Player

func register_level(p_y_sort_entities: Node2D, p_player: Player) -> void:
	y_sort_entities = p_y_sort_entities
	player = p_player
