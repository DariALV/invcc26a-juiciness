extends Node

var enemies_alive = 0
var enemies_dead = 0

func get_player() -> Player:
	return get_tree().get_first_node_in_group("player") as Player
