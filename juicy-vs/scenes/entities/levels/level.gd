extends Node2D

@onready var timer:Timer = $Timer
@onready var label = $CanvasLayer/Label
@onready var id = $CanvasLayer/Stats/VBoxContainer/ID
@onready var survival_time = $CanvasLayer/Stats/VBoxContainer/SurvivalTime
@onready var hits_taken = $CanvasLayer/Stats/VBoxContainer/HitsTaken
@onready var enemies_defeated = $CanvasLayer/Stats/VBoxContainer/EnemiesDefeated
@onready var player: Player = $YSortEntities/Player
@onready var stats = $CanvasLayer/Stats
@onready var y_sort_entities = $YSortEntities

func _ready():
	timer.timeout.connect(on_timeout)
	player.health.died.connect(on_player_died)
	UpgradeManager.spawn_node = y_sort_entities

func _process(delta):
	label.text = str(int(round(timer.time_left)))
	
func on_timeout():
	finish_level(timer.wait_time)
	Database.add_game_data({"student_id": Database.selected_id, "survival_time": timer.wait_time, "hits_taken": Database.hits_taken, "enemies_defeated": Database.enemies_defeated})
	

func on_player_died():
	finish_level(timer.wait_time - timer.time_left)
	Database.add_game_data({"student_id": Database.selected_id, "survival_time": timer.wait_time, "hits_taken": Database.hits_taken, "enemies_defeated": Database.enemies_defeated})

func finish_level(surv_time: int):
	id.text = "ID: " + Database.selected_id
	survival_time.text = "Survival Time: " + str(surv_time)
	hits_taken.text = "Hits Taken: " + str(Database.hits_taken)
	enemies_defeated.text = "Enemies Defeated: " + str(Database.enemies_defeated)
	stats.visible = true
	Engine.time_scale = 0
