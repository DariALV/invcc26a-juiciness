extends Node2D

@onready var label = $CanvasLayer/Label
@onready var id = $CanvasLayer/Stats/VBoxContainer/ID
@onready var survival_time = $CanvasLayer/Stats/VBoxContainer/SurvivalTime
@onready var hits_taken = $CanvasLayer/Stats/VBoxContainer/HitsTaken
@onready var enemies_defeated = $CanvasLayer/Stats/VBoxContainer/EnemiesDefeated
@onready var player: Player = $YSortEntities/Player
@onready var stats = $CanvasLayer/Stats
@onready var y_sort_entities = $YSortEntities
@onready var enemy_spawner: EnemySpawner = $EnemySpawner

## Tiempo de supervivencia acumulado (segundos).
var elapsed_time: float = 0.0
## Se activa cuando el EnemySpawner termina la ultima oleada.
var waves_done: bool = false
## Evita finalizar la partida mas de una vez.
var finished: bool = false

func _ready():
	LevelManager.register_level(y_sort_entities, player)
	player.health.died.connect(on_player_died)
	enemy_spawner.waves_completed.connect(on_waves_completed)

func _process(delta):
	if finished:
		return
	elapsed_time += delta
	label.text = str(int(round(elapsed_time)))
	# Victoria: ultima oleada completada y sin enemigos vivos.
	if waves_done and GlobalData.enemies_alive <= 0:
		win_level()

func on_waves_completed():
	waves_done = true

func win_level():
	var surv_time := int(round(elapsed_time))
	finish_level(surv_time)
	Database.add_game_data({"student_id": Database.selected_id, "survival_time": surv_time, "hits_taken": Database.hits_taken, "enemies_defeated": Database.enemies_defeated})

func on_player_died():
	var surv_time := int(round(elapsed_time))
	finish_level(surv_time)
	Database.add_game_data({"student_id": Database.selected_id, "survival_time": surv_time, "hits_taken": Database.hits_taken, "enemies_defeated": Database.enemies_defeated})

func finish_level(surv_time: int):
	if finished:
		return
	finished = true
	id.text = "ID: " + Database.selected_id
	survival_time.text = "Survival Time: " + str(surv_time)
	hits_taken.text = "Hits Taken: " + str(Database.hits_taken)
	enemies_defeated.text = "Enemies Defeated: " + str(Database.enemies_defeated)
	stats.visible = true
	Engine.time_scale = 0
