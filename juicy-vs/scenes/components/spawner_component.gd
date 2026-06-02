class_name SpawnerComponent extends Node

## Genera instancias de una escena de forma periodica alrededor del nodo padre.
##
## Pensado para poblaciones (enemigos, pickups, props): le importa el AREA donde
## aparecen y cuantas pueden estar vivas a la vez, no la direccion/velocidad (para
## proyectiles dirigidos usa un ShootComponent).

signal spawned(entity: Node)

## Donde se ubican las entidades respecto a spawn_circle.
## EDGE = en el borde (anillo); INSIDE = dentro del area; CENTER = en el centro.
enum SpawnArea { EDGE, INSIDE, CENTER }

@export var scene: PackedScene
## Define el area de aparicion. Su 'center' actua como offset local respecto al
## padre y su 'radius' como alcance. Si queda null se usa un circulo unitario.
@export var spawn_circle: Circle
## Generaciones por segundo.
@export var speed: float = 1.0
## Entidades creadas en cada generacion.
@export var spawn_count: int = 1
## Maximo de entidades vivas simultaneamente de este spawner. <= 0 = sin limite.
@export var max_entities: int = 0
@export var spawn_area: SpawnArea = SpawnArea.EDGE
## Si arranca a generar automaticamente en _ready.
@export var auto_start: bool = true
## Padre de las entidades generadas. Si queda null se usa LevelManager.y_sort_entities.
@export var spawn_container: Node = null

var active_count: int = 0

var _origin: Node2D
var _timer: Timer

func _ready():
	assert(get_parent() is Node2D, "SpawnerComponent parent must be a Node2D")
	_origin = get_parent()
	if spawn_circle == null:
		spawn_circle = Circle.new()
	_timer = Timer.new()
	_timer.timeout.connect(_on_timer_timeout)
	add_child(_timer)
	if auto_start:
		start()

func start():
	if speed <= 0:
		push_warning("SpawnerComponent: 'speed' debe ser > 0 para autogenerar.")
		return
	_timer.wait_time = 1.0 / speed
	_timer.start()

func stop():
	_timer.stop()

func is_full() -> bool:
	return max_entities > 0 and active_count >= max_entities

## Crea una entidad inmediatamente (respetando max_entities). Devuelve la entidad
## o null si no se pudo generar.
func spawn() -> Node:
	if scene == null or not scene.can_instantiate() or is_full():
		return null
	var container := _get_container()
	if container == null:
		return null
	var entity := scene.instantiate()
	container.add_child(entity)
	if entity is Node2D:
		entity.global_position = _spawn_position()
	active_count += 1
	entity.tree_exited.connect(_on_entity_freed, CONNECT_ONE_SHOT)
	spawned.emit(entity)
	return entity

func _on_timer_timeout():
	for i in spawn_count:
		if is_full():
			break
		spawn()

func _on_entity_freed():
	active_count = maxi(0, active_count - 1)

func _get_container() -> Node:
	if spawn_container:
		return spawn_container
	if LevelManager.y_sort_entities:
		return LevelManager.y_sort_entities
	return _origin.get_parent()

func _spawn_position() -> Vector2:
	var offset: Vector2
	match spawn_area:
		SpawnArea.CENTER:
			offset = spawn_circle.center
		SpawnArea.INSIDE:
			offset = spawn_circle.random_point_inside()
		_:
			offset = spawn_circle.random_point_in_edge()
	return _origin.global_position + offset
