extends Node

@onready var http_request: HTTPRequest = $HTTPRequest

## Version del build que se guarda en cada Run. Actualizala en cada release.
const BUILD_VERSION := "1.0.0"

var selected_id: String = "C20413"
var enemies_alive = 0
var hits_taken = 0
var enemies_defeated = 0

var supabase_api_key: String = ""
var supabase_url: String = "https://tajerjmvchsmddhmxwmj.supabase.co/rest/v1/"
var headers = [
				"Content-Type: application/json",
			  	"apikey: sb_publishable_-Ap2Oix5EPoCCafGTGL7eg_7jxcc7Zk",
				"Authorization: Bearer sb_publishable_-Ap2Oix5EPoCCafGTGL7eg_7jxcc7Zk",
				"Prefer: return=minimal"
				]

# ---------------------------------------------------------------------------
# Estado de la run actual (ciclo de vida: start_run -> ... -> end_run)
# ---------------------------------------------------------------------------

## UUID de la run en curso. Se genera en start_run y enlaza todas las tablas hijas.
var current_run_id: String = ""
var _run_player_id: String = ""
var _run_build_version: String = BUILD_VERSION
var _run_started_at: String = ""

## Contexto que actualiza el nivel cada frame, para etiquetar los eventos.
var run_game_time: int = 0
var run_round: int = 1

## Buffers de eventos (se vacian de golpe en end_run para no spamear la red).
var _damage_events: Array = []
var _upgrade_choice_events: Array = []

## Acumuladores de la run.
var _total_damage_taken: float = 0.0
## Ultimo enemigo que hizo dano al jugador (para Run.death_enemy si pierde).
var _last_damage_enemy: String = ""
## UpgradeStats por canal de dano: source -> {"damage": float, "kills": int}.
var _channel_stats: Dictionary = {}

## Vuelca el buffer de snapshots cuando alcanza este tamano (volcado periodico para
## no perder datos si la partida se cierra). A 100ms, 150 ~ cada 15 segundos.
const SNAPSHOT_FLUSH_AT := 150

## Buffer de snapshots periodicos (time-series del estado de la partida).
var _snapshot_events: Array = []
## Acumuladores para calcular los deltas de dano entre snapshots consecutivos.
var _last_snap_damage_taken: float = 0.0
var _last_snap_damage_dealt: float = 0.0
## Contadores de herramientas defensivas usadas en la run (los suben el jugador).
var run_dodges: int = 0
var run_heals: int = 0

func add_game_data(test: Dictionary):
	var data = JSON.stringify(test)
	var url = supabase_url + "game_data"
	http_request.request(url, headers, HTTPClient.Method.METHOD_POST, data)
	pass
#curl "http://localhost:3000/table_name" \
  #-X POST -H "Content-Type: application/json" \
  #-d '{ "col1": "value1", "col2": "value2" }'


# ---------------------------------------------------------------------------
# Ciclo de vida de la run (capa de alto nivel)
# ---------------------------------------------------------------------------

## Arranca una run: genera el UUID y limpia buffers/acumuladores. Llamar al inicio
## del nivel. El UUID queda disponible de inmediato para todos los eventos.
func start_run(player_id: String, build_version: String = BUILD_VERSION) -> void:
	current_run_id = _generate_uuid()
	_run_player_id = player_id
	_run_build_version = build_version
	_run_started_at = _now_iso()
	run_game_time = 0
	run_round = 1
	_damage_events.clear()
	_upgrade_choice_events.clear()
	_total_damage_taken = 0.0
	_last_damage_enemy = ""
	_channel_stats.clear()
	_snapshot_events.clear()
	_last_snap_damage_taken = 0.0
	_last_snap_damage_dealt = 0.0
	run_dodges = 0
	run_heals = 0

## Bufferiza una eleccion de mejora con el contexto actual (tiempo y ronda).
func log_upgrade_choice(option_1: String, option_2: String, option_3: String, selected_option: String, decision_ms: int = 0, rerolls: int = 0) -> void:
	if current_run_id == "":
		return
	_upgrade_choice_events.append({
		"run_id": current_run_id,
		"game_time_seconds": run_game_time,
		"round": run_round,
		"option_1": option_1,
		"option_2": option_2,
		"option_3": option_3,
		"selected_option": selected_option,
		"decision_ms": decision_ms,
		"rerolls": rerolls,
	})

## Bufferiza un golpe recibido por el jugador y acumula el dano total + el ultimo
## enemigo (para death_enemy). La llama el jugador en cada golpe efectivo.
func log_damage_taken(enemy_type: String, damage_type: String, damage_amount: float, hp_before_hit: float, hp_after_hit: float) -> void:
	if current_run_id == "":
		return
	_total_damage_taken += damage_amount
	_last_damage_enemy = enemy_type
	_damage_events.append({
		"run_id": current_run_id,
		"game_time_seconds": run_game_time,
		"round": run_round,
		"enemy_type": enemy_type,
		"damage_type": damage_type,
		"damage_amount": damage_amount,
		"hp_before_hit": hp_before_hit,
		"hp_after_hit": hp_after_hit,
	})

## Acumula dano/kills por canal de dano (arrow, crit, burn, aura, death_arrows) para
## UpgradeStats. La llama HealthComponent al dañar a un enemigo (source != "").
func record_channel_damage(source: String, amount: float, killed: bool) -> void:
	if current_run_id == "" or source == "" or amount <= 0.0:
		return
	var s: Dictionary = _channel_stats.get(source, {"damage": 0.0, "kills": 0})
	s["damage"] += amount
	if killed:
		s["kills"] += 1
	_channel_stats[source] = s

## Suma el dano infligido acumulado en todos los canales (para dmg_dealt_delta).
func _total_damage_dealt() -> float:
	var total := 0.0
	for source in _channel_stats:
		total += _channel_stats[source]["damage"]
	return total

## Bufferiza un snapshot del estado de la partida (lo llama el nivel cada 100ms).
## 'data' trae los campos crudos; aqui se completan run_id, tiempo y los deltas de
## dano (recibido/infligido) desde el snapshot previo. Cuando el buffer crece se vuelca solo.
func log_snapshot(data: Dictionary) -> void:
	if current_run_id == "":
		return
	var dmg_taken_delta := _total_damage_taken - _last_snap_damage_taken
	var dmg_dealt_total := _total_damage_dealt()
	var dmg_dealt_delta := dmg_dealt_total - _last_snap_damage_dealt
	_last_snap_damage_taken = _total_damage_taken
	_last_snap_damage_dealt = dmg_dealt_total
	data["run_id"] = current_run_id
	data["game_time_seconds"] = run_game_time
	data["dmg_taken_delta"] = dmg_taken_delta
	data["dmg_dealt_delta"] = dmg_dealt_delta
	_snapshot_events.append(data)
	if _snapshot_events.size() >= SNAPSHOT_FLUSH_AT:
		flush_snapshots()

## Vuelca el buffer de snapshots a Supabase en un lote y lo vacia. Seguro de llamar en
## cualquier momento: GameSnapshot ya no tiene FK a Run.
func flush_snapshots() -> void:
	if _snapshot_events.is_empty():
		return
	_insert("GameSnapshot", _snapshot_events)
	_snapshot_events = []

## Cierra la run: inserta Run (espera a que confirme, por la FK) y luego vacia los
## buffers (DamageTaken, UpgradeChoice, GameSnapshot) + RunBuild (de UpgradeManager) +
## UpgradeStats (por canal).
## 'end_reason' indica como termino la run ('win' | 'death' | 'quit'). Si llega vacio
## se deduce de 'player_won'.
func end_run(player_won: bool, duration_seconds: int, final_round: int, final_level: int, total_kills: int, total_xp: int, end_reason: String = "") -> void:
	if current_run_id == "":
		return
	var death_enemy := "" if player_won else _last_damage_enemy
	var reason := end_reason
	if reason == "":
		reason = "win" if player_won else "death"

	# 1) Run primero. Espera a que termine para no violar la FK de las hijas.
	var req := create_run(current_run_id, _run_player_id, _run_build_version, _run_started_at, \
		_now_iso(), int(GameTimer.current_run_time), final_round, final_level, death_enemy, total_kills, \
		_total_damage_taken, total_xp, reason, run_dodges, run_heals)
	GameTimer.last_runs_time += GameTimer.current_run_time
	GameTimer.current_run_time = 0
	if req:
		await req.request_completed

	# 2) Eventos bufferizados en lote (una peticion por tabla).
	if not _damage_events.is_empty():
		_insert("DamageTaken", _damage_events)
	if not _upgrade_choice_events.is_empty():
		_insert("UpgradeChoice", _upgrade_choice_events)
	flush_snapshots()

	# 3) Build final del jugador (mejora -> nivel == veces tomada).
	for upgrade in UpgradeManager.times_taken:
		add_run_build(current_run_id, upgrade.title, UpgradeManager.times_taken[upgrade])

	# 4) Estadisticas por canal de dano.
	for source in _channel_stats:
		var s: Dictionary = _channel_stats[source]
		add_upgrade_stats(current_run_id, source, s["damage"], s["kills"])

	current_run_id = ""


# ---------------------------------------------------------------------------
# Helpers internos
# ---------------------------------------------------------------------------

## Envia un POST a la tabla indicada con un HTTPRequest desechable (fire-and-forget),
## de modo que varias inserciones puedan dispararse en paralelo sin colisionar.
## 'body' puede ser un Dictionary (una fila) o un Array (insercion en lote).
## Devuelve el HTTPRequest (para poder await su request_completed) o null si fallo.
func _insert(table: String, body) -> HTTPRequest:
	var data := JSON.stringify(body)
	var url := supabase_url + table
	var request := HTTPRequest.new()
	request.timeout = 10.0
	add_child(request)
	# Libera el nodo cuando la peticion termina (o expira por timeout).
	request.request_completed.connect(
		func(_result, _code, _resp_headers, _resp_body): request.queue_free()
	)
	var err := request.request(url, headers, HTTPClient.Method.METHOD_POST, data)
	if err != OK:
		request.queue_free()
		return null
	return request

## Genera un UUID v4 para usarlo como llave primaria del lado del cliente.
func _generate_uuid() -> String:
	var b: Array = []
	for i in range(16):
		b.append(randi() % 256)
	b[6] = (b[6] & 0x0f) | 0x40  # version 4
	b[8] = (b[8] & 0x3f) | 0x80  # variant 10xx
	return "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x" % b

## Fecha/hora actual en UTC, formato ISO 8601 con sufijo Z (para timestamptz).
func _now_iso() -> String:
	return Time.get_datetime_string_from_system(true) + "Z"


# ---------------------------------------------------------------------------
# PlayerIDConfig  (tabla de solo lectura: asigna las condiciones de camara)
# ---------------------------------------------------------------------------

## Lee la configuracion de un participante. El resultado (Dictionary, vacio si no se
## encontro) se entrega por el callback 'on_completed'. El match es case-insensitive
## (ilike sin comodines = igualdad ignorando mayus/minus), asi "c20413" encuentra a
## "C20413". El id se codifica para la URL por seguridad.
func get_player_config(player_id: String, on_completed: Callable) -> void:
	var url = supabase_url + "PlayerIDConfig?id=ilike." + player_id.uri_encode() + "&select=*"
	var request := HTTPRequest.new()
	request.timeout = 10.0
	add_child(request)
	request.request_completed.connect(
		func(_result, code, _resp_headers, resp_body):
			var config: Dictionary = {}
			if code == 200:
				var parsed = JSON.parse_string(resp_body.get_string_from_utf8())
				if parsed is Array and parsed.size() > 0:
					config = parsed[0]
			on_completed.call(config)
			request.queue_free()
	)
	var err := request.request(url, headers, HTTPClient.Method.METHOD_GET)
	if err != OK:
		request.queue_free()
		on_completed.call({})


# ---------------------------------------------------------------------------
# Metodos por tabla (insercion de una fila; construyen el diccionario adentro)
# ---------------------------------------------------------------------------

## Inserta una run completada. El UUID se pasa como parametro (lo genera start_run).
## Devuelve el HTTPRequest para poder esperar su confirmacion antes de las hijas.
func create_run(
	run_id: String,
	player_id: String,
	build_version: String,
	started_at: String,
	ended_at: String,
	duration_seconds: int,
	final_round: int,
	final_level: int,
	death_enemy: String,
	total_kills: int,
	total_damage_taken: float,
	total_xp: int,
	end_reason: String = "",
	dodges_used: int = 0,
	heals_used: int = 0
) -> HTTPRequest:
	var body := {
		"id": run_id,
		"player_id": player_id,
		"build_version": build_version,
		"started_at": started_at,
		"ended_at": ended_at,
		"duration_seconds": duration_seconds,
		"final_round": final_round,
		"final_level": final_level,
		"death_enemy": death_enemy,
		"total_kills": total_kills,
		"total_damage_taken": total_damage_taken,
		"total_xp": total_xp,
		"end_reason": end_reason,
		"dodges_used": dodges_used,
		"heals_used": heals_used,
	}
	return _insert("Run", body)


func add_upgrade_choice(
	run_id: String,
	game_time_seconds: int,
	round_number: int,
	option_1: String,
	option_2: String,
	option_3: String,
	selected_option: String
) -> void:
	var body := {
		"run_id": run_id,
		"game_time_seconds": game_time_seconds,
		"round": round_number,
		"option_1": option_1,
		"option_2": option_2,
		"option_3": option_3,
		"selected_option": selected_option,
	}
	_insert("UpgradeChoice", body)


func add_damage_taken(
	run_id: String,
	game_time_seconds: int,
	round_number: int,
	enemy_type: String,
	damage_type: String,
	damage_amount: float,
	hp_before_hit: float,
	hp_after_hit: float
) -> void:
	var body := {
		"run_id": run_id,
		"game_time_seconds": game_time_seconds,
		"round": round_number,
		"enemy_type": enemy_type,
		"damage_type": damage_type,
		"damage_amount": damage_amount,
		"hp_before_hit": hp_before_hit,
		"hp_after_hit": hp_after_hit,
	}
	_insert("DamageTaken", body)


func add_run_build(
	run_id: String,
	upgrade_name: String,
	level: int
) -> void:
	var body := {
		"run_id": run_id,
		"upgrade_name": upgrade_name,
		"level": level,
	}
	_insert("RunBuild", body)


func add_upgrade_stats(
	run_id: String,
	damage_source: String,
	total_damage: float,
	total_kills: int
) -> void:
	var body := {
		"run_id": run_id,
		"damage_source": damage_source,
		"total_damage": total_damage,
		"total_kills": total_kills,
	}
	_insert("UpgradeStats", body)


# ---------------------------------------------------------------------------
# RunFeedback  (cuestionario post-run; menu temporal de pruebas)
# ---------------------------------------------------------------------------

## Inserta una respuesta del cuestionario post-run. 'fields' trae las calificaciones
## (difficulty, fun, chaos, ...) y aqui se completa con id y run_id. Fire-and-forget.
func submit_feedback(feedback_run_id: String, fields: Dictionary) -> void:
	var body: Dictionary = fields.duplicate()
	body["id"] = _generate_uuid()
	body["run_id"] = feedback_run_id
	_insert("RunFeedback", body)
