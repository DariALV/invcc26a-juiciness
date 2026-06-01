extends Node

const EFFECTS := {
	"hit_effect": preload("res://effects/hit_effect.tscn")
}

const PREWARM := 8

var _free: Dictionary = {}

func _ready() -> void:
	for name in EFFECTS:
		_free[name] = []
		for i in PREWARM:
			_free[name].append(_instantiate(name))

func spawn(effect_name: String, pos: Vector2, direction: Vector2 = Vector2(0, 0)) -> void:
	if not EFFECTS.has(effect_name):
		push_warning("ParticleManager: efecto desconocido '%s'" % effect_name)
		return
	var p := _acquire(effect_name)
	p.global_position = pos
	if direction != Vector2.ZERO:
		p.direction = direction
	p.restart()

func _acquire(effect_name: String) -> CPUParticles2D:
	var pool: Array = _free[effect_name]
	if pool.size() > 0:
		return pool.pop_back()
	return _instantiate(effect_name)

func _instantiate(effect_name: String) -> CPUParticles2D:
	var p: CPUParticles2D = EFFECTS[effect_name].instantiate()
	p.one_shot = true
	p.emitting = false
	add_child(p)
	p.finished.connect(func(): _free[effect_name].append(p))
	return p
