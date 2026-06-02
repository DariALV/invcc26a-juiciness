class_name Arrow extends BaseProjectile

func _process(_delta):
	if velocity != Vector2.ZERO:
		rotation = velocity.angle()

func projectile_name() -> String:
	return "arrow"
