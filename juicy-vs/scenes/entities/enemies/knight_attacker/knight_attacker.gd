class_name KnightAttacker extends BaseEnemy

## Caballero ATACANTE: rápido y con mucha vida. Va directo al jugador.
##
## No necesita lógica propia: la persecución la da el SeekBehavior heredado de
## base_enemy (seek "player"). La velocidad y la vida se configuran en su escena
## (knight_attacker.tscn) / GameConfig. Existe como clase para tener identidad de
## tipo y un config_key propio.
##
## Pertenece al grupo "enemy" (heredado), así que muere con la cadena de muerte del
## Rey (9999 de daño a todo "enemy").
