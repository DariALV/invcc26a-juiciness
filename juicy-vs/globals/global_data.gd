extends Node

var enemies_alive = 0
var enemies_dead = 0

## Apuntado automatico al enemigo mas cercano (lo alterna el boton de la esquina
## inferior derecha). Se registra en cada snapshot para comparar rendimiento del
## jugador con auto-aim vs apuntado manual.
var auto_aim: bool = false

## Multiplicador de dano de la oleada actual. Lo fija el EnemySpawner al iniciar cada
## oleada y lo leen los enemigos (dano cuerpo a cuerpo) y sus proyectiles al nacer.
var wave_damage_multiplier: float = 1.0

## Multiplicador GLOBAL de vida de la oleada actual. Lo fija el EnemySpawner al
## iniciar cada oleada y se aplica a la vida de TODOS los enemigos al spawnear
## (reemplaza al multiplicador de vida por-entidad de cada WaveEnemy).
var wave_health_multiplier: float = 1.0
