"""Generate synthetic research data for the camera-juiciness study.

The generated CSVs are only for pipeline testing: dashboard checks, joins,
ANOVA prototypes, correlations, and import experiments. They are not empirical
results and must not be reported as study findings.
"""

from __future__ import annotations

import csv
import itertools
import math
import random
import statistics
import uuid
from datetime import datetime, timedelta, timezone
from pathlib import Path


SEED = 20260617
RUNS_PER_CONDITION = 15
BUILD_VERSION = "synthetic-1.0.0"

ROOT = Path(__file__).resolve().parents[1]
OUT_DIR = ROOT / "synthetic_data"
SUBJECTIVE_SOURCE = ROOT / "overthrown_datos_subjetivos.csv"

FEEDBACK_LIKERT = [
    "difficulty",
    "fun",
    "chaos",
    "monotony",
    "boredom",
    "stress",
    "style_liking",
]

UPGRADES = [
    "Damage",
    "Pierce",
    "Fire Rate",
    "Arrow Speed",
    "Move Speed",
    "Regen",
    "Heal on Kill",
    "Dodge",
    "Reflection",
    "Death Arrows",
    "Area Damage",
    "Area Rate",
    "Area Targets",
    "Luck",
    "XP Gain",
]

ENEMIES = ["zombie", "slime", "archer", "knight_attacker", "knight_defender", "necromancer", "king"]
DAMAGE_TYPES = ["melee", "projectile", "magic", "contact"]
DAMAGE_SOURCES = ["arrow", "burn", "aura", "death_arrows"]


def clamp(value: float, low: float, high: float) -> float:
    return max(low, min(high, value))


def likert(value: float, low: int = 1, high: int = 7) -> int:
    return int(round(clamp(value, low, high)))


def bool_text(value: bool) -> str:
    return "true" if value else "false"


def iso(dt: datetime) -> str:
    return dt.astimezone(timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")


def read_overthrown_source() -> tuple[list[str], list[list[int]]]:
    if not SUBJECTIVE_SOURCE.exists():
        return [], []
    with SUBJECTIVE_SOURCE.open(encoding="utf-8-sig", newline="") as f:
        reader = csv.reader(f)
        rows = list(reader)
    if not rows:
        return [], []
    headers = rows[0]
    data: list[list[int]] = []
    for row in rows[1:]:
        try:
            data.append([int(cell) for cell in row])
        except ValueError:
            continue
    return headers, data


def write_csv(path: Path, fieldnames: list[str], rows: list[dict]) -> None:
    with path.open("w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames, extrasaction="ignore")
        writer.writeheader()
        writer.writerows(rows)


def condition_name(shake: bool, zoom: bool, recoil: bool) -> str:
    active = []
    if shake:
        active.append("shake")
    if zoom:
        active.append("zoom")
    if recoil:
        active.append("recoil")
    return "baseline" if not active else "+".join(active)


def adjusted_subjective_row(
    rng: random.Random,
    source_rows: list[list[int]],
    shake: bool,
    zoom: bool,
    recoil: bool,
) -> list[int]:
    """Sample a real subjective row and nudge it by synthetic condition effects."""
    if source_rows:
        values = list(rng.choice(source_rows))
    else:
        values = [likert(rng.gauss(3.4, 0.9), 1, 5) for _ in range(24)]

    # Appreciation / playability items from the Overthrown block.
    appreciation = [0, 1, 2, 6, 7, 8]
    ease = [3, 4, 5]
    absorption = [9, 12, 13, 14, 20, 21, 22]
    embodiment = [16, 17, 18, 19, 23]

    active_count = int(shake) + int(zoom) + int(recoil)
    for idx in appreciation:
        values[idx] += rng.choice([0, 0, 1]) if active_count else rng.choice([-1, 0, 0])
    for idx in absorption:
        values[idx] += int(zoom) + rng.choice([-1, 0, 0, 1])
    for idx in embodiment:
        values[idx] += int(recoil) + rng.choice([-1, 0, 0, 1])
    for idx in ease:
        values[idx] -= int(shake and recoil)

    if active_count == 3:
        values[4] -= 1
        values[10] += 1

    return [likert(v, 1, 5) for v in values]


def mean_indexes(values: list[int], indexes: list[int]) -> float:
    return statistics.mean(values[i] for i in indexes)


def build_research_rows(rng: random.Random) -> tuple[list[dict], list[dict], list[dict]]:
    overthrown_headers, overthrown_source_rows = read_overthrown_source()
    player_configs: list[dict] = []
    research_rows: list[dict] = []
    subjective_rows: list[dict] = []

    base_start = datetime(2026, 6, 20, 14, 0, tzinfo=timezone.utc)
    participant_no = 1

    for condition_no, (shake, zoom, recoil) in enumerate(itertools.product([False, True], repeat=3), start=1):
        for repeat in range(RUNS_PER_CONDITION):
            participant_id = f"SYN{participant_no:03d}"
            run_id = str(uuid.uuid5(uuid.NAMESPACE_DNS, f"invcc26a-{participant_id}-{SEED}"))
            cond = condition_name(shake, zoom, recoil)
            active_count = int(shake) + int(zoom) + int(recoil)
            skill = rng.gauss(0.0, 1.0)
            experience = (
                "Experto" if skill > 1.2 else
                "Avanzado" if skill > 0.35 else
                "Intermedio" if skill > -0.5 else
                "Basico"
            )

            subjective = adjusted_subjective_row(rng, overthrown_source_rows, shake, zoom, recoil)
            appreciation = mean_indexes(subjective, list(range(0, 9)))
            absorption = mean_indexes(subjective, list(range(9, 16)))
            presence = mean_indexes(subjective, list(range(16, 24)))
            total_subjective = statistics.mean(subjective)

            immersion = clamp(
                3.7
                + 0.45 * zoom
                + 0.28 * shake
                + 0.33 * recoil
                - 0.20 * (active_count == 3)
                + 0.30 * (absorption - 3.0)
                + rng.gauss(0, 0.55),
                1,
                7,
            )
            task_accuracy = clamp(
                0.82
                + 0.04 * skill
                - 0.045 * shake
                - 0.025 * zoom
                - 0.040 * recoil
                - 0.012 * max(active_count - 1, 0)
                + rng.gauss(0, 0.045),
                0.45,
                0.98,
            )
            reaction_time_ms = int(round(clamp(
                620
                - 38 * skill
                + 48 * shake
                + 26 * zoom
                + 42 * recoil
                + 18 * max(active_count - 1, 0)
                + rng.gauss(0, 70),
                330,
                1100,
            )))
            stress = clamp(2.5 + 0.45 * shake + 0.30 * recoil + 0.16 * active_count - 0.18 * skill + rng.gauss(0, 0.65), 1, 7)
            difficulty = clamp(3.5 + 0.35 * shake + 0.25 * recoil - 0.14 * skill + rng.gauss(0, 0.65), 1, 7)
            chaos = clamp(3.1 + 0.55 * shake + 0.27 * recoil + 0.15 * zoom + rng.gauss(0, 0.70), 1, 7)
            fun = clamp(4.6 + 0.24 * active_count + 0.18 * (appreciation - 3.5) - 0.12 * stress + rng.gauss(0, 0.55), 1, 7)
            style_liking = clamp(4.9 + 0.24 * zoom + 0.18 * shake + 0.14 * recoil + 0.25 * (appreciation - 3.5) + rng.gauss(0, 0.50), 1, 7)
            monotony = clamp(3.2 - 0.20 * active_count - 0.12 * fun + rng.gauss(0, 0.55), 1, 7)
            boredom = clamp(2.9 - 0.28 * active_count - 0.18 * fun + rng.gauss(0, 0.55), 1, 7)

            shots = max(80, int(round(rng.gauss(1.65, 0.25) * 420)))
            aim_hits = int(round(shots * task_accuracy))
            duration_seconds = int(round(clamp(
                390
                + 120 * task_accuracy
                + 18 * skill
                - 9 * stress
                + rng.gauss(0, 55),
                180,
                610,
            )))

            player_configs.append({
                "id": participant_id,
                "camera_shake": bool_text(shake),
                "camera_zoom": bool_text(zoom),
                "camera_recoil": bool_text(recoil),
                "condition": cond,
                "condition_index": condition_no,
                "synthetic": "true",
            })

            row = {
                "synthetic": "true",
                "participant_id": participant_id,
                "run_id": run_id,
                "condition": cond,
                "condition_index": condition_no,
                "camera_shake": bool_text(shake),
                "camera_zoom": bool_text(zoom),
                "camera_recoil": bool_text(recoil),
                "gamer_skill_z": round(skill, 3),
                "game_experience": experience,
                "started_at": iso(base_start + timedelta(minutes=participant_no * 4)),
                "overthrown_appreciation_1_5": round(appreciation, 3),
                "overthrown_absorption_1_5": round(absorption, 3),
                "overthrown_presence_1_5": round(presence, 3),
                "overthrown_total_1_5": round(total_subjective, 3),
                "immersion_score_1_7": round(immersion, 3),
                "enjoyment_score_1_7": round(fun, 3),
                "style_liking_1_7": round(style_liking, 3),
                "stress_score_1_7": round(stress, 3),
                "difficulty_score_1_7": round(difficulty, 3),
                "chaos_score_1_7": round(chaos, 3),
                "task_accuracy": round(task_accuracy, 4),
                "reaction_time_ms": reaction_time_ms,
                "aim_shots": shots,
                "aim_hits": aim_hits,
                "duration_seconds": duration_seconds,
            }
            research_rows.append(row)

            if overthrown_headers:
                subjective_rows.append({
                    "synthetic": "true",
                    "participant_id": participant_id,
                    "run_id": run_id,
                    **{header: subjective[i] for i, header in enumerate(overthrown_headers)},
                })

            participant_no += 1

    return player_configs, research_rows, subjective_rows


def build_supabase_like_tables(
    rng: random.Random,
    research_rows: list[dict],
) -> dict[str, list[dict]]:
    runs: list[dict] = []
    feedback: list[dict] = []
    snapshots: list[dict] = []
    damage_taken: list[dict] = []
    upgrade_choices: list[dict] = []
    run_build: list[dict] = []
    upgrade_stats: list[dict] = []

    for row in research_rows:
        run_id = row["run_id"]
        participant_id = row["participant_id"]
        duration = int(row["duration_seconds"])
        started = datetime.fromisoformat(row["started_at"].replace("Z", "+00:00"))
        ended = started + timedelta(seconds=duration)
        shake = row["camera_shake"] == "true"
        zoom = row["camera_zoom"] == "true"
        recoil = row["camera_recoil"] == "true"
        active_count = int(shake) + int(zoom) + int(recoil)
        accuracy = float(row["task_accuracy"])
        reaction_ms = int(row["reaction_time_ms"])
        skill = float(row["gamer_skill_z"])

        max_hp = 100
        hp = float(max_hp)
        kills_so_far = 0
        xp = 0
        total_damage_taken = 0.0
        total_damage_dealt = 0.0
        x = rng.gauss(0, 40)
        y = rng.gauss(0, 40)
        base_damage = 9.0 + 2.0 * skill + rng.random() * 2.0
        fire_rate = 1.0 + 0.12 * skill
        move_speed = 145 + 8 * skill
        pierce = 1

        last_total_taken = 0.0
        last_total_dealt = 0.0
        snapshot_times = list(range(0, duration + 1, 5))
        if snapshot_times[-1] != duration:
            snapshot_times.append(duration)

        for t in snapshot_times:
            progress = t / max(duration, 1)
            round_no = min(5, int(t // 120) + 1)
            pressure = clamp(0.35 + progress + 0.09 * active_count - 0.05 * skill, 0.1, 1.7)
            enemies_alive = max(0, int(round(rng.gauss(5 + round_no * 3 + pressure * 5, 3))))
            nearest_enemy_dist = clamp(rng.gauss(180 - pressure * 52, 35), 25, 320)
            dmg_event = rng.random() < clamp(0.13 + 0.05 * pressure + 0.03 * recoil, 0.02, 0.45)
            dmg_taken = clamp(rng.gauss(5.0 + pressure * 2.5, 2.0), 0, 18) if dmg_event else 0.0
            hp = clamp(hp - dmg_taken + 0.45 + 0.10 * skill, 0, max_hp)
            total_damage_taken += dmg_taken

            kills_delta = max(0, int(round(rng.gauss(2.2 + 2.5 * accuracy + progress * 1.2, 1.5))))
            kills_so_far += kills_delta
            xp += kills_delta * 9 + int(progress * 2)
            dealt_delta = kills_delta * clamp(rng.gauss(13 + base_damage, 5), 4, 42)
            total_damage_dealt += dealt_delta

            angle = rng.uniform(-math.pi, math.pi)
            speed = clamp(rng.gauss(move_speed * (0.55 + 0.15 * pressure), 35), 0, 260)
            distance = speed * 5 / 60
            x += math.cos(angle) * distance + rng.gauss(0, 2.5)
            y += math.sin(angle) * distance + rng.gauss(0, 2.5)

            damage = base_damage + int(progress * 6) + active_count * 0.3
            if t > duration * 0.35:
                fire_rate += 0.002
            if t > duration * 0.55:
                pierce = 2

            snapshots.append({
                "run_id": run_id,
                "game_time_seconds": t,
                "game_time_ms": t * 1000,
                "player_x": round(x, 3),
                "player_y": round(y, 3),
                "hp": round(hp, 3),
                "max_hp": max_hp,
                "enemies_alive": enemies_alive,
                "nearest_enemy_dist": round(nearest_enemy_dist, 3),
                "dmg_taken_delta": round(total_damage_taken - last_total_taken, 3),
                "dmg_dealt_delta": round(total_damage_dealt - last_total_dealt, 3),
                "level": max(1, xp // 100 + 1),
                "xp": xp,
                "inputs_delta": max(0, int(round(rng.gauss(5 + pressure * 2, 2)))),
                "dir_changes_delta": max(0, int(round(rng.gauss(1.8 + pressure + 0.5 * shake, 1.2)))),
                "distance_moved": round(distance, 3),
                "aim_shots": int(round((t / max(duration, 1)) * int(row["aim_shots"]))),
                "aim_hits": int(round((t / max(duration, 1)) * int(row["aim_hits"]))),
                "fps": round(clamp(rng.gauss(59 - enemies_alive * 0.06, 2.2), 42, 61), 2),
                "speed": round(speed, 3),
                "aim_angle": round(angle, 4),
                "projectiles_alive": max(0, int(round(rng.gauss(8 + fire_rate * 4, 3)))),
                "xp_orbs_alive": max(0, int(round(rng.gauss(12 + kills_delta * 2, 5)))),
                "round": round_no,
                "kills_so_far": kills_so_far,
                "damage": round(damage, 3),
                "pierce": pierce,
                "regen": round(0.3 + max(skill, -1.0) * 0.08, 3),
                "move_speed": round(move_speed, 3),
                "arrows": 1 + int(progress > 0.45),
                "fire_rate": round(fire_rate, 3),
                "arrow_speed": round(320 + 14 * skill + progress * 25, 3),
                "heal_on_kill": round(max(0, 0.04 + progress * 0.06), 3),
                "dodge_chance": round(max(0, 0.03 + progress * 0.07), 3),
                "reflect_chance": round(max(0, progress * 0.06), 3),
                "death_arrow_chance": round(max(0, progress * 0.08), 3),
                "area_damage": round(max(0, progress * 5.5), 3),
                "area_attack_rate": round(max(0, 0.4 + progress * 0.5), 3),
                "area_max_targets": 1 + int(progress > 0.5),
                "luck": round(1.0 + max(skill, -1) * 0.05, 3),
                "xp_multiplier": round(1.0 + progress * 0.18, 3),
                "auto_aim": "true",
            })
            last_total_taken = total_damage_taken
            last_total_dealt = total_damage_dealt

        final_round = min(5, int(duration // 120) + 1)
        final_level = max(1, xp // 100 + 1)
        won = duration >= 570 and hp > 0
        end_reason = "win" if won else "death"
        death_enemy = "" if won else rng.choice(ENEMIES)
        dodges = int(round(clamp(rng.gauss(1.5 + skill + active_count * 0.3, 1.4), 0, 8)))
        heals = int(round(clamp(rng.gauss(1.0 + total_damage_taken / 80, 1.0), 0, 7)))

        runs.append({
            "id": run_id,
            "player_id": participant_id,
            "build_version": BUILD_VERSION,
            "started_at": iso(started),
            "ended_at": iso(ended),
            "duration_seconds": duration,
            "final_round": final_round,
            "final_level": final_level,
            "death_enemy": death_enemy,
            "total_kills": kills_so_far,
            "total_damage_taken": round(total_damage_taken, 3),
            "total_xp": xp,
            "end_reason": end_reason,
            "dodges_used": dodges,
            "heals_used": heals,
        })

        feedback.append({
            "id": str(uuid.uuid5(uuid.NAMESPACE_DNS, f"feedback-{run_id}")),
            "run_id": run_id,
            "created_at": iso(ended + timedelta(seconds=45)),
            "difficulty": likert(float(row["difficulty_score_1_7"])),
            "fun": likert(float(row["enjoyment_score_1_7"])),
            "chaos": likert(float(row["chaos_score_1_7"])),
            "monotony": likert(3.0 - 0.12 * active_count + rng.gauss(0, 0.7)),
            "boredom": likert(2.8 - 0.18 * active_count + rng.gauss(0, 0.7)),
            "stress": likert(float(row["stress_score_1_7"])),
            "style_liking": likert(float(row["style_liking_1_7"])),
            "unnecessary_upgrades": bool_text(rng.random() < (0.17 + 0.04 * active_count)),
            "comments": "SYNTHETIC TEST ROW - not empirical data",
        })

        event_count = max(1, int(round(total_damage_taken / 9)))
        hp_after = max_hp
        for _ in range(event_count):
            t = rng.randint(8, max(9, duration))
            amount = clamp(rng.gauss(6.0 + active_count * 0.7, 2.5), 1, 18)
            hp_before = hp_after
            hp_after = clamp(hp_after - amount + rng.random() * 2, 0, max_hp)
            damage_taken.append({
                "run_id": run_id,
                "game_time_seconds": t,
                "round": min(5, int(t // 120) + 1),
                "enemy_type": rng.choice(ENEMIES),
                "damage_type": rng.choice(DAMAGE_TYPES),
                "damage_amount": round(amount, 3),
                "hp_before_hit": round(hp_before, 3),
                "hp_after_hit": round(hp_after, 3),
            })

        taken_upgrades: dict[str, int] = {}
        choice_times = list(range(35, max(36, duration), 55))
        for t in choice_times:
            options = rng.sample(UPGRADES, 3)
            weights = [1.0 + (0.5 if opt in {"Damage", "Fire Rate", "Move Speed", "Luck"} else 0.0) for opt in options]
            selected = rng.choices(options, weights=weights, k=1)[0]
            taken_upgrades[selected] = taken_upgrades.get(selected, 0) + 1
            upgrade_choices.append({
                "run_id": run_id,
                "game_time_seconds": t,
                "round": min(5, int(t // 120) + 1),
                "option_1": options[0],
                "option_2": options[1],
                "option_3": options[2],
                "selected_option": selected,
                "decision_ms": max(500, int(round(rng.gauss(reaction_ms * 2.1, 350)))),
                "rerolls": max(0, int(round(rng.gauss(0.25 + 0.15 * active_count, 0.6)))),
            })

        for upgrade, level in sorted(taken_upgrades.items()):
            run_build.append({
                "run_id": run_id,
                "upgrade_name": upgrade,
                "level": level,
            })

        source_weights = [0.66, 0.12, 0.14, 0.08]
        remaining_kills = kills_so_far
        for i, source in enumerate(DAMAGE_SOURCES):
            if i == len(DAMAGE_SOURCES) - 1:
                source_kills = max(0, remaining_kills)
            else:
                source_kills = max(0, int(round(kills_so_far * source_weights[i] + rng.gauss(0, 4))))
                remaining_kills -= source_kills
            upgrade_stats.append({
                "run_id": run_id,
                "damage_source": source,
                "total_damage": round(max(0, source_kills * rng.gauss(18, 4)), 3),
                "total_kills": source_kills,
            })

    return {
        "Run": runs,
        "RunFeedback": feedback,
        "GameSnapshot": snapshots,
        "DamageTaken": damage_taken,
        "UpgradeChoice": upgrade_choices,
        "RunBuild": run_build,
        "UpgradeStats": upgrade_stats,
    }


def write_readme() -> None:
    readme = """# Synthetic research data

These files are generated test fixtures for the InvCC26a camera-juiciness
research pipeline. They are not empirical observations and must not be reported
as scientific results.

Generated files:

- `research_analysis_dataset.csv`: one row per synthetic participant/run, ready
  for ANOVA, regression, and correlation smoke tests.
- `PlayerIDConfig.csv`: participant condition assignment for the 2x2x2 design.
- `Run.csv`, `GameSnapshot.csv`, `DamageTaken.csv`, `UpgradeChoice.csv`,
  `RunBuild.csv`, `UpgradeStats.csv`, `RunFeedback.csv`: Supabase-like table
  exports for local joins and dashboard/import experiments.
- `OverthrownSubjectiveSynthetic.csv`: synthetic 1-5 item responses using the
  local Overthrown subjective CSV as the distributional base.

Design notes:

- Seed: 20260617.
- 8 conditions, 15 participants per condition, 120 total runs.
- Conditions are the full factorial combinations of `camera_shake`,
  `camera_zoom`, and `camera_recoil`.
- Synthetic camera effects intentionally create plausible test signals:
  camera effects tend to increase immersion/style scores while slightly reducing
  motor performance through lower `task_accuracy` and higher
  `reaction_time_ms`.

Regenerate with:

```bash
python3 analysis/generate_synthetic_research_data.py
```
"""
    (OUT_DIR / "README.md").write_text(readme, encoding="utf-8")


def main() -> None:
    rng = random.Random(SEED)
    OUT_DIR.mkdir(exist_ok=True)

    player_configs, research_rows, subjective_rows = build_research_rows(rng)
    tables = build_supabase_like_tables(rng, research_rows)

    research_fields = list(research_rows[0].keys())
    write_csv(OUT_DIR / "research_analysis_dataset.csv", research_fields, research_rows)

    player_fields = list(player_configs[0].keys())
    write_csv(OUT_DIR / "PlayerIDConfig.csv", player_fields, player_configs)

    if subjective_rows:
        subjective_fields = list(subjective_rows[0].keys())
        write_csv(OUT_DIR / "OverthrownSubjectiveSynthetic.csv", subjective_fields, subjective_rows)

    table_fields = {
        "Run": [
            "id", "player_id", "build_version", "started_at", "ended_at",
            "duration_seconds", "final_round", "final_level", "death_enemy",
            "total_kills", "total_damage_taken", "total_xp", "end_reason",
            "dodges_used", "heals_used",
        ],
        "RunFeedback": [
            "id", "run_id", "created_at", *FEEDBACK_LIKERT,
            "unnecessary_upgrades", "comments",
        ],
        "GameSnapshot": [
            "run_id", "game_time_seconds", "game_time_ms", "player_x", "player_y",
            "hp", "max_hp", "enemies_alive", "nearest_enemy_dist",
            "dmg_taken_delta", "dmg_dealt_delta", "level", "xp",
            "inputs_delta", "dir_changes_delta", "distance_moved",
            "aim_shots", "aim_hits", "fps", "speed", "aim_angle",
            "projectiles_alive", "xp_orbs_alive", "round", "kills_so_far",
            "damage", "pierce", "regen", "move_speed", "arrows", "fire_rate",
            "arrow_speed", "heal_on_kill", "dodge_chance", "reflect_chance",
            "death_arrow_chance", "area_damage", "area_attack_rate",
            "area_max_targets", "luck", "xp_multiplier", "auto_aim",
        ],
        "DamageTaken": [
            "run_id", "game_time_seconds", "round", "enemy_type", "damage_type",
            "damage_amount", "hp_before_hit", "hp_after_hit",
        ],
        "UpgradeChoice": [
            "run_id", "game_time_seconds", "round", "option_1", "option_2",
            "option_3", "selected_option", "decision_ms", "rerolls",
        ],
        "RunBuild": ["run_id", "upgrade_name", "level"],
        "UpgradeStats": ["run_id", "damage_source", "total_damage", "total_kills"],
    }

    for table, rows in tables.items():
        write_csv(OUT_DIR / f"{table}.csv", table_fields[table], rows)

    write_readme()

    print(f"Generated synthetic data in {OUT_DIR}")
    print(f"participants/runs: {len(research_rows)}")
    for table, rows in tables.items():
        print(f"{table}: {len(rows)} rows")
    if subjective_rows:
        print(f"OverthrownSubjectiveSynthetic: {len(subjective_rows)} rows")


if __name__ == "__main__":
    main()
