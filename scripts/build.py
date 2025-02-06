"""Make a README.md from registry.json"""

import json
from pathlib import Path
from dataclasses import dataclass
from typing import Literal


@dataclass
class Puzzle:
    title: str
    solution_path: str
    contribution_link: str
    puzzle_link: str | None
    puzzle_type: Literal["classic", "coc"]


OK_EMOJI = "✅"


def read_registry(registry_path: Path) -> list[Puzzle]:
    puzzles = []
    with registry_path.open() as f:
        data = json.load(f)
    for entry in data:
        puzzle = Puzzle(
            entry.get("title"),
            entry.get("solutionPath"),
            entry.get("contributionLink"),
            entry.get("puzzleLink"),
            entry.get("type"),
        )
        puzzles.append(puzzle)
    return puzzles


def fmt_optional_link(link: str | None) -> str:
    optional_link = f"[{OK_EMOJI}]({link})" if link is not None else "❌"
    return f"<div align='center'>{optional_link}</div>"


def fmt_puzzles(puzzles: list[Puzzle]) -> str:
    puzzles.sort(key=lambda puzzle: puzzle.title)

    out = ""
    for puzzle in puzzles:
        row = [puzzle.title]
        # solution_path and contribution_link are never None
        # but we still use fmt_optional_link for consistency
        solution_path = fmt_optional_link(puzzle.solution_path)
        row.append(solution_path)
        contribution_link = fmt_optional_link(puzzle.contribution_link)
        row.append(contribution_link)
        if puzzle.puzzle_type == "classic":
            puzzle_link = fmt_optional_link(puzzle.puzzle_link)
            row.append(puzzle_link)
        out += f"{' | '.join(row)}\n"
    return out


def generate_readme(puzzles: list[Puzzle]) -> str:
    classic_puzzles = [puzzle for puzzle in puzzles if puzzle.puzzle_type == "classic"]
    coc_puzzles = [puzzle for puzzle in puzzles if puzzle.puzzle_type == "coc"]
    print(f"Found {len(classic_puzzles)} puzzles and {len(coc_puzzles)} clashes.")

    out = ""
    out += f"*Click on the {OK_EMOJI} to open the link.*\n\n"
    out += "## Puzzles\n\n"
    out += "| Title | Solution | Contribution | Puzzle |\n"
    out += "|-------|----------|--------------|--------|\n"
    out += fmt_puzzles(classic_puzzles)
    out += "\n"
    out += "## Clashes\n\n"
    out += "| Title | Solution | Contribution |\n"
    out += "|-------|----------|--------------|\n"
    out += fmt_puzzles(coc_puzzles)

    return out


def main():
    registry_path = Path("scripts/registry.json")
    opath = Path("README_tmp.md")

    puzzles = read_registry(registry_path)
    readme_content = generate_readme(puzzles)
    with opath.open("w") as f:
        f.write(readme_content)
    print(f"Successfully wrote file at {opath}")


if __name__ == "__main__":
    main()
