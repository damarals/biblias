import json
from pathlib import Path


def corrected_refs(code: str, root: Path) -> set[tuple[str, int, int]]:
    """Refs (book_code, chapter, verse) hand-corrected in the canonical store."""
    path = root / f"{code}.json"
    if not path.exists():
        return set()
    data = json.loads(path.read_text(encoding="utf-8"))
    return {(d["book"], d["chapter"], d["verse"]) for d in data}
