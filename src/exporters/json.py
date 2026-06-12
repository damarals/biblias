import json
from pathlib import Path

from model import Bible


class JsonExporter:
    """Writes the legacy public JSON: one object per book with nested chapter/verse text."""

    def export(self, bible: Bible, path: Path) -> None:
        payload = [
            {
                "abbrev": book.abbrev,
                "chapters": [[v.text for v in ch.verses] for ch in book.chapters],
                "name": book.name,
            }
            for book in bible.books
        ]
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(payload, ensure_ascii=False), encoding="utf-8")
