from pathlib import Path

import books
from model import Bible, BibleMeta, Book


def save_bible(bible: Bible, root: Path) -> None:
    out = root / bible.meta.code
    out.mkdir(parents=True, exist_ok=True)
    (out / "meta.json").write_text(bible.meta.model_dump_json(indent=2), encoding="utf-8")
    for book in bible.books:
        (out / f"{book.code}.json").write_text(book.model_dump_json(indent=2), encoding="utf-8")


def load_bible(code: str, root: Path) -> Bible:
    src = root / code
    meta = BibleMeta.model_validate_json((src / "meta.json").read_text(encoding="utf-8"))
    loaded: list[Book] = []
    for ref in books.BOOKS:
        path = src / f"{ref.code}.json"
        if path.exists():
            loaded.append(Book.model_validate_json(path.read_text(encoding="utf-8")))
    return Bible(meta=meta, books=loaded)
