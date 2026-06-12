import sqlite3
from itertools import groupby
from pathlib import Path

import books
import catalog
from model import Bible, BibleMeta, Book, Chapter, Verse


class OpenLpSqliteSource:
    """Reads an OpenLP-format `.sqlite` (verse table) into the canonical model."""

    def __init__(self, db_dir: Path):
        self._db_dir = db_dir

    def fetch(self, version: str) -> Bible:
        entry = catalog.get(version)
        meta = BibleMeta(
            code=entry.code, name=entry.name, year=entry.year,
            publisher=entry.publisher, license=entry.license, scope=entry.scope,
            source="openlp_sqlite",
        )
        con = sqlite3.connect(self._db_dir / f"{version}.sqlite")
        try:
            rows = con.execute(
                "SELECT book_id, chapter, verse, text FROM verse "
                "ORDER BY book_id, chapter, verse"
            ).fetchall()
        finally:
            con.close()

        built: list[Book] = []
        for book_id, brows in groupby(rows, key=lambda r: r[0]):
            ref = books.by_id(book_id)
            chapters = [
                Chapter(number=ch, verses=[Verse(number=r[2], text=r[3]) for r in crows])
                for ch, crows in groupby(brows, key=lambda r: r[1])
            ]
            built.append(Book(id=ref.id, code=ref.code, name=ref.name,
                              abbrev=ref.abbrev, chapters=chapters))
        return Bible(meta=meta, books=built)
