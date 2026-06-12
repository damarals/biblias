import sqlite3
from pathlib import Path

from model import Bible

_SCHEMA = """
CREATE TABLE metadata (key VARCHAR(255) PRIMARY KEY, value VARCHAR(255));
CREATE TABLE book (
    id INTEGER PRIMARY KEY,
    book_reference_id INTEGER,
    testament_reference_id INTEGER,
    name VARCHAR(50)
);
CREATE TABLE verse (
    id INTEGER PRIMARY KEY,
    book_id INTEGER,
    chapter INTEGER,
    verse INTEGER,
    text TEXT,
    FOREIGN KEY (book_id) REFERENCES book (id)
);
"""


class SqliteExporter:
    """Writes an OpenLP-native bible SQLite (metadata/book/verse schema)."""

    def export(self, bible: Bible, path: Path) -> None:
        path.parent.mkdir(parents=True, exist_ok=True)
        if path.exists():
            path.unlink()
        con = sqlite3.connect(path)
        try:
            con.executescript(_SCHEMA)
            con.execute("INSERT INTO metadata (key, value) VALUES (?, ?)", ("name", bible.meta.name))
            con.execute("INSERT INTO metadata (key, value) VALUES (?, ?)", ("dbversion", "2"))
            verse_id = 0
            for book in bible.books:
                testament = 1 if book.id <= 39 else 2
                con.execute(
                    "INSERT INTO book (id, book_reference_id, testament_reference_id, name) "
                    "VALUES (?, ?, ?, ?)",
                    (book.id, book.id, testament, book.name),
                )
                for chapter in book.chapters:
                    for verse in chapter.verses:
                        verse_id += 1
                        con.execute(
                            "INSERT INTO verse (id, book_id, chapter, verse, text) "
                            "VALUES (?, ?, ?, ?, ?)",
                            (verse_id, book.id, chapter.number, verse.number, verse.text),
                        )
            con.commit()
        finally:
            con.close()
