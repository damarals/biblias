import sqlite3
from pathlib import Path

from exporters.sqlite import OpenLpSqliteExporter
from model import Bible, BibleMeta, Book, Chapter, Verse


def _bible() -> Bible:
    return Bible(
        meta=BibleMeta(code="KJA", name="King James Atualizada", year=1999,
                       publisher="Abba Press", license="copyright", scope="full", source="t"),
        books=[
            Book(id=1, code="GEN", name="Gênesis", abbrev="Gn",
                 chapters=[Chapter(number=1, verses=[Verse(number=1, text="No princípio...")])]),
            Book(id=43, code="JHN", name="João", abbrev="Jo",
                 chapters=[Chapter(number=3, verses=[Verse(number=16, text="Porque Deus amou...")])]),
        ],
    )


def test_export_creates_openlp_schema(tmp_path: Path):
    out = tmp_path / "KJA.sqlite"
    OpenLpSqliteExporter().export(_bible(), out)
    con = sqlite3.connect(out)
    tables = {r[0] for r in con.execute("SELECT name FROM sqlite_master WHERE type='table'")}
    assert {"metadata", "book", "verse"} <= tables
    assert con.execute("SELECT value FROM metadata WHERE key='name'").fetchone()[0] == "King James Atualizada"
    con.close()


def test_export_writes_books_and_verses(tmp_path: Path):
    out = tmp_path / "KJA.sqlite"
    OpenLpSqliteExporter().export(_bible(), out)
    con = sqlite3.connect(out)
    rows = dict(con.execute("SELECT name, testament_reference_id FROM book").fetchall())
    assert rows == {"Gênesis": 1, "João": 2}
    text = con.execute(
        "SELECT v.text FROM verse v JOIN book b ON v.book_id=b.id "
        "WHERE b.name='João' AND v.chapter=3 AND v.verse=16"
    ).fetchone()[0]
    assert text == "Porque Deus amou..."
    con.close()
