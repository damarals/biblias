import sqlite3
from pathlib import Path

from sources.openlp_sqlite import OpenLpSqliteSource


def _make_db(path: Path) -> None:
    con = sqlite3.connect(path)
    con.execute("CREATE TABLE verse (id INTEGER, book_id INTEGER, chapter INTEGER, verse INTEGER, text TEXT)")
    con.executemany(
        "INSERT INTO verse (id, book_id, chapter, verse, text) VALUES (?,?,?,?,?)",
        [
            (1, 1, 1, 1, "No princípio criou Deus..."),
            (2, 1, 1, 2, "E a terra era sem forma..."),
            (3, 20, 10, 4, "As mãos preguiçosas..."),
        ],
    )
    con.commit()
    con.close()


def test_fetch_builds_canonical_bible(tmp_path: Path):
    _make_db(tmp_path / "KJA.sqlite")
    bible = OpenLpSqliteSource(tmp_path).fetch("KJA")

    assert bible.meta.code == "KJA"
    assert bible.meta.source == "openlp_sqlite"
    assert [b.code for b in bible.books] == ["GEN", "PRO"]
    gen = bible.books[0]
    assert gen.name == "Gênesis"
    assert gen.chapters[0].verses[1].text == "E a terra era sem forma..."
    assert bible.books[1].chapters[0].verses[0].number == 4
