from pathlib import Path

from canon import load_bible, save_bible
from model import Bible, BibleMeta, Book, Chapter, Verse


def _bible() -> Bible:
    return Bible(
        meta=BibleMeta(
            code="KJA", name="King James Atualizada", year=1999,
            publisher="Abba Press", license="copyright", scope="full",
            source="openlp_sqlite",
        ),
        books=[
            Book(id=1, code="GEN", name="Gênesis", abbrev="Gn",
                 chapters=[Chapter(number=1, verses=[Verse(number=1, text="No princípio...")])]),
            Book(id=20, code="PRO", name="Provérbios", abbrev="Pv",
                 chapters=[Chapter(number=10, verses=[Verse(number=4, text="As mãos...")])]),
        ],
    )


def test_save_writes_sliced_files(tmp_path: Path):
    save_bible(_bible(), tmp_path)
    assert (tmp_path / "KJA" / "meta.json").exists()
    assert (tmp_path / "KJA" / "GEN.json").exists()
    assert (tmp_path / "KJA" / "PRO.json").exists()


def test_round_trip(tmp_path: Path):
    original = _bible()
    save_bible(original, tmp_path)
    loaded = load_bible("KJA", tmp_path)
    assert loaded == original
