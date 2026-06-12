import pytest
from pydantic import ValidationError

from model import Bible, BibleMeta, Book, Chapter, Verse


def _sample_bible() -> Bible:
    return Bible(
        meta=BibleMeta(
            code="KJA",
            name="King James Atualizada",
            year=1999,
            publisher="Abba Press",
            license="copyright",
            scope="full",
            source="openlp_sqlite",
        ),
        books=[
            Book(
                id=20,
                code="PRO",
                name="Provérbios",
                abbrev="Pv",
                chapters=[Chapter(number=10, verses=[Verse(number=4, text="...")])],
            )
        ],
    )


def test_build_and_access():
    b = _sample_bible()
    assert b.meta.code == "KJA"
    assert b.books[0].chapters[0].verses[0].number == 4


def test_round_trip_json():
    b = _sample_bible()
    again = Bible.model_validate_json(b.model_dump_json())
    assert again == b


def test_verse_number_must_be_positive():
    with pytest.raises(ValidationError):
        Verse(number=0, text="x")
