import books
from versification import CHAPTERS, chapters_for


def test_total_chapters_is_1189():
    assert sum(CHAPTERS.values()) == 1189


def test_every_book_has_a_count():
    assert {b.code for b in books.BOOKS} == set(CHAPTERS)


def test_known_counts():
    assert chapters_for("GEN") == 50
    assert chapters_for("PSA") == 150
    assert chapters_for("OBA") == 1
    assert chapters_for("REV") == 22
