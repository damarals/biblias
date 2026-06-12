from model import Bible, BibleMeta, Book, Chapter, Verse
from validate import Tier, cross_version_findings


def _bible(code: str, text: str) -> Bible:
    return Bible(
        meta=BibleMeta(code=code, name=code, license="copyright", scope="full", source="t"),
        books=[Book(id=20, code="PRO", name="Provérbios", abbrev="Pv",
                    chapters=[Chapter(number=10, verses=[Verse(number=4, text=text)])])],
    )


def test_short_outlier_is_flagged_high():
    full = "As mãos preguiçosas empobrecem o ser humano, porém as mãos laboriosas lhe trazem riqueza."
    bibles = [_bible("A", full), _bible("B", full), _bible("KJA", "As mãos preguiçosas lhe")]
    by_code = cross_version_findings(bibles)
    assert by_code["A"] == []
    assert by_code["B"] == []
    assert len(by_code["KJA"]) == 1
    f = by_code["KJA"][0]
    assert f.tier == Tier.HIGH
    assert (f.book_code, f.chapter, f.verse) == ("PRO", 10, 4)


def test_no_flag_when_fewer_than_three_versions():
    bibles = [_bible("A", "x" * 100), _bible("B", "x")]
    assert cross_version_findings(bibles) == {"A": [], "B": []}
