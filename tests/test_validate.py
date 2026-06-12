from model import Bible, BibleMeta, Book, Chapter, Verse
from validate import Tier, validate_bible


def _bible(verses: list[tuple[int, str]]) -> Bible:
    return Bible(
        meta=BibleMeta(code="X", name="Teste", license="public-domain", scope="full", source="t"),
        books=[Book(id=20, code="PRO", name="Provérbios", abbrev="Pv",
                    chapters=[Chapter(number=10, verses=[Verse(number=n, text=t) for n, t in verses])])],
    )


def test_clean_verse_has_no_findings():
    report = validate_bible(_bible([(1, "Tudo certo aqui.")]))
    assert report.findings == []


def test_empty_text_is_high():
    report = validate_bible(_bible([(1, "   ")]))
    assert [(f.tier, f.reason) for f in report.findings] == [(Tier.HIGH, "empty verse")]


def test_missing_terminal_punctuation_is_low():
    report = validate_bible(_bible([(1, "as mãos laboriosas lhe")]))
    assert report.findings[0].tier == Tier.LOW
    assert report.findings[0].book_code == "PRO"
    assert (report.findings[0].chapter, report.findings[0].verse) == (10, 1)


def test_grouping_is_info_when_next_verse_continues_lowercase():
    report = validate_bible(_bible([(1, "Não adianta me dizerem: fuja para as montanhas"),
                                    (2, "porque os maus já armaram os arcos.")]))
    f1 = [f for f in report.findings if f.verse == 1][0]
    assert f1.tier == Tier.INFO


def test_html_residue_is_low():
    report = validate_bible(_bible([(1, "<b>De Davi.</b> Com Deus estou seguro.")]))
    assert any(f.reason == "HTML residue" and f.tier == Tier.LOW for f in report.findings)


def test_report_counts_by_tier():
    report = validate_bible(_bible([(1, "   "), (2, "termina sem ponto")]))
    assert report.counts == {Tier.HIGH: 1, Tier.LOW: 1, Tier.INFO: 0}
