from model import Bible, BibleMeta, Book, Chapter, Verse
from validate import Tier, cross_version_findings, validate_bible


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


def test_corruption_leaked_marker_is_high():
    report = validate_bible(_bible([(1, "da casa de Baal-Berite e le9")]))
    assert any(f.tier is Tier.HIGH and "corrup" in f.reason for f in report.findings)


def test_corruption_placeholder_is_high():
    report = validate_bible(_bible([(1, "Verszeile ohne Text")]))
    assert any(f.tier is Tier.HIGH and "corrup" in f.reason for f in report.findings)


_LONG = "Esta é uma frase bem longa e completa, suficiente para servir de mediana nas comparações entre as versões."


def _bible_code(code: str, verses: list[tuple[int, str]]) -> Bible:
    return Bible(
        meta=BibleMeta(code=code, name=code, license="public-domain", scope="full", source="t"),
        books=[Book(id=20, code="PRO", name="Provérbios", abbrev="Pv",
                    chapters=[Chapter(number=10, verses=[Verse(number=n, text=t) for n, t in verses])])],
    )


def _cross_tier(short_text: str, next_text: str | None = None, short_code: str = "SHORT") -> Tier | None:
    short = [(1, short_text)] + ([(2, next_text)] if next_text is not None else [])
    long = [(1, _LONG)] + ([(2, "Continuação normal aqui.")] if next_text is not None else [])
    bibles = [_bible_code(f"V{i}", long) for i in range(3)] + [_bible_code(short_code, short)]
    hits = [f for f in cross_version_findings(bibles).get(short_code, []) if f.verse == 1]
    return hits[0].tier if hits else None


def test_cross_omission_is_info():
    assert _cross_tier("—") is Tier.INFO


def test_cross_versification_split_is_info():
    assert _cross_tier("Então falou o Senhor a", next_text="cada um dos que ali estavam.") is Tier.INFO


def test_cross_complete_but_short_is_info():
    assert _cross_tier("Deus é amor.") is Tier.INFO


def test_cross_list_continuation_is_info():
    assert _cross_tier("Quedar, Adbeel, Mibsão,") is Tier.INFO


def test_cross_complete_with_semicolon_is_info():
    assert _cross_tier("e seu filho Ismael tinha treze anos;") is Tier.INFO


def test_cross_genuine_truncation_is_high():
    assert _cross_tier("E o sacerdote olhou para") is Tier.HIGH


def test_paraphrase_excluded_from_cross_version():
    assert _cross_tier("frase curta", short_code="OL") is None
