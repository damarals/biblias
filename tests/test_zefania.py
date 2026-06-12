import xml.etree.ElementTree as ET
from pathlib import Path

from exporters.zefania import ZefaniaExporter
from model import Bible, BibleMeta, Book, Chapter, Verse


def _bible() -> Bible:
    return Bible(
        meta=BibleMeta(code="KJA", name="King James Atualizada", year=1999,
                       publisher="Abba Press", license="copyright", scope="full",
                       source="openlp_sqlite"),
        books=[Book(id=20, code="PRO", name="Provérbios", abbrev="Pv",
                    chapters=[Chapter(number=10, verses=[
                        Verse(number=4, text="As mãos preguiçosas empobrecem...")])])],
    )


def test_export_produces_valid_zefania(tmp_path: Path):
    out = tmp_path / "KJA.xml"
    ZefaniaExporter().export(_bible(), out)

    root = ET.parse(out).getroot()
    assert root.tag == "XMLBIBLE"
    assert root.attrib["biblename"] == "King James Atualizada"

    book = root.find("BIBLEBOOK")
    assert book.attrib == {"bnumber": "20", "bname": "Provérbios", "bsname": "Pv"}

    verse = book.find("CHAPTER/VERS")
    assert verse.attrib["vnumber"] == "4"
    assert verse.text.startswith("As mãos preguiçosas")


def test_publisher_present_when_set(tmp_path: Path):
    bible = Bible(
        meta=BibleMeta(code="X", name="X", publisher="Editora Y", license="copyright",
                       scope="full", source="t"),
        books=[Book(id=1, code="GEN", name="Gênesis", abbrev="Gn",
                    chapters=[Chapter(number=1, verses=[Verse(number=1, text="a.")])])],
    )
    out = tmp_path / "X.xml"
    ZefaniaExporter().export(bible, out)
    info = ET.parse(out).getroot().find("INFORMATION")
    assert info.findtext("publisher") == "Editora Y"


def test_publisher_absent_when_none(tmp_path: Path):
    bible = Bible(
        meta=BibleMeta(code="X", name="X", publisher=None, license="public-domain",
                       scope="full", source="t"),
        books=[Book(id=1, code="GEN", name="Gênesis", abbrev="Gn",
                    chapters=[Chapter(number=1, verses=[Verse(number=1, text="a.")])])],
    )
    out = tmp_path / "X.xml"
    ZefaniaExporter().export(bible, out)
    info = ET.parse(out).getroot().find("INFORMATION")
    assert info.find("publisher") is None


def test_special_chars_are_escaped(tmp_path: Path):
    bible = Bible(
        meta=BibleMeta(code="X", name="X", license="public-domain", scope="full", source="t"),
        books=[Book(id=1, code="GEN", name="Gênesis", abbrev="Gn",
                    chapters=[Chapter(number=1, verses=[Verse(number=1, text="a < b & c > d")])])],
    )
    out = tmp_path / "X.xml"
    ZefaniaExporter().export(bible, out)
    assert "&amp;" in out.read_text(encoding="utf-8")
    verse = ET.parse(out).getroot().find("BIBLEBOOK/CHAPTER/VERS")
    assert verse.text == "a < b & c > d"
