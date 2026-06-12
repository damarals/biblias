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
