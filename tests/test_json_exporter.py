import json
from pathlib import Path

from exporters.json import JsonExporter
from model import Bible, BibleMeta, Book, Chapter, Verse


def _bible() -> Bible:
    return Bible(
        meta=BibleMeta(code="KJA", name="King James Atualizada", license="copyright",
                       scope="full", source="t"),
        books=[Book(id=1, code="GEN", name="Gênesis", abbrev="Gn", chapters=[
            Chapter(number=1, verses=[Verse(number=1, text="No princípio..."),
                                      Verse(number=2, text="E a terra...")]),
            Chapter(number=2, verses=[Verse(number=1, text="Assim foram...")]),
        ])],
    )


def test_export_writes_legacy_shape(tmp_path: Path):
    out = tmp_path / "KJA.json"
    JsonExporter().export(_bible(), out)
    data = json.loads(out.read_text(encoding="utf-8"))
    assert data == [
        {"abbrev": "Gn", "chapters": [["No princípio...", "E a terra..."], ["Assim foram..."]],
         "name": "Gênesis"}
    ]
