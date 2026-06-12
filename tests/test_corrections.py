import json
from pathlib import Path

from corrections import corrected_refs


def test_no_file_means_empty(tmp_path: Path):
    assert corrected_refs("KJA", tmp_path) == set()


def test_reads_refs(tmp_path: Path):
    (tmp_path / "KJA.json").write_text(
        json.dumps([{"book": "PRO", "chapter": 10, "verse": 4},
                    {"book": "PSA", "chapter": 11, "verse": 1}]),
        encoding="utf-8",
    )
    assert corrected_refs("KJA", tmp_path) == {("PRO", 10, 4), ("PSA", 11, 1)}
