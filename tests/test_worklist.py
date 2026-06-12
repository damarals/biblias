from pathlib import Path

from validate import Finding, Report, Tier
from worklist import render, write_worklist


def _report() -> Report:
    return Report(code="KJA", findings=[
        Finding("PRO", 10, 4, Tier.HIGH, "verse much shorter than other versions (23 vs median 88 chars)"),
        Finding("PSA", 11, 1, Tier.INFO, "verse continues in next (grouping/split)"),
    ])


def test_render_groups_by_tier():
    text = render(_report())
    assert "# Worklist — KJA" in text
    assert "## high (1)" in text
    assert "- PRO 10:4 —" in text
    assert "## info (1)" in text
    assert "## low (0)" in text


def test_write_worklist_creates_file(tmp_path: Path):
    path = write_worklist(_report(), tmp_path)
    assert path == tmp_path / "KJA.md"
    assert "PRO 10:4" in path.read_text(encoding="utf-8")
