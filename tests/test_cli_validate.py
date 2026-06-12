from pathlib import Path

from typer.testing import CliRunner

import canon
import cli
from model import Bible, BibleMeta, Book, Chapter, Verse

runner = CliRunner()


def _save(code: str, text: str, canon_dir: Path) -> None:
    bible = Bible(
        meta=BibleMeta(code=code, name=code, license="copyright", scope="full", source="t"),
        books=[Book(id=20, code="PRO", name="Provérbios", abbrev="Pv",
                    chapters=[Chapter(number=10, verses=[Verse(number=4, text=text)])])],
    )
    canon.save_bible(bible, canon_dir)


def test_validate_writes_worklists(tmp_path: Path, monkeypatch):
    canon_dir = tmp_path / "canonical"
    work_dir = tmp_path / "worklist"
    full = "As mãos preguiçosas empobrecem o ser humano, porém as laboriosas enriquecem."
    _save("A", full, canon_dir)
    _save("B", full, canon_dir)
    _save("KJA", "As mãos preguiçosas lhe", canon_dir)
    monkeypatch.setattr(cli, "CANON_DIR", canon_dir)
    monkeypatch.setattr(cli, "WORKLIST_DIR", work_dir)

    result = runner.invoke(cli.app, ["validate"])
    assert result.exit_code == 0
    assert (work_dir / "KJA.md").exists()
    assert "high (1)" in (work_dir / "KJA.md").read_text(encoding="utf-8")
