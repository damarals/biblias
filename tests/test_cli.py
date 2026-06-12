import sqlite3
from pathlib import Path

from typer.testing import CliRunner

import cli

runner = CliRunner()


def _make_db(path: Path) -> None:
    con = sqlite3.connect(path)
    con.execute("CREATE TABLE verse (id INTEGER, book_id INTEGER, chapter INTEGER, verse INTEGER, text TEXT)")
    con.execute("INSERT INTO verse VALUES (1, 20, 10, 4, 'As mãos preguiçosas...')")
    con.commit()
    con.close()


def test_help_lists_commands():
    result = runner.invoke(cli.app, ["--help"])
    assert result.exit_code == 0
    assert "fetch" in result.stdout
    assert "build" in result.stdout


def test_fetch_then_build(tmp_path: Path, monkeypatch):
    sql_dir = tmp_path / "inst" / "sql"
    sql_dir.mkdir(parents=True)
    _make_db(sql_dir / "KJA.sqlite")

    canon_dir = tmp_path / "data" / "canonical"
    dist_dir = tmp_path / "dist"
    monkeypatch.setattr(cli, "SQL_DIR", sql_dir)
    monkeypatch.setattr(cli, "CANON_DIR", canon_dir)

    r1 = runner.invoke(cli.app, ["fetch", "KJA"])
    assert r1.exit_code == 0
    assert (canon_dir / "KJA" / "PRO.json").exists()

    r2 = runner.invoke(cli.app, ["build", "KJA", "--out", str(dist_dir)])
    assert r2.exit_code == 0
    assert (dist_dir / "KJA.xml").exists()
