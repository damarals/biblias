from pathlib import Path

import typer

import canon
from exporters.zefania import ZefaniaExporter
from sources.openlp_sqlite import OpenLpSqliteSource

app = typer.Typer(help="Ferramenta para gerar Bíblias em português a partir de uma fonte canônica.")

SQL_DIR = Path("inst/sql")
CANON_DIR = Path("data/canonical")


@app.command()
def fetch(code: str, source: str = "openlp") -> None:
    """Busca uma versão de uma fonte e grava no canônico."""
    if source != "openlp":
        raise typer.BadParameter(f"Fonte desconhecida: {source}")
    bible = OpenLpSqliteSource(SQL_DIR).fetch(code)
    canon.save_bible(bible, CANON_DIR)
    chapters = sum(len(b.chapters) for b in bible.books)
    typer.echo(f"{code}: {chapters} capítulos gravados no canônico.")


@app.command()
def build(code: str, format: str = "zefania", out: Path = Path("dist")) -> None:
    """Gera um formato de saída a partir do canônico."""
    bible = canon.load_bible(code, CANON_DIR)
    if format != "zefania":
        raise typer.BadParameter(f"Formato desconhecido: {format}")
    ZefaniaExporter().export(bible, out / f"{code}.xml")
    typer.echo(f"{code}: {format} gerado em {out}.")
