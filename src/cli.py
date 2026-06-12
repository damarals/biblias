from pathlib import Path

import typer

import canon
import corrections
import validate as validate_mod
import worklist
from validate import Report
from exporters.zefania import ZefaniaExporter
from sources.openlp_sqlite import OpenLpSqliteSource

app = typer.Typer(help="Ferramenta para gerar Bíblias em português a partir de uma fonte canônica.")

SQL_DIR = Path("inst/sql")
CANON_DIR = Path("data/canonical")
CORRECTIONS_DIR = Path("data/corrections")
WORKLIST_DIR = Path("data/worklist")


@app.command()
def fetch(code: str, source: str = "openlp", force: bool = False) -> None:
    """Busca uma versão de uma fonte e grava no canônico."""
    if source != "openlp":
        raise typer.BadParameter(f"Fonte desconhecida: {source}")
    if not force and corrections.corrected_refs(code, CORRECTIONS_DIR):
        raise typer.BadParameter(
            f"{code} tem correções manuais registradas; use --force para sobrescrever."
        )
    bible = OpenLpSqliteSource(SQL_DIR).fetch(code)
    canon.save_bible(bible, CANON_DIR)
    chapters = sum(len(b.chapters) for b in bible.books)
    typer.echo(f"{code}: {chapters} capítulos gravados no canônico.")


@app.command()
def build(code: str, format: str = "zefania", out: Path = Path("dist")) -> None:
    """Gera um formato de saída a partir do canônico."""
    if format != "zefania":
        raise typer.BadParameter(f"Formato desconhecido: {format}")
    bible = canon.load_bible(code, CANON_DIR)
    ZefaniaExporter().export(bible, out / f"{code}.xml")
    typer.echo(f"{code}: {format} gerado em {out}.")


@app.command()
def validate(code: str | None = None) -> None:
    """Valida o canônico e grava worklists por versão."""
    codes = [code] if code else sorted(
        p.name for p in CANON_DIR.iterdir() if (p / "meta.json").exists()
    )
    bibles = [canon.load_bible(c, CANON_DIR) for c in codes]
    cross = validate_mod.cross_version_findings(bibles)
    for bible in bibles:
        single = validate_mod.validate_bible(bible)
        merged = Report(code=bible.meta.code,
                        findings=single.findings + cross.get(bible.meta.code, []))
        worklist.write_worklist(merged, WORKLIST_DIR)
        c = merged.counts
        typer.echo(f"{bible.meta.code}: {c[validate_mod.Tier.HIGH]} alta, "
                   f"{c[validate_mod.Tier.LOW]} baixa, {c[validate_mod.Tier.INFO]} info")
