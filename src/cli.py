from pathlib import Path

import typer

import canon
import corrections
import validate as validate_mod
import worklist
from validate import Report
from exporters.registry import EXTENSIONS, make_exporter
from sources.registry import make_source

app = typer.Typer(help="Ferramenta para gerar Bíblias em português a partir de uma fonte canônica.")

SQL_DIR = Path("inst/sql")
CANON_DIR = Path("data/canonical")
CORRECTIONS_DIR = Path("data/corrections")
WORKLIST_DIR = Path("data/worklist")


@app.command()
def fetch(code: str, source: str = "openlp", force: bool = False) -> None:
    """Busca uma versão de uma fonte e grava no canônico."""
    if not force and corrections.corrected_refs(code, CORRECTIONS_DIR):
        raise typer.BadParameter(
            f"{code} tem correções manuais registradas; use --force para sobrescrever."
        )
    adapter = make_source(source, SQL_DIR)
    bible = adapter.fetch(code)
    canon.save_bible(bible, CANON_DIR)
    chapters = sum(len(b.chapters) for b in bible.books)
    typer.echo(f"{code}: {chapters} capítulos gravados no canônico.")


@app.command()
def build(code: str, format: str = "zefania", out: Path = Path("dist")) -> None:
    """Gera um ou mais formatos de saída a partir do canônico."""
    formats = [f.strip() for f in format.split(",") if f.strip()]
    for fmt in formats:
        if fmt not in EXTENSIONS:
            raise typer.BadParameter(f"Formato desconhecido: {fmt}")
    bible = canon.load_bible(code, CANON_DIR)
    for fmt in formats:
        make_exporter(fmt).export(bible, out / f"{code}.{EXTENSIONS[fmt]}")
    typer.echo(f"{code}: {', '.join(formats)} gerado(s) em {out}.")


@app.command(name="diff-sources")
def diff_sources(code: str, sources: str = "bolls,getbible") -> None:
    """Compara uma versão entre fontes, validando cada uma."""
    for name in [s.strip() for s in sources.split(",") if s.strip()]:
        try:
            bible = make_source(name, SQL_DIR).fetch(code)
        except Exception as exc:  # noqa: BLE001 - report any source failure as a row
            typer.echo(f"{name}: indisponível ({type(exc).__name__})")
            continue
        counts = validate_mod.validate_bible(bible).counts
        typer.echo(f"{name}: {counts[validate_mod.Tier.HIGH]} alta, "
                   f"{counts[validate_mod.Tier.LOW]} baixa, {counts[validate_mod.Tier.INFO]} info")


@app.command()
def validate(code: str | None = typer.Argument(None)) -> None:
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
