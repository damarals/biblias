from pathlib import Path

from sources.base import SourceAdapter
from sources.bolls import BollsSource
from sources.getbible import GetBibleSource
from sources.openlp_sqlite import OpenLpSqliteSource


def make_source(name: str, sql_dir: Path = Path("inst/sql")) -> SourceAdapter:
    if name == "openlp":
        return OpenLpSqliteSource(sql_dir)
    if name == "bolls":
        return BollsSource()
    if name == "getbible":
        return GetBibleSource()
    raise ValueError(f"Fonte desconhecida: {name}")
