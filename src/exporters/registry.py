from exporters.base import Exporter
from exporters.json import JsonExporter
from exporters.sqlite import OpenLpSqliteExporter
from exporters.zefania import ZefaniaExporter

EXTENSIONS: dict[str, str] = {"zefania": "xml", "sqlite": "sqlite", "json": "json"}


def make_exporter(fmt: str) -> Exporter:
    if fmt == "zefania":
        return ZefaniaExporter()
    if fmt == "sqlite":
        return OpenLpSqliteExporter()
    if fmt == "json":
        return JsonExporter()
    raise ValueError(f"Formato desconhecido: {fmt}")
