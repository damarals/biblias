import pytest

from exporters.json import JsonExporter
from exporters.registry import EXTENSIONS, make_exporter
from exporters.sqlite import OpenLpSqliteExporter
from exporters.zefania import ZefaniaExporter


def test_make_known_exporters():
    assert isinstance(make_exporter("zefania"), ZefaniaExporter)
    assert isinstance(make_exporter("sqlite"), OpenLpSqliteExporter)
    assert isinstance(make_exporter("json"), JsonExporter)


def test_extensions():
    assert EXTENSIONS == {"zefania": "xml", "sqlite": "sqlite", "json": "json"}


def test_unknown_format_raises():
    with pytest.raises(ValueError):
        make_exporter("pdf")
