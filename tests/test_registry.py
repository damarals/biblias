import pytest

from sources.bolls import BollsSource
from sources.getbible import GetBibleSource
from sources.openlp_sqlite import OpenLpSqliteSource
from sources.registry import make_source


def test_make_known_sources():
    assert isinstance(make_source("bolls"), BollsSource)
    assert isinstance(make_source("getbible"), GetBibleSource)
    assert isinstance(make_source("openlp"), OpenLpSqliteSource)


def test_unknown_source_raises():
    with pytest.raises(ValueError):
        make_source("nope")
