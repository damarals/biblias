import pytest

from catalog import CATALOG, get


def test_has_thirteen_seed_versions():
    assert len(CATALOG) == 13
    assert "KJA" in CATALOG
    assert "TB" in CATALOG


def test_get_returns_entry():
    entry = get("KJA")
    assert entry.name == "King James Atualizada"
    assert entry.scope == "full"


def test_tb_is_public_domain():
    assert get("TB").license == "public-domain"


def test_unknown_code_raises():
    with pytest.raises(KeyError):
        get("ZZZ")
