import pytest

from catalog import CATALOG, get


def test_has_twenty_versions():
    assert len(CATALOG) == 20
    assert "KJA" in CATALOG
    assert "BLIVRE" in CATALOG


def test_biblia_livre_is_public_domain_full():
    entry = get("BLIVRE")
    assert entry.license == "public-domain"
    assert entry.scope == "full"


def test_nt_judaico_is_nt_scope():
    assert get("NTJud").scope == "nt"


def test_get_returns_entry():
    entry = get("KJA")
    assert entry.name == "King James Atualizada"
    assert entry.scope == "full"


def test_tb_is_public_domain():
    assert get("TB").license == "public-domain"


def test_unknown_code_raises():
    with pytest.raises(KeyError):
        get("ZZZ")
