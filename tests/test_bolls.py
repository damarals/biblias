import httpx

from sources.bolls import BollsSource, _clean


def _handler(request: httpx.Request) -> httpx.Response:
    # /get-text/<slug>/<booknr>/<chapter>/
    return httpx.Response(200, json=[
        {"pk": 1, "verse": 1, "text": "<b>De Davi.</b> Texto &amp; mais."},
    ])


def _source() -> BollsSource:
    client = httpx.Client(transport=httpx.MockTransport(_handler), base_url="https://bolls.life")
    return BollsSource(client)


def test_clean_strips_tags_and_unescapes():
    assert _clean("<b>De Davi.</b> Texto &amp; mais.") == "De Davi. Texto & mais."


def test_fetch_respects_nt_scope(monkeypatch):
    # A scope="nt" version fetches only the 27 NT books.
    import catalog
    import sources.bolls as bolls_mod
    monkeypatch.setattr(
        catalog, "get",
        lambda code: catalog.CatalogEntry(code, code, None, None, "copyright", "nt"),
    )
    monkeypatch.setitem(bolls_mod._SLUGS, "NTONLY", "NTONLY")
    bible = _source().fetch("NTONLY")
    assert bible.meta.source == "bolls"
    assert len(bible.books) == 27
    assert bible.books[0].code == "MAT"
    assert bible.books[0].chapters[0].verses[0].text == "De Davi. Texto & mais."


def test_unknown_version_raises():
    import pytest
    with pytest.raises(KeyError):
        _source().fetch("KJF")


def test_get_json_retries_on_429():
    calls = {"n": 0}

    def handler(request: httpx.Request) -> httpx.Response:
        calls["n"] += 1
        if calls["n"] == 1:
            return httpx.Response(429)
        return httpx.Response(200, json=[{"verse": 1, "text": "ok"}])

    client = httpx.Client(transport=httpx.MockTransport(handler), base_url="https://bolls.life")
    src = BollsSource(client, retry_base=0)
    assert src._get_json("/get-text/X/1/1/") == [{"verse": 1, "text": "ok"}]
    assert calls["n"] == 2
