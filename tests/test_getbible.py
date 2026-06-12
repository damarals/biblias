import httpx

from sources.getbible import GetBibleSource


def _handler(request: httpx.Request) -> httpx.Response:
    # /v2/<slug>/<booknr>.json
    nr = int(request.url.path.rsplit("/", 1)[1].removesuffix(".json"))
    return httpx.Response(200, json={
        "nr": nr, "name": "x",
        "chapters": [{"chapter": 1, "verses": [
            {"verse": 1, "text": f"livro {nr} v1"},
            {"verse": 2, "text": f"livro {nr} v2"},
        ]}],
    })


def _source() -> GetBibleSource:
    client = httpx.Client(transport=httpx.MockTransport(_handler),
                          base_url="https://api.getbible.net")
    return GetBibleSource(client)


def test_fetch_full_bible_has_66_books():
    bible = _source().fetch("BLIVRE")
    assert bible.meta.code == "BLIVRE"
    assert bible.meta.source == "getbible"
    assert len(bible.books) == 66
    assert bible.books[0].code == "GEN"
    assert bible.books[0].chapters[0].verses[1].text == "livro 1 v2"


def test_unknown_version_raises():
    import pytest
    with pytest.raises(KeyError):
        _source().fetch("NOPE")
