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


def test_fetch_nt_only_version_has_27_books():
    # NTJud is scope="nt" -> only NT books fetched.
    bible = _source().fetch("NTJud")
    assert bible.meta.source == "bolls"
    assert len(bible.books) == 27
    assert bible.books[0].code == "MAT"
    assert bible.books[0].chapters[0].verses[0].text == "De Davi. Texto & mais."


def test_unknown_version_raises():
    import pytest
    with pytest.raises(KeyError):
        _source().fetch("KJF")
