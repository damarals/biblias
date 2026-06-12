from pathlib import Path

import httpx
from typer.testing import CliRunner

import cli
from sources.bolls import BollsSource
from sources.getbible import GetBibleSource

runner = CliRunner()


def _bolls_handler(request: httpx.Request) -> httpx.Response:
    return httpx.Response(200, json=[{"pk": 1, "verse": 1, "text": "texto completo."}])


def _getbible_handler(request: httpx.Request) -> httpx.Response:
    nr = int(request.url.path.rsplit("/", 1)[1].removesuffix(".json"))
    return httpx.Response(200, json={"nr": nr, "name": "x", "chapters": [
        {"chapter": 1, "verses": [{"verse": 1, "text": "texto completo."}]}]})


def test_diff_sources_lists_each_source(monkeypatch):
    def fake_make(name: str, sql_dir: Path = Path("inst/sql")):
        if name == "bolls":
            return BollsSource(httpx.Client(transport=httpx.MockTransport(_bolls_handler),
                                            base_url="https://bolls.life"))
        return GetBibleSource(httpx.Client(transport=httpx.MockTransport(_getbible_handler),
                                           base_url="https://api.getbible.net"))

    monkeypatch.setattr(cli, "make_source", fake_make)
    result = runner.invoke(cli.app, ["diff-sources", "NVI", "--sources", "bolls,getbible"])
    assert result.exit_code == 0
    assert "bolls" in result.stdout
    assert "getbible" in result.stdout
