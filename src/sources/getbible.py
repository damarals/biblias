import httpx

import books
import catalog
from model import Bible, BibleMeta, Book, Chapter, Verse

# Our version code -> getbible translation slug (public-domain coverage only).
_SLUGS: dict[str, str] = {
    "BLIVRE": "livre",
    "ALM1911": "almeida",
}


class GetBibleSource:
    """Fetches a version from getbible.net, one book per request."""

    def __init__(self, client: httpx.Client | None = None):
        self._client = client or httpx.Client(base_url="https://api.getbible.net", timeout=30)

    def fetch(self, version: str) -> Bible:
        slug = _SLUGS[version]
        entry = catalog.get(version)
        meta = BibleMeta(code=entry.code, name=entry.name, year=entry.year,
                         publisher=entry.publisher, license=entry.license,
                         scope=entry.scope, source="getbible")
        refs = [b for b in books.BOOKS if entry.scope == "full" or b.testament == "NT"]
        built: list[Book] = []
        for ref in refs:
            data = self._client.get(f"/v2/{slug}/{ref.id}.json").raise_for_status().json()
            chapters = [
                Chapter(number=ch["chapter"],
                        verses=[Verse(number=v["verse"], text=v["text"]) for v in ch["verses"]])
                for ch in data["chapters"]
            ]
            built.append(Book(id=ref.id, code=ref.code, name=ref.name,
                              abbrev=ref.abbrev, chapters=chapters))
        return Bible(meta=meta, books=built)
