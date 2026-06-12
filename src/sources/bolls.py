import html
import re
import time

import httpx

import books
import catalog
import versification
from model import Bible, BibleMeta, Book, Chapter, Verse

# Our version code -> bolls translation slug.
_SLUGS: dict[str, str] = {
    "ACF": "ACF11", "ARA": "ARA", "ARC": "ARC09", "AS21": "ALM21", "KJA": "KJA",
    "NAA": "NAA", "NBV": "NBV07", "NTLH": "NTLH", "NVI": "NVIPT", "NVT": "NVT",
    "TB": "TB10", "OL": "OL", "MENS": "MENS", "NTJud": "NTJud", "VFL": "VFL",
    "CNBB": "CNBB",
}

_TAG = re.compile(r"<[^>]+>")


def _clean(text: str) -> str:
    return html.unescape(_TAG.sub("", text)).strip()


class BollsSource:
    """Fetches a version from bolls.life, one chapter per request."""

    def __init__(self, client: httpx.Client | None = None, retry_base: float = 1.0):
        self._client = client or httpx.Client(base_url="https://bolls.life", timeout=30)
        self._retry_base = retry_base

    def _get_json(self, url: str) -> list:
        # bolls rate-limits aggressively; retry 429/5xx with exponential backoff.
        resp = self._client.get(url)
        for attempt in range(6):
            if resp.status_code == 429 or resp.status_code >= 500:
                time.sleep(self._retry_base * (2 ** attempt))
                resp = self._client.get(url)
                continue
            break
        return resp.raise_for_status().json()

    def fetch(self, version: str) -> Bible:
        slug = _SLUGS[version]
        entry = catalog.get(version)
        meta = BibleMeta(code=entry.code, name=entry.name, year=entry.year,
                         publisher=entry.publisher, license=entry.license,
                         scope=entry.scope, source="bolls")
        refs = [b for b in books.BOOKS if entry.scope == "full" or b.testament == "NT"]
        built: list[Book] = []
        for ref in refs:
            chapters: list[Chapter] = []
            for ch in range(1, versification.chapters_for(ref.code) + 1):
                rows = self._get_json(f"/get-text/{slug}/{ref.id}/{ch}/")
                chapters.append(Chapter(
                    number=ch,
                    verses=[Verse(number=r["verse"], text=_clean(r["text"])) for r in rows],
                ))
            built.append(Book(id=ref.id, code=ref.code, name=ref.name,
                              abbrev=ref.abbrev, chapters=chapters))
        return Bible(meta=meta, books=built)
