import re
from dataclasses import dataclass
from enum import Enum

from model import Bible

_TERMINAL = ('.', '!', '?', '"', '”', '’', '»', ')', ']', ':', '…', '—', '*')
_HTML = re.compile(r"<[a-zA-Z/]")


class Tier(str, Enum):
    HIGH = "high"    # lost words (likely missing content)
    LOW = "low"      # missing terminal punctuation / minor integrity
    INFO = "info"    # versification / grouping (expected for some versions)


@dataclass(frozen=True)
class Finding:
    book_code: str
    chapter: int
    verse: int
    tier: Tier
    reason: str


@dataclass(frozen=True)
class Report:
    code: str
    findings: list[Finding]

    @property
    def counts(self) -> dict[Tier, int]:
        return {tier: sum(1 for f in self.findings if f.tier is tier) for tier in Tier}


def _ends_clean(text: str) -> bool:
    stripped = text.rstrip()
    return bool(stripped) and stripped.endswith(_TERMINAL)


def validate_bible(bible: Bible) -> Report:
    findings: list[Finding] = []
    for book in bible.books:
        for chapter in book.chapters:
            verses = chapter.verses
            for i, verse in enumerate(verses):
                text = verse.text.strip()
                if not text:
                    findings.append(Finding(book.code, chapter.number, verse.number,
                                            Tier.HIGH, "empty verse"))
                    continue
                if _HTML.search(verse.text):
                    findings.append(Finding(book.code, chapter.number, verse.number,
                                            Tier.LOW, "HTML residue"))
                if not _ends_clean(verse.text):
                    nxt = verses[i + 1] if i + 1 < len(verses) else None
                    if nxt and nxt.text.strip()[:1].islower():
                        findings.append(Finding(book.code, chapter.number, verse.number,
                                                Tier.INFO, "verse continues in next (grouping/split)"))
                    else:
                        findings.append(Finding(book.code, chapter.number, verse.number,
                                                Tier.LOW, "missing terminal punctuation"))
    return Report(code=bible.meta.code, findings=findings)
