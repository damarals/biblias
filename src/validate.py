import re
import statistics
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


def cross_version_findings(
    bibles: list[Bible],
    min_versions: int = 3,
    ratio: float = 0.5,
    min_gap: int = 15,
) -> dict[str, list[Finding]]:
    """Flag verses that are much shorter than the same verse in other versions
    (likely lost words). Compares verse text length across all given bibles."""
    lengths: dict[tuple[str, int, int], dict[str, int]] = {}
    for bible in bibles:
        for book in bible.books:
            for chapter in book.chapters:
                for verse in chapter.verses:
                    ref = (book.code, chapter.number, verse.number)
                    lengths.setdefault(ref, {})[bible.meta.code] = len(verse.text.strip())

    out: dict[str, list[Finding]] = {bible.meta.code: [] for bible in bibles}
    for (book_code, chapter, verse), per_code in lengths.items():
        if len(per_code) < min_versions:
            continue
        median = statistics.median(per_code.values())
        if median == 0:
            continue
        for code, length in per_code.items():
            if length < ratio * median and (median - length) >= min_gap:
                out[code].append(Finding(
                    book_code, chapter, verse, Tier.HIGH,
                    f"verse much shorter than other versions ({length} vs median {int(median)} chars)",
                ))
    return out
