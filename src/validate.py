import re
import statistics
from dataclasses import dataclass
from enum import Enum

from model import Bible

_TERMINAL = ('.', '!', '?', '"', '”', '’', '»', ')', ']', ':', ';', '…', '—', '*')
_HTML = re.compile(r"<[a-zA-Z/]")
_FUSE = re.compile(r"[A-Za-zÀ-ÿ]\d|\d[A-Za-zÀ-ÿ]")  # footnote marker fused into a word (e.g. "le9")
_JUNK = ("verszeile", "ohne text")                  # foreign placeholders leaked by legacy tooling
_DASHES = set("—–- ")

# Paraphrases don't track formal verse boundaries; cross-version length comparison is invalid for them.
_PARAPHRASE = frozenset({"OL", "MENS"})


class Tier(str, Enum):
    HIGH = "high"    # corruption or likely lost words (needs a real fix)
    LOW = "low"      # missing terminal punctuation / minor integrity
    INFO = "info"    # versification / grouping / intentional omission (expected for some versions)


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


def _is_corruption(text: str) -> bool:
    low = text.lower()
    return bool(_FUSE.search(text)) or any(j in low for j in _JUNK)


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
                if _is_corruption(verse.text):
                    findings.append(Finding(book.code, chapter.number, verse.number,
                                            Tier.HIGH, "corruption (leaked marker / placeholder)"))
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


def _classify_short(text: str, next_lower: bool, length: int, median: float) -> tuple[Tier, str]:
    """Decide the tier of a verse flagged 'much shorter than other versions'.
    Only genuine truncation stays HIGH; structurally-explained cases drop to INFO."""
    stripped = text.strip()
    note = f"({length} vs median {int(median)} chars)"
    if len(stripped) <= 3 or set(stripped) <= _DASHES:
        return Tier.INFO, "much shorter — likely intentional omission"
    if next_lower:
        return Tier.INFO, f"much shorter — continues in next verse (versification split) {note}"
    if _ends_clean(text):
        return Tier.INFO, f"shorter but complete — critical-text/versification {note}"
    if stripped.endswith(","):
        return Tier.INFO, f"much shorter — list/continuation {note}"
    return Tier.HIGH, f"verse much shorter than other versions — likely truncation {note}"


def cross_version_findings(
    bibles: list[Bible],
    min_versions: int = 3,
    ratio: float = 0.5,
    min_gap: int = 15,
) -> dict[str, list[Finding]]:
    """Flag verses much shorter than the same verse in other (formal) versions, then
    classify each: genuine truncation stays HIGH; intentional omissions, versification
    splits, complete-but-short verses and list continuations drop to INFO. Paraphrases
    are excluded — verse-by-verse length comparison is meaningless for them."""
    formal = [b for b in bibles if b.meta.code not in _PARAPHRASE]
    lengths: dict[tuple[str, int, int], dict[str, int]] = {}
    texts: dict[tuple[str, str, int, int], str] = {}
    next_lower: dict[tuple[str, str, int, int], bool] = {}
    for bible in formal:
        for book in bible.books:
            for chapter in book.chapters:
                verses = chapter.verses
                for i, verse in enumerate(verses):
                    ref = (book.code, chapter.number, verse.number)
                    lengths.setdefault(ref, {})[bible.meta.code] = len(verse.text.strip())
                    key = (bible.meta.code, *ref)
                    texts[key] = verse.text
                    nxt = verses[i + 1] if i + 1 < len(verses) else None
                    next_lower[key] = bool(nxt and nxt.text.strip()[:1].islower())

    out: dict[str, list[Finding]] = {bible.meta.code: [] for bible in bibles}
    for (book_code, chapter, verse), per_code in lengths.items():
        if len(per_code) < min_versions:
            continue
        median = statistics.median(per_code.values())
        if median == 0:
            continue
        for code, length in per_code.items():
            if length < ratio * median and (median - length) >= min_gap:
                tier, reason = _classify_short(
                    texts[(code, book_code, chapter, verse)],
                    next_lower[(code, book_code, chapter, verse)],
                    length, median,
                )
                out[code].append(Finding(book_code, chapter, verse, tier, reason))
    return out
