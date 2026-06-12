from pathlib import Path

from validate import Report, Tier


def render(report: Report) -> str:
    lines = [f"# Worklist — {report.code}", ""]
    counts = report.counts
    for tier in (Tier.HIGH, Tier.LOW, Tier.INFO):
        tier_findings = [f for f in report.findings if f.tier is tier]
        lines.append(f"## {tier.value} ({counts[tier]})")
        for f in tier_findings:
            lines.append(f"- {f.book_code} {f.chapter}:{f.verse} — {f.reason}")
        lines.append("")
    return "\n".join(lines)


def write_worklist(report: Report, root: Path) -> Path:
    root.mkdir(parents=True, exist_ok=True)
    path = root / f"{report.code}.md"
    path.write_text(render(report), encoding="utf-8")
    return path
