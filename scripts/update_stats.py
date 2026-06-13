"""Snapshot GitHub Release download counts per Bible version and render a chart.

Run daily by .github/workflows/stats.yml. GitHub only exposes the *current*
cumulative download count per release asset (no history), so the time series is
built forward: each run appends one dated row to data/stats/downloads.csv (wide
format: date + one column per version) and redraws data/stats/downloads.svg.
"""
import csv
import datetime
import json
import os
import sys
import urllib.request
from pathlib import Path

sys.path.insert(0, "src")
import catalog  # noqa: E402

REPO = os.environ.get("GITHUB_REPOSITORY", "damarals/biblias")
TOKEN = os.environ.get("GH_TOKEN") or os.environ.get("GITHUB_TOKEN")
CODES = sorted(catalog.CATALOG)
CSV = Path("data/stats/downloads.csv")
SVG = Path("data/stats/downloads.svg")
STARS_SVG = Path("data/stats/stars.svg")


def api(url: str, accept: str = "application/vnd.github+json"):
    headers = {"Accept": accept, "User-Agent": "biblias-stats"}
    if TOKEN:
        headers["Authorization"] = f"Bearer {TOKEN}"
    with urllib.request.urlopen(urllib.request.Request(url, headers=headers), timeout=60) as r:
        return json.loads(r.read().decode("utf-8"))


def fetch_totals() -> dict[str, int]:
    """Sum download_count of every asset, grouped by version code, across all releases."""
    totals = {c: 0 for c in CODES}
    page = 1
    while True:
        batch = api(f"https://api.github.com/repos/{REPO}/releases?per_page=100&page={page}")
        if not batch:
            break
        for release in batch:
            for asset in release.get("assets", []):
                code = asset["name"].split(".")[0]
                if code in totals:
                    totals[code] += asset.get("download_count", 0)
        if len(batch) < 100:
            break
        page += 1
    return totals


def update_csv(totals: dict[str, int]) -> list[dict]:
    today = datetime.date.today().isoformat()
    rows = []
    if CSV.exists():
        with CSV.open(encoding="utf-8") as f:
            rows = [r for r in csv.DictReader(f) if r.get("date") != today]
    rows.append({"date": today, **{c: totals[c] for c in CODES}})
    rows.sort(key=lambda r: r["date"])
    CSV.parent.mkdir(parents=True, exist_ok=True)
    fields = ["date"] + CODES
    with CSV.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fields)
        writer.writeheader()
        for row in rows:
            writer.writerow({k: row.get(k, 0) for k in fields})
    return rows


def render(rows: list[dict]) -> None:
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.dates as mdates
    import matplotlib.pyplot as plt

    dates = [datetime.date.fromisoformat(r["date"]) for r in rows]
    fig, ax = plt.subplots(figsize=(11, 6))
    drawn = 0
    for code in CODES:
        ys = [int(r.get(code, 0) or 0) for r in rows]
        if max(ys) == 0:
            continue
        ax.plot(dates, ys, marker="o", ms=3, lw=1.4, label=code)
        drawn += 1
    ax.set_xlabel("Data")
    ax.set_ylabel("Downloads acumulados")
    ax.set_title("Downloads por versão (acumulado nas Releases)")
    ax.grid(True, alpha=0.3)
    if len(dates) > 1:
        ax.xaxis.set_major_formatter(mdates.DateFormatter("%d/%m/%y"))
    if drawn:
        ax.legend(ncol=3, fontsize=8, loc="upper left")
    fig.autofmt_xdate()
    fig.tight_layout()
    fig.savefig(SVG)


def fetch_stars() -> list:
    """Every star timestamp (GitHub keeps this history), sorted ascending."""
    dates = []
    page = 1
    while True:
        batch = api(
            f"https://api.github.com/repos/{REPO}/stargazers?per_page=100&page={page}",
            accept="application/vnd.github.star+json",
        )
        if not batch:
            break
        for star in batch:
            dates.append(datetime.date.fromisoformat(star["starred_at"][:10]))
        if len(batch) < 100:
            break
        page += 1
    return sorted(dates)


def render_stars(dates: list) -> None:
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.dates as mdates
    import matplotlib.pyplot as plt

    fig, ax = plt.subplots(figsize=(11, 6))
    if dates:
        ys = list(range(1, len(dates) + 1))
        ax.plot(dates, ys, lw=2, color="#F25278")
        ax.fill_between(dates, ys, alpha=0.12, color="#F25278")
    ax.set_xlabel("Data")
    ax.set_ylabel("Estrelas")
    ax.set_title("Estrelas ao longo do tempo")
    ax.grid(True, alpha=0.3)
    if len(dates) > 1:
        ax.xaxis.set_major_formatter(mdates.DateFormatter("%m/%y"))
    fig.autofmt_xdate()
    fig.tight_layout()
    fig.savefig(STARS_SVG)


def main() -> None:
    totals = fetch_totals()
    rows = update_csv(totals)
    render(rows)
    render_stars(fetch_stars())
    active = sum(1 for c in CODES if totals[c] > 0)
    print(f"snapshot {rows[-1]['date']}: {sum(totals.values())} downloads, {active} versions; stars chart rendered")


if __name__ == "__main__":
    main()
