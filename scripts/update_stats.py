"""Snapshot GitHub Release download counts per Bible version and render charts.

Run daily by .github/workflows/stats.yml. GitHub exposes only the *current*
cumulative download count per release asset (no history), so the download time
series is built forward: each run appends rows to data/stats/downloads.csv in
long format (date,version,count) and redraws the chart. Stars come with full
history from the stargazers API. Charts follow the thesis report look: clean
light box (theme_light feel), no grid, monospace ticks, primary #2c5282.
"""
import collections
import csv
import datetime
import json
import os
import sys
import urllib.request
import warnings
from pathlib import Path

sys.path.insert(0, "src")
import catalog  # noqa: E402

# maxticks on a date axis emits a benign "unable to pick an appropriate interval" note.
warnings.filterwarnings("ignore", message="AutoDateLocator was unable to pick an appropriate interval")

REPO = os.environ.get("GITHUB_REPOSITORY", "damarals/biblias")
TOKEN = os.environ.get("GH_TOKEN") or os.environ.get("GITHUB_TOKEN")
CODES = sorted(catalog.CATALOG)
CSV = Path("data/stats/downloads.csv")
DOWNLOADS_SVG = Path("data/stats/downloads.svg")
STARS_SVG = Path("data/stats/stars.svg")
PRIMARY = "#2c5282"


def api(url, accept="application/vnd.github+json"):
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
        dates += [datetime.date.fromisoformat(s["starred_at"][:10]) for s in batch]
        if len(batch) < 100:
            break
        page += 1
    return sorted(dates)


def update_csv(totals: dict[str, int]) -> list[dict]:
    """Append today's per-version counts. Long format survives versions being added/removed."""
    today = datetime.date.today().isoformat()
    rows = []
    if CSV.exists():
        with CSV.open(encoding="utf-8") as f:
            rows = [r for r in csv.DictReader(f) if r.get("date") != today]
    rows += [{"date": today, "version": c, "count": totals[c]} for c in CODES]
    rows.sort(key=lambda r: (r["date"], r["version"]))
    CSV.parent.mkdir(parents=True, exist_ok=True)
    with CSV.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=["date", "version", "count"])
        writer.writeheader()
        writer.writerows(rows)
    return rows


def _style(ax, title: str, xlabel: str, ylabel: str) -> None:
    """Thesis report look: clean light box, no grid, monospace tick labels."""
    from matplotlib.ticker import MaxNLocator

    ax.set_title(title, loc="left", fontsize=17, color="#1a1a1a", pad=12)
    ax.set_xlabel(xlabel, fontsize=14, color="#333333")
    ax.set_ylabel(ylabel, fontsize=15, color="#333333")
    ax.grid(False)
    for spine in ax.spines.values():
        spine.set_color("#c9c9c9")
        spine.set_linewidth(0.8)
    ax.set_ylim(bottom=0)
    ax.yaxis.set_major_locator(MaxNLocator(integer=True))
    ax.tick_params(colors="#444444", labelsize=14, length=3)
    for label in ax.get_xticklabels() + ax.get_yticklabels():
        label.set_fontfamily("monospace")


def render_downloads(rows: list[dict]) -> None:
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.dates as mdates
    import matplotlib.pyplot as plt

    series: dict = collections.defaultdict(dict)
    for r in rows:
        series[r["version"]][datetime.date.fromisoformat(r["date"])] = int(r["count"])
    dates = sorted({datetime.date.fromisoformat(r["date"]) for r in rows})

    fig, ax = plt.subplots(figsize=(7, 4.5))
    palette = plt.cm.tab20.colors
    drawn = 0
    for i, code in enumerate(CODES):
        ys = [series[code].get(d, 0) for d in dates]
        if max(ys) == 0:
            continue
        ax.plot(dates, ys, marker="o", ms=3, lw=1.5, color=palette[i % len(palette)], label=code)
        drawn += 1
    ax.xaxis.set_major_locator(mdates.AutoDateLocator(maxticks=5))
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%m/%y"))
    if len(dates) <= 1:
        only = dates[0] if dates else datetime.date.today()
        ax.set_xlim(only - datetime.timedelta(days=20), only + datetime.timedelta(days=20))
    _style(ax, "Downloads por versão", "", "Downloads")
    if drawn:
        legend = ax.legend(ncol=5, fontsize=10, loc="upper center",
                           bbox_to_anchor=(0.5, -0.12), frameon=False)
        for text in legend.get_texts():
            text.set_fontfamily("monospace")
    fig.savefig(DOWNLOADS_SVG, bbox_inches="tight")


def render_stars(dates: list) -> None:
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.dates as mdates
    import matplotlib.pyplot as plt

    fig, ax = plt.subplots(figsize=(7, 4.5))
    if dates:
        ys = list(range(1, len(dates) + 1))
        ax.plot(dates, ys, lw=2, color=PRIMARY)
        ax.fill_between(dates, ys, alpha=0.10, color=PRIMARY)
    ax.xaxis.set_major_locator(mdates.YearLocator())
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%m/%y"))
    _style(ax, "Estrelas ao longo do tempo", "", "Estrelas")
    fig.savefig(STARS_SVG, bbox_inches="tight")


def main() -> None:
    totals = fetch_totals()
    rows = update_csv(totals)
    render_downloads(rows)
    render_stars(fetch_stars())
    active = sum(1 for c in CODES if totals[c] > 0)
    print(f"snapshot {rows[-1]['date']}: {sum(totals.values())} downloads, {active} versions; charts rendered")


if __name__ == "__main__":
    main()
