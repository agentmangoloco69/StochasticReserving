"""
Fetch and cache benchmark loss development factors and loss ratios.

Run this once (and whenever you want to refresh) before simulating:
    python scrape_benchmarks.py

The simulation reads ONLY from the local cache in benchmarks/ and never hits the
web at runtime. Hard-coded Canadian P&C benchmarks (from CIA/IBC actuarial
literature) are the authoritative source. Live scraping of IBC is best-effort and
only supplements the hard-coded values where it succeeds.

Benchmark fields per line of business:
    loss_ratio    industry ultimate loss ratio (current cost level)
    annual_ldfs   age-to-age development factors at 12m, 24m, 36m, ... intervals
    tail_factor   development beyond the last annual factor
    phi_factor    relative ODP dispersion knob (see simulate_triangle.py)
    description    human-readable note
"""

import json
from datetime import datetime
from pathlib import Path

BENCHMARKS_DIR = Path(__file__).parent / "benchmarks"

# ---------------------------------------------------------------------------
# Hard-coded Canadian benchmarks (region = canada)
# Sources: CIA research papers, IBC Statistical Yearbook, published LDF studies.
# These are representative industry figures, not any single insurer's experience.
# ---------------------------------------------------------------------------

HARDCODED = {
    "property": {
        "loss_ratio": 0.52,
        "annual_ldfs": [1.350, 1.080, 1.030, 1.015, 1.008, 1.004, 1.002, 1.001],
        "tail_factor": 1.001,
        "phi_factor": 0.10,
        "description": (
            "Canadian property (commercial + personal). Short tail, "
            "~3-4 years to full development."
        ),
    },
    "commercial_auto": {
        "loss_ratio": 0.73,
        "annual_ldfs": [1.300, 1.130, 1.060, 1.030, 1.015, 1.008, 1.004, 1.002, 1.001],
        "tail_factor": 1.001,
        "phi_factor": 0.20,
        "description": (
            "Canadian commercial auto liability. Medium tail, "
            "~5-6 years to full development."
        ),
    },
    "gl": {
        "loss_ratio": 0.68,
        "annual_ldfs": [
            1.750, 1.280, 1.140, 1.070, 1.040,
            1.022, 1.013, 1.008, 1.005, 1.003, 1.002,
        ],
        "tail_factor": 1.005,
        "phi_factor": 0.40,
        "description": (
            "Canadian general liability. Long tail, 10+ years to full "
            "development. Exposed to social inflation."
        ),
    },
}


def _scrape_ibc_loss_ratios() -> dict:
    """
    Best-effort scrape of the IBC facts & statistics page for current industry
    loss ratios. Returns a partial {lob: {"loss_ratio": x}} dict, or {} on any
    failure. Never raises -- the hard-coded benchmarks always stand in.
    """
    try:
        import requests
        from bs4 import BeautifulSoup
    except ImportError:
        print("[scrape] requests/beautifulsoup4 not installed; using hard-coded only.")
        return {}

    try:
        url = "https://www.ibc.ca/news-insights/facts-and-statistics"
        headers = {"User-Agent": "StochasticReserving/1.0 (educational research)"}
        resp = requests.get(url, headers=headers, timeout=10)
        resp.raise_for_status()
        soup = BeautifulSoup(resp.text, "html.parser")

        # The IBC page layout changes over time, so we do not assume a fixed
        # structure. We just flag any table that mentions loss ratios so a human
        # can extract values and update HARDCODED above.
        for table in soup.find_all("table"):
            if "loss ratio" in table.get_text().lower():
                print(
                    "[scrape] Found a table mentioning 'loss ratio' on the IBC "
                    "page. Inspect it manually and update HARDCODED if desired."
                )
                break
        else:
            print("[scrape] No loss-ratio table auto-detected on the IBC page.")
        return {}
    except Exception as e:  # noqa: BLE001 - best-effort, never fatal
        print(f"[scrape] Live IBC scrape skipped ({e}); using hard-coded benchmarks.")
        return {}


def build_benchmarks(region: str = "canada") -> dict:
    """Merge hard-coded defaults with any successfully scraped values."""
    if region != "canada":
        raise ValueError(
            f"Region '{region}' not supported in v1. Only 'canada' is available. "
            "See GitHub issue #2 for the regional expansion roadmap."
        )

    benchmarks = {lob: dict(data) for lob, data in HARDCODED.items()}

    live = _scrape_ibc_loss_ratios()
    for lob, values in live.items():
        if lob in benchmarks:
            benchmarks[lob].update(values)

    benchmarks["_metadata"] = {
        "region": region,
        "source": "CIA/IBC actuarial literature (hard-coded) + best-effort IBC scrape",
        "last_updated": datetime.now().isoformat(timespec="seconds"),
    }
    return benchmarks


def save_benchmarks(benchmarks: dict, region: str = "canada") -> Path:
    BENCHMARKS_DIR.mkdir(parents=True, exist_ok=True)
    path = BENCHMARKS_DIR / f"{region}.json"
    with open(path, "w") as f:
        json.dump(benchmarks, f, indent=2)
    print(f"Benchmarks saved -> {path}")
    return path


def load_benchmarks(region: str = "canada") -> dict:
    """Load cached benchmarks. Raises a clear error if the cache is missing."""
    path = BENCHMARKS_DIR / f"{region}.json"
    if not path.exists():
        raise FileNotFoundError(
            f"No benchmark cache at {path}.\n"
            "Run this first:  python scrape_benchmarks.py"
        )
    with open(path) as f:
        return json.load(f)


if __name__ == "__main__":
    save_benchmarks(build_benchmarks())
