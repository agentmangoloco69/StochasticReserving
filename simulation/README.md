# Claims Triangle Simulation

Tools to simulate realistic quarterly claims development triangles for general
insurance, and to derive expected loss ratios (ELRs) from them the way actuaries
do in practice.

Everything is simulated on an **accident-quarter × development-quarter** grid, so
you can always aggregate up to annual (or annual accident year × quarterly
development) afterwards.

## Files

| File | Purpose |
|---|---|
| `scrape_benchmarks.py` | Builds/refreshes the local benchmark cache (development factors, loss ratios, dispersion) for each line of business. |
| `simulate_triangle.py` | Core simulator. CLI, Python API, and `wild` mode. |
| `derive_elr.py` | Standalone: ex-chain-ladder → trend to current → on-level loss ratios → ELR. Works on simulated *or* real triangles. |
| `benchmarks/` | Cached benchmark JSON (created by `scrape_benchmarks.py`). |
| `configs/` | Example YAML configs. |
| `outputs/` | Generated triangles and earned-premium files. |

## Quick start

```bash
pip install -r requirements.txt

# 1. Build the benchmark cache (run once, refresh as needed)
python scrape_benchmarks.py

# 2. Simulate a triangle
python simulate_triangle.py --config configs/gl_canada.yaml

# 3. Derive ELRs from it
python derive_elr.py --triangle outputs/gl_canada_triangle.csv \
                     --premium outputs/gl_canada_earned_premium.csv
```

## Python API

```python
from simulate_triangle import simulate

# Single triangle
df = simulate(lob="commercial_auto", region="canada", seed=7)

# Batch of 100 triangles (one CSV with a sim_id column)
frames = simulate(lob="gl", n_simulations=100)

# Wild mode: realistic pattern for an UNKNOWN line of business
df = simulate(lob="wild", seed=123)
```

## Scope (v1)

- **Lines of business:** Property, Commercial Auto, General Liability, plus `wild`.
- **Region:** Canada only.
- **Noise model:** Over-dispersed Poisson (matches the bootstrap in `../Reserve_Risk`).
- **Benchmarks:** Hard-coded from CIA/IBC actuarial literature; live IBC scrape is best-effort and only supplements them.

## Roadmap (tracked as GitHub issues)

- **#1** More lines of business (Workers Comp, Med Mal, D&O, cyber, …).
- **#2** More regions (US via NAIC, Europe, Australia), each with its own development shape.
- **#3** Seasonality (esp. Property: cat seasons by quarter).
- **#4** Full earned-premium treatment (on-levelling, premium trend/noise).

## Modelling notes & simplifications

- Annual development factors are interpolated to quarterly with a piecewise-linear
  cumulative curve (flat quarterly increments within a development year). Smoother
  sub-annual interpolation is a future improvement.
- The development pattern is normalised to fully develop within the triangle window.
- ODP dispersion `phi_factor` is treated as a scale-free relative knob: the absolute
  ODP variance is anchored to the average cell size, so the coefficient of variation
  of the average cell is roughly `sqrt(phi_factor)`.
- Earned premium is computed only to anchor the loss scale and is written to a side
  file; richer treatment is issue #4.
