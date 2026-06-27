# Risk emergence — the analytic algorithm

Plain-language and detailed documentation of the analytic (Merz-Wuthrich 2008)
one-year reserve-risk calculation used by `merz_wuthrich.py` and
`risk_emergence.py`. For the diagrams, open `algorithm.html` in a browser (it
also contains this explanation at the bottom).

## The idea in one paragraph

A claims triangle tells us the past; the reserve is our estimate of the blank
future. We measure reserve uncertainty in two horizons: the **lifetime / ultimate**
view (Mack 1993 — uncertainty until every claim is settled) and the **one-year**
view (Merz-Wuthrich 2008 — how much the best estimate can move over the next 12
months, the Solvency II reserve-risk horizon). The **risk emergence factor** is
one-year risk / lifetime risk: the share of total reserve uncertainty that
crystallises in the next year. It is computed in closed form (no simulation) and
reproduces the industry-standard R `ChainLadder` result exactly.

## The detailed algorithm

### 0. Setup — turn the triangle into parameters

From the cumulative triangle the code estimates, for each development step *j*:

- **`f_j`** — the volume-weighted development factor (how cumulative claims grow
  from column *j* to *j+1*). The *mean* of the process.
- **`sigma_j^2`** — the variance parameter: the weighted spread of the historical
  link ratios around `f_j`. The *volatility* of that step. The last `sigma`
  (no data to estimate it) is filled by log-linear extrapolation.
- **`S_j`** — the column volume (sum of cumulative claims feeding `f_j`), and
  **`alpha_j = newest-diagonal cumulative / S_j`** — how heavily the most recent
  observation weighs in that column.

Everything else is built from `f_j`, `sigma_j^2`, `S_j`, `alpha_j` and the
projected ultimates `U_i`.

### 1. The iteration — walk each accident year forward, step by step

For each accident year *i*, start at its latest observed column and walk
**forward through its remaining development steps** `s = 0, 1, 2, ...`. At each
future step, add that step's uncertainty to a running total. Two contributions:

- **Process variance** (`res3` in the code): `U_i^2 * (sigma_j^2/f_j^2) / C_ij`.
  Even with the *true* factor known, the next increment is random; this is that
  irreducible randomness. It scales with claim size (`C_ij`) and volatility.
- **Estimation variance** (`res5 * U_i^2`): `U_i^2 * (sigma_j^2/f_j^2) / S_j`,
  accumulated with the `(1-alpha)` products. We only *estimated* `f_j` from
  limited data, so it carries a standard error `proportional to sigma_j^2/S_j`
  (more volume -> tighter estimate).

This is the "iterative" part: variance is **propagated down the development path**
of each accident year and summed — the recursive structure of Mack /
England-Verrall.

### 2. The alpha weights — the one-year filter (the crux)

This is *why one-year < lifetime*. When next year's diagonal lands, it adds **one**
observation to each column, nudging each `f_j` by an amount governed by `alpha_j`
(newest / total volume).

- The **one-year CDR** keeps only the slice of parameter uncertainty that **gets
  resolved by that one new diagonal** — hence the `alpha` and `(1-alpha)` product
  terms: `alpha` = "resolved now", `(1-alpha)` = "still unresolved, waits for
  future diagonals".
- The **lifetime (Mack)** version effectively lets every diagonal arrive
  (everything resolves), so it is the full sum.

So the one-year number is the lifetime uncertainty *filtered through how much
actually becomes observable in 12 months*. Long-tail line -> each year reveals
little -> small filter -> low emergence factor.

### 3. Cross-accident-year covariance — why it is not just a sum

Every accident year uses the **same** estimated factors `f_j`, so their estimation
errors are **shared** -> the years are correlated. The code captures this in the
total with the `res5[min(i,i1)] * U_i * U_i1` double sum (the `min` picks the
more-developed year, which correctly zeroes the covariance once that year has
finished developing). Process risk, being genuinely random per cell, has no such
cross term.

### 4. Reconciliation — why it all hangs together

Today's estimate `E_0` and the eventual true ultimate are linked by the chain of
annual revisions:

    E_0 - final = CDR(1) + CDR(2) + ... + CDR(n)

Under Mack's model the estimate is a fair (martingale) estimate, so **each CDR has
mean zero**, and successive CDRs are driven by **fresh, non-overlapping data ->
uncorrelated**. Uncorrelated mean-zero pieces have **additive variances**:

    Var(CDR_1) + Var(CDR_2) + ... = lifetime (Mack) variance

So the one-year risk and the lifetime risk are the **same uncertainty, sliced by
calendar year**. The emergence factor is simply the first slice over the whole.

### 5. Why it makes sense, in one line

You decompose total reserve uncertainty into *when it gets revealed*. The
next-year slice is what capital must absorb over the Solvency II horizon; the full
bar is the run-off view; and because the slices are orthogonal, the analytic
formula and the simulation agree exactly (e.g. 72.7% both ways on GenIns).

## Assumptions (Mack's model)

Chain ladder is the correct model; accident years are independent; variance is
proportional to the cumulative (alpha = 1, the only case Merz-Wuthrich covers);
no tail beyond the triangle. A single unusual cell feeds both the factor and the
variance of its column, so outliers can move the answer — use the leave-one-out
sensitivity to find them.
