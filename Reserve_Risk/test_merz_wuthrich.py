"""
Validation test for merz_wuthrich.py.

Checks the Python Merz-Wuthrich implementation (loglinear sigma, the default)
against the reference figures produced by R's ChainLadder::CDR(MackChainLadder(GenIns))
on the Taylor-Ashe / GenIns triangle (== claims_triangle.csv).

Run:  python test_merz_wuthrich.py
"""

import numpy as np
import pandas as pd

from merz_wuthrich import merz_wuthrich

# Reference values from R: ChainLadder::CDR(MackChainLadder(GenIns))
# columns: accident year -> (one-year CDR S.E., ultimate Mack S.E.)
R_REFERENCE = {
    2:  (71835.2, 71835.2),
    3:  (104446.3, 119473.7),
    4:  (78738.8, 131572.8),
    5:  (234800.9, 260530.0),
    6:  (318170.4, 410406.9),
    7:  (360811.6, 557795.5),
    8:  (629452.3, 874882.2),
    9:  (588492.7, 970959.8),
    10: (1029850.0, 1362981.1),
}
R_TOTAL_ONEYEAR = 1774013.8
R_TOTAL_ULTIMATE = 2441364.1
R_TOTAL_RESERVE = 18680855.6


def test_against_r_chainladder():
    inc = pd.read_csv("claims_triangle.csv", index_col=0).to_numpy(dtype=float)
    res = merz_wuthrich(inc, sigma_method="loglinear")
    table = res["table"].set_index("accident_year")

    # Per accident year (relative tolerance 1e-4 covers R's rounding to 0.1)
    for ay, (oneyear, ult) in R_REFERENCE.items():
        got_oneyear = table.loc[ay, "oneyear_CDR_SE"]
        got_ult = table.loc[ay, "ultimate_Mack_SE"]
        assert np.isclose(got_oneyear, oneyear, rtol=1e-4), \
            f"AY{ay} one-year: got {got_oneyear:.1f}, expected {oneyear:.1f}"
        assert np.isclose(got_ult, ult, rtol=1e-4), \
            f"AY{ay} ultimate: got {got_ult:.1f}, expected {ult:.1f}"

    # Totals
    assert np.isclose(res["total_oneyear_se"], R_TOTAL_ONEYEAR, rtol=1e-5)
    assert np.isclose(res["total_ultimate_se"], R_TOTAL_ULTIMATE, rtol=1e-5)
    assert np.isclose(res["total_reserve"], R_TOTAL_RESERVE, rtol=1e-6)
    print("PASS: Python Merz-Wuthrich matches R ChainLadder::CDR on GenIns.")
    print(f"  one-year SE : {res['total_oneyear_se']:,.1f}  (R: {R_TOTAL_ONEYEAR:,.1f})")
    print(f"  ultimate SE : {res['total_ultimate_se']:,.1f}  (R: {R_TOTAL_ULTIMATE:,.1f})")
    print(f"  emergence   : {res['emergence_factor']:.1%}")


if __name__ == "__main__":
    test_against_r_chainladder()
