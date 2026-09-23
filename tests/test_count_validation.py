"""Reject bad geometry joins before constructing CP-SAT expressions."""
from pathlib import Path
import sys
import unittest
import warnings

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
import asu_cpsat as solver


class CountValidationTest(unittest.TestCase):
    def test_invalid_counts_fail_with_geoid_before_cast_or_model(self):
        for column in ("tract_ASU_unemp", "tract_ASU_emp", "tract_pop2024"):
            for value in (np.nan, np.inf, -np.inf, -1, float(2**63)):
                with self.subTest(column=column, value=value):
                    frame = pd.DataFrame({"geoid": ["09001000100"],
                                          "tract_ASU_unemp": [7.0], "tract_ASU_emp": [70.0],
                                          "tract_pop2024": [110.0]})
                    frame.loc[0, column] = value
                    with warnings.catch_warnings():
                        warnings.simplefilter("error", RuntimeWarning)
                        with self.assertRaisesRegex(ValueError, column + ".*09001000100"):
                            solver.build_many_asus_cpsat(frame, [[]], .0645, 10000, workers=1, verbose=False)


if __name__ == "__main__":
    unittest.main()
