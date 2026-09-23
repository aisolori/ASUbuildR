"""Regression coverage for national/multi-state dashboard inputs."""
from pathlib import Path
import sys
import unittest

import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "inst" / "python"))
import asu_cpsat as solver


class NationalInputValidationTest(unittest.TestCase):
    def test_solver_rejects_nonfinite_counts_before_model_construction(self):
        for column in ("tract_ASU_unemp", "tract_ASU_emp", "tract_pop2024"):
            with self.subTest(column=column):
                frame = pd.DataFrame({
                    "geoid": ["01001020100", "01001020200"],
                    "tract_ASU_unemp": [10, 20],
                    "tract_ASU_emp": [90, 80],
                    "tract_pop2024": [10000, 10000],
                })
                frame.loc[1, column] = float("nan")
                with self.assertRaisesRegex(
                    ValueError, rf"{column}: 1 invalid counts.*01001020200"
                ):
                    solver.build_many_asus_cpsat(
                        frame, [[1], [0]], .1, 10000,
                        max_asus=1, time_limit=0, workers=1, verbose=False,
                    )

    def test_nonnumeric_counts_identify_the_column(self):
        for column in ("tract_ASU_unemp", "tract_ASU_emp", "tract_pop2024"):
            with self.subTest(column=column):
                frame = pd.DataFrame({
                    "tract_ASU_unemp": [10], "tract_ASU_emp": [90],
                    "tract_pop2024": [10000],
                })
                frame[column] = ["not a count"]
                with self.assertRaisesRegex(ValueError, column + " must contain only numeric values"):
                    solver.build_many_asus_cpsat(frame, [[]], .1, 10000, verbose=False)


if __name__ == "__main__":
    unittest.main()
