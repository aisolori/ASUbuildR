"""Preview publication requires a strict gain over the saved/best selection."""
import ast
import contextlib
import io
from pathlib import Path
import threading
import unittest
from unittest.mock import Mock

import numpy as np


class IncumbentPreviewTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        path = Path(__file__).resolve().parents[1] / "inst/python/asu_cpsat.py"
        tree = ast.parse(path.read_text(encoding="utf-8"))
        factory = next(node for node in ast.walk(tree)
                       if isinstance(node, ast.FunctionDef)
                       and node.name == "_incumbent_preview")
        cls.code = compile(ast.fix_missing_locations(
            ast.Module(body=[factory], type_ignores=[])), str(path), "exec")

    def setUp(self):
        self.emit = Mock()
        self.env = dict(
            progress_out_path="progress.json", u=np.array([10, 5, 5, 0, 20]),
            preview_lock=threading.Lock(), preview_deltas={},
            _emit_progress=self.emit, verbose=True,
        )
        exec(self.code, self.env)

    def preview(self, key="test", baseline=(0,)):
        return self.env["_incumbent_preview"](key, range(5), baseline)

    def test_duplicate_equal_and_worse_selections_do_not_publish(self):
        report = self.preview()
        output = io.StringIO()
        with contextlib.redirect_stdout(output):
            report([0], 10)          # Existing saved incumbent.
            report([0, 3], 10)       # Different shape, no unemployment gain.
            self.emit.assert_not_called()
            report([0, 1], 15)       # First strict improvement.
            report([1, 0, 1], 15)    # Same set, different ordering/duplicates.
            report([0, 2], 15)       # Equal-value different selection.
            report([0], 10)          # Worse than the preview.
            report([0, 1], 999)      # Objective changes are not real U gains.
            self.assertEqual(self.emit.call_count, 1)
            report([0, 1, 2], 20)    # Second strict improvement.
        self.assertEqual(self.emit.call_count, 2)
        self.assertEqual(output.getvalue().count("[incumbent map]"), 2)
        self.emit.assert_called_with(
            "INCUMBENT_PREVIEW", exploring_added_idx=[1, 2],
            exploring_removed_idx=[])

    def test_independent_territories_keep_their_own_best(self):
        first = self.preview("first")
        second = self.preview("second", baseline=(2,))
        with contextlib.redirect_stdout(io.StringIO()):
            first([0, 4], 30)
            second([2, 1], 10)  # Lower than first's value, but improves this ASU.
            second([2, 3], 5)
        self.assertEqual(self.emit.call_count, 2)
        self.emit.assert_called_with(
            "INCUMBENT_PREVIEW", exploring_added_idx=[1, 4],
            exploring_removed_idx=[])

    def test_new_attempt_resets_baseline_and_disabled_preview_is_none(self):
        with contextlib.redirect_stdout(io.StringIO()):
            self.preview()([0, 4], 30)
            self.env["preview_deltas"].clear()
            self.preview()([0, 1], 15)
        self.assertEqual(self.emit.call_count, 2)
        self.env["progress_out_path"] = None
        self.assertIsNone(self.preview())


if __name__ == "__main__":
    unittest.main()
