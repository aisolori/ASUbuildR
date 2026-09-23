"""Checkpoint synchronization and early legacy result preservation."""
import json
from pathlib import Path
import sys
import tempfile
import threading
import time
import unittest
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst" / "python"))
sys.path.insert(0, str(Path(__file__).resolve().parent))
from asu_checkpoint import request_rds_checkpoint
from test_main_immediate_merge import MainImmediateMergeTest
import asu_cpsat as solver


class CheckpointTest(unittest.TestCase):
    def test_waits_for_success_ack_and_retains_assignments(self):
        with tempfile.TemporaryDirectory() as directory:
            def acknowledge():
                deadline = time.monotonic() + 5
                while time.monotonic() < deadline:
                    requests = list(Path(directory).glob("*.request.json"))
                    if requests:
                        request = requests[0]
                        self.assertEqual(json.loads(request.read_text())["asu_id"], [1, -1])
                        time.sleep(.15)
                        ack = Path(str(request).replace(".request.json", ".ack.json"))
                        pending = ack.with_suffix(".pending")
                        pending.write_text(json.dumps({"ok": True, "path": "saved.rds"}))
                        pending.replace(ack)
                        return
                    time.sleep(.01)
                raise AssertionError("No request")
            thread = threading.Thread(target=acknowledge)
            thread.start()
            start = time.monotonic()
            request_rds_checkpoint(directory, "LEGACY_SOLVE", [1, -1], timeout=5)
            thread.join()
            self.assertGreaterEqual(time.monotonic() - start, .15)
            self.assertEqual(len(list(Path(directory).glob("*.request.json"))), 1)

    def test_timeout_preserves_json_and_stops(self):
        with tempfile.TemporaryDirectory() as directory:
            with self.assertRaisesRegex(RuntimeError, "timed out"):
                request_rds_checkpoint(directory, "LEGACY_SOLVE", [1], timeout=0)
            self.assertEqual(len(list(Path(directory).glob("*.request.json"))), 1)

    def test_callback_before_refinement_and_no_partition_callback(self):
        original = solver.build_many_asus_cpsat
        for partition in (False, True):
            seen = []
            def callback(phase, ids):
                # Abort here to simulate failure in later stages. The captured
                # exact solution must already be recoverable.
                seen.append((phase, ids))
                raise RuntimeError("checkpoint barrier")
            def build(*args, **kwargs):
                return original(*args, **kwargs, legacy_checkpoint_callback=callback)
            with patch.object(solver, "build_many_asus_cpsat", side_effect=build):
                if partition:
                    MainImmediateMergeTest().run_build(partition=True)
                    self.assertEqual(seen, [])
                else:
                    with self.assertRaisesRegex(RuntimeError, "checkpoint barrier"):
                        MainImmediateMergeTest().run_build()
                    self.assertEqual(seen, [("LEGACY_SOLVE", [1, 1, -1, -1])])


if __name__ == "__main__":
    unittest.main()
