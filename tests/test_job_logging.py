"""Per-line timestamps without browser-owned pipes or text-only interception."""
import io
import re
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "inst/python"))
from asu_job import TimestampedOutput, wait_with_logging, write_timestamped


STAMP = re.compile(r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}\+00:00 ")


class JobLoggingTests(unittest.TestCase):
    def payloads(self, log):
        lines = log.getvalue().splitlines()
        for line in lines:
            if line:
                self.assertRegex(line, STAMP)
        return [STAMP.sub("", line) for line in lines]

    def test_multiline_and_blank_lines(self):
        log = io.StringIO()
        write_timestamped(log, "#Model 0.1s\n\n#Bound 0.2s\n#1 0.3s best:10\n")
        self.assertEqual(self.payloads(log),
                         ["#Model 0.1s", "", "#Bound 0.2s", "#1 0.3s best:10"])

    def test_partial_utf8_crlf_and_final_unterminated_line(self):
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "raw.log"
            with path.open("wb", buffering=0) as raw, path.open("rb", buffering=0) as source:
                log = io.StringIO()
                output = TimestampedOutput(source, log)
                raw.write(b"#Bound 1s\r\nincumbent \xe2")
                output.drain()
                self.assertEqual(self.payloads(log), ["#Bound 1s"])
                raw.write(b"\x89\xa5 100\nlast partial")
                output.drain()
                self.assertEqual(self.payloads(log), ["#Bound 1s", "incumbent \u2265 100"])
                output.drain(final=True)
                self.assertEqual(self.payloads(log),
                                 ["#Bound 1s", "incumbent \u2265 100", "last partial"])
                unchanged = log.getvalue()
                output.drain(final=True)
                self.assertEqual(log.getvalue(), unchanged)
                # A second child (RDS recovery) shares the same spool.
                raw.write(b"recovery complete\n")
                output.drain(final=True)
                self.assertEqual(self.payloads(log)[-1], "recovery complete")

    def test_large_output_and_non_utf8_do_not_drop_lines(self):
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "raw.log"
            path.write_bytes(b"x" * 240000 + b"\ninvalid: \xff\n")
            with path.open("rb", buffering=0) as source:
                log = io.StringIO()
                TimestampedOutput(source, log).drain(final=True)
            self.assertEqual(self.payloads(log), ["x" * 240000, "invalid: \ufffd"])

    def test_native_stdout_stderr_and_failed_child(self):
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "raw.log"
            log = io.StringIO()
            heartbeats = []
            with path.open("wb", buffering=0) as raw, path.open("rb", buffering=0) as source:
                # os.write exercises file-descriptor output like native CP-SAT,
                # not only Python's sys.stdout wrapper.
                script = ("import os,time; os.write(1,b'#Model 0.1s\\n'); "
                          "os.write(2,b'#Bound 0.2s\\n'); time.sleep(.2); "
                          "os.write(1,b'#1 0.3s best:100'); raise SystemExit(7)")
                child = subprocess.Popen(
                    [sys.executable, "-u", "-c", script],
                    stdout=raw, stderr=subprocess.STDOUT,
                    creationflags=subprocess.CREATE_NO_WINDOW if sys.platform == "win32" else 0)
                try:
                    code = wait_with_logging(child, TimestampedOutput(source, log),
                                             lambda: heartbeats.append(True))
                finally:
                    if child.poll() is None:
                        child.kill()
                    child.wait()
            self.assertEqual(code, 7)
            self.assertTrue(heartbeats)
            self.assertEqual(self.payloads(log),
                             ["#Model 0.1s", "#Bound 0.2s", "#1 0.3s best:100"])
            self.assertNotIn(b"+00:00", path.read_bytes())


if __name__ == "__main__":
    unittest.main()
