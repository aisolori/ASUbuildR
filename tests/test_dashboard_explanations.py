"""Guard audited dashboard explanations against known stale behavior claims."""
from pathlib import Path
import unittest


class DashboardExplanationTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        root = Path(__file__).resolve().parents[1]
        cls.dashboard = (root / "inst/shiny_app/ASU_Flexdashboard_mapgl.Rmd").read_text(encoding="utf-8")
        cls.guide = (root / "inst/shiny_app/dashboard-guide.md").read_text(encoding="utf-8")
        cls.solver = (root / "inst/python/asu_cpsat.py").read_text(encoding="utf-8")
        cls.text = cls.dashboard + "\n" + cls.guide

    def test_detached_job_help(self):
        self.assertIn("CP-SAT jobs survive browser closes and disconnects", self.guide)
        self.assertIn("viewer did not terminate the job", self.dashboard)
        self.assertNotIn("ending the session terminates", self.text)
        self.assertIn("Only running jobs are available to attach", self.dashboard)

    def test_saved_data_is_not_an_optimization_request(self):
        self.assertIn("it does not start or resume optimization", self.dashboard)
        self.assertIn("full_data_reset(loaded_data)", self.dashboard)
        self.assertIn("Loading an RDS sets that file as the new reset baseline", self.guide)
        self.assertNotIn("Loading an RDS does not establish", self.text)
        self.assertIn("LEGACY_REOPTIMIZE", self.guide)

    def test_review_is_reactive_and_unrounded(self):
        self.assertIn("uses the unrounded aggregate rate for qualification", self.guide)
        self.assertIn("Once generated, it reacts to assignment edits", self.guide)
        self.assertIn("`Unemployment Rate Exact` >= input$asu_ur", self.dashboard)
        self.assertNotIn("rounds to five decimal places before comparison", self.text)

    def test_consolidated_polish_dashboard_default(self):
        self.assertIn('"cpsat_polish_consolidated_asus", "Polish consolidated groups", value = TRUE', self.dashboard)
        self.assertIn("**Polish consolidated groups** is optional and on by default in the dashboard", self.guide)
        self.assertIn("Turning off final consolidation does not disable earlier touching-group merges", self.dashboard)

    def test_touching_policy_and_budget(self):
        self.assertIn("PARTITION_TOUCHING_SAFE_UNION", self.solver)
        self.assertIn("partitioning first uses **safe union**", self.guide)
        self.assertIn("at most 180 seconds across the entire run", self.guide)
        self.assertIn("touching_joint_seconds_remaining = min(", self.solver)
        self.assertNotIn("Partitioning no longer automatically unions", self.text)
        self.assertNotIn("instead of automatically merged", self.text)
        self.assertIn("up to 50 rounds", self.guide)
        self.assertIn("max_rounds=50, cut_limit=5000", self.solver)
        self.assertNotIn("at most eight rounds, 60 seconds", self.text)

    def test_cut_limits_and_progress_claims(self):
        self.assertIn("_CUT_ROUND_SECONDS = 5.0", self.solver)
        self.assertIn("each cut solve has a five-second", self.guide)
        self.assertIn("50 rounds without higher valid unemployment", self.dashboard)
        self.assertNotIn("neither a total nor a per-round time cap", self.text)
        self.assertIn("valid_unemp <= best_preview_unemp", self.solver)
        self.assertIn("Identical selections and candidates without a strict gain", self.guide)
        self.assertIn("not one proof of the best overall ASU", self.dashboard)

    def test_export_destinations_and_scope(self):
        self.assertIn("workbook **before** loading the RDS", self.guide)
        self.assertIn("a display-state filter", self.guide)
        self.assertIn("does not filter its output", self.guide)
        self.assertIn("RDS and Save log as downloads go to the browser", self.guide)
        self.assertNotIn("Files are saved where R runs, not automatically", self.text)


if __name__ == "__main__":
    unittest.main()
