"""Dashboard wiring checks; test_checkpoint_runner.R executes the real runner."""
from pathlib import Path
import unittest


class ComponentGlobalDashboardTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.source = (Path(__file__).resolve().parents[1] / 'inst' / 'shiny_app' /
                      'ASU_Flexdashboard_mapgl.Rmd').read_text(encoding='utf-8')

    def test_global_choice_removed_and_other_controls_available(self):
        self.assertNotIn('"Component-first global (automatic ASU count)" = "component_global"', self.source)
        self.assertIn('selected = "single"', self.source)
        self.assertIn('max_asus = if (use_component_global) NULL else max_asus', self.source)
        self.assertIn('parallel_asus <- 1L', self.source)
        self.assertIn('incumbent_stall_seconds <- 0L', self.source)
        self.assertNotIn("input.cpsat_strategy !== 'component_global'", self.source)
        start = self.source.index('shiny::numericInput("cpsat_max"')
        end = self.source.index('shiny::numericInput("cpsat_timelimit"', start)
        controls = self.source[start:end]
        for name in ('cpsat_max', 'cpsat_parallel_asus', 'cpsat_incumbent_stall_seconds'):
            self.assertIn(name, controls)

    def test_runner_global_flag_and_bound_metadata(self):
        self.assertIn('component_global=%s,', self.source)
        self.assertIn('if (use_component_global) "True" else "False",', self.source)
        self.assertIn("if kwargs['component_global'] and isinstance(res, dict):", self.source)
        for key in ('status', 'optimal', 'total_unemp', 'upper_bound', 'absolute_gap', 'relative_gap', 'rounds'):
            self.assertIn(repr(key), self.source)
        self.assertIn('json.dump(output, g)', self.source)
        self.assertIn('[STAGE] COMPONENT_GLOBAL_RESULT status=%s optimal=%s valid_unemp=%s', self.source)

    def test_removed_strategy_has_no_selection_help_panel(self):
        self.assertNotIn("input.cpsat_strategy === 'component_global'", self.source)
        self.assertNotIn("Experimental global selection:", self.source)
        for label in ("Partitioning strategy (potential multiple ASUs)",
                      "Legacy single-ASU solve", "Split saved ASUs (joint model)"):
            self.assertIn(label, self.source)

    def test_attached_strategy_controls_skip_and_recovery(self):
        self.assertIn('"asu_job_state", "asu_job_recover", "asu_job_read"', self.source)
        self.assertIn('cpsat_active_strategy(asu_job_read(file.path(run_dir, "job.json"))$strategy)', self.source)
        self.assertIn('if (identical(cpsat_active_strategy(), "component_global")) "Skip Cut Round"', self.source)
        self.assertIn('legacy_checkpoint <- !use_partitioning && !use_split && !use_component_global', self.source)
        self.assertIn('skip cut round requested', self.source)
        self.assertIn('preserving the best valid selection, and continuing the global search.', self.source)


if __name__ == '__main__':
    unittest.main()
