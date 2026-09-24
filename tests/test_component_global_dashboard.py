"""Dashboard wiring checks; test_checkpoint_runner.R executes the real runner."""
from pathlib import Path
import unittest


class ComponentGlobalDashboardTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.source = (Path(__file__).resolve().parents[1] / 'inst' / 'shiny_app' /
                      'ASU_Flexdashboard_mapgl.Rmd').read_text(encoding='utf-8')

    def test_global_choice_warm_start_and_controls(self):
        self.assertIn('"Component-first global (automatic ASU count)" = "component_global"', self.source)
        self.assertIn('selected = "single"', self.source)
        self.assertIn('max_asus = if (use_component_global) NULL else max_asus', self.source)
        self.assertIn('parallel_asus <- 1L', self.source)
        self.assertIn('incumbent_stall_seconds <- 0L', self.source)
        start = self.source.index('input.cpsat_strategy !== ')
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

    def test_global_strengthening_help_preserves_proof_and_budget_scope(self):
        self.assertIn('whole-search budget including model preparation, cut generation, and local repair', self.source)
        self.assertIn('Batched component cuts, regional eligibility constraints', self.source)
        self.assertIn('best certified objective ceiling', self.source)
        self.assertIn('unrestricted global search resumes', self.source)
        self.assertIn('local repair bounds never establish global optimality', self.source)
        self.assertIn('Quiet cut rounds run for at most five solver seconds', self.source)

    def test_attached_strategy_controls_skip_and_recovery(self):
        self.assertIn('"asu_job_state", "asu_job_recover", "asu_job_read"', self.source)
        self.assertIn('cpsat_active_strategy(asu_job_read(file.path(run_dir, "job.json"))$strategy)', self.source)
        self.assertIn('if (identical(cpsat_active_strategy(), "component_global")) "Skip Cut Round"', self.source)
        self.assertIn('legacy_checkpoint <- !use_partitioning && !use_split && !use_component_global', self.source)
        self.assertIn('skip cut round requested', self.source)
        self.assertIn('preserving the best valid selection, and continuing the global search.', self.source)


if __name__ == '__main__':
    unittest.main()
