"""Check placement of outputs within the dashboard's flex columns."""
from pathlib import Path
import unittest


class DashboardLayoutTests(unittest.TestCase):
    def test_attach_navigation_targets_initial_tab(self):
        dashboard = (Path(__file__).resolve().parents[1] / "inst/shiny_app" /
                     "ASU_Flexdashboard_mapgl.Rmd").read_text(encoding="utf-8")
        self.assertIn('addCustomMessageHandler("asu-show-initial-asu"', dashboard)
        self.assertIn('window.FlexDashboardUtils.showPage("#section-load-initial-asu")', dashboard)
        self.assertIn('# Load Initial ASU\n', dashboard)

    def test_initial_summary_is_below_map_not_in_controls(self):
        dashboard = (Path(__file__).resolve().parents[1] / "inst/shiny_app" /
                     "ASU_Flexdashboard_mapgl.Rmd").read_text(encoding="utf-8")
        initial = dashboard.split("# Load Initial ASU\n", 1)[1].split(
            "# Modify ASU Selections\n", 1)[0]
        controls, map_column = initial.split('## Map {data-width="800"}', 1)
        summary = 'shiny::tableOutput("asu")'
        self.assertNotIn(summary, controls)
        self.assertEqual(dashboard.count(summary), 1)
        self.assertLess(map_column.index('mapgl::maplibreOutput("initial_map"'),
                        map_column.index(summary))
        self.assertIn('shiny::h4("ASU summary")', map_column)
        total = 'shiny::htmlOutput("total_unemp")'
        self.assertNotIn(total, controls)
        self.assertEqual(dashboard.count(total), 1)
        self.assertLess(map_column.index('mapgl::maplibreOutput("initial_map"'),
                        map_column.index(total))
        self.assertLess(map_column.index(total), map_column.index(summary))


if __name__ == "__main__":
    unittest.main()
