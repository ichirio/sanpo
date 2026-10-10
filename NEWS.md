# sanpo (development version)

## sanpo 0.0.0.9001

Phase 1: the release-1 plot collection. Every figure has a data function, a
plot function and a one-call wrapper, takes column names as strings, checks
the shape of its input with clear errors, and labels in English by default
(`lang = "ja"` for Japanese).

* `sp_trend()` (`sp_trend_data()`, `sp_plot_trend()`): monthly, fiscal-year
  (April start) or calendar-year counts of patients or records, optionally
  as rates with a denominator; fiscal-year boundaries are marked on monthly
  plots.
* `sp_pyramid()` (`sp_pyramid_data()`, `sp_plot_pyramid()`): age-sex pyramid
  in 5-year bands with a configurable open top band (`"85+"` by default);
  accepts patient-level or counted data and M/F, male/female or 1/2 codes.
* `sp_rate()` (`sp_rate_data()`, `sp_plot_rate()`): incidence rates (exact
  Poisson, Byar or normal intervals) and prevalence (exact binomial or
  normal intervals) over time.
* `sp_forest()` (`sp_forest_data()`, `sp_plot_forest()`): rate ratios, odds
  ratios and the like on a log axis, with estimate text and subgroup
  headings; recognises `broom::tidy()` column names.
* `sp_km()` (`sp_km_data()`, `sp_plot_km()`, `sp_plot_risk_table()`):
  Kaplan-Meier curves and, for competing risks, Aalen-Johansen cumulative
  incidence via `survival::survfit()`; the number at risk as an optional
  separate panel (needs 'patchwork').
* `sp_persistence()` (`sp_persistence_data()`, `sp_plot_persistence()`)
  with the helper `sp_episodes()`: time to discontinuation from
  prescription fills with an allowed gap, as a Kaplan-Meier preset.
* `sp_pref_tile()` (`sp_pref_tile_data()`, `sp_plot_pref_tile()`,
  `sp_pref_grid()`): a 47-prefecture tile-grid map with no boundary data.
* `sp_theme()`, `sp_palette()` (Okabe-Ito, grey, viridis), `sp_label()`
  (English / Japanese axis words) and `sp_save()` (PNG, LZW-compressed TIFF
  or SVG at journal sizes and dpi).
* Helpers `sp_month()`, `sp_fiscal_year()` and `sp_age_band()`.
* Made-up example data: `sp_synth_claims()`, `sp_synth_rates()`,
  `sp_synth_estimates()`, `sp_synth_survival()`, `sp_synth_rx()`,
  `sp_synth_pref()`.
* Imports now include `dplyr`, `tidyr` and `survival`; `patchwork`, `ragg`,
  `svglite`, `systemfonts` and `vdiffr` are suggested.

## sanpo 0.0.0.9000

* Package skeleton (DESCRIPTION, testthat 3, lintr, R-CMD-check workflow,
  `AGENTS.md`, `CONTRIBUTING.md`).
* Treatment-pathway Sankey and sunburst moved from tflspec, renamed with the
  `sp_` prefix: `sp_sankey_data()`, `sp_sunburst_data()`, `sp_plot_sankey()`,
  `sp_plot_sankey_batch()`, `sp_plot_sunburst()`. Behaviour is unchanged
  except that the data functions now default to the column names
  `patient_id`, `line` and `drug_class` (tflspec: `USUBJID`, `LINE`, `TRT`).
* New one-call wrappers `sp_sankey()` and `sp_sunburst()` (the data function
  followed by the plot function; the table drawn is kept in
  `attr(, "sp_data")`).
* New `sp_synth_pathways()`: a small made-up treatment-sequence data set for
  examples and tests.
