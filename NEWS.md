# sanpo (development version)

## sanpo 0.0.0.9001

* First minimal journey API (package name provisional; see `DECISIONS.md`):
  `jn_simulate()` (synthetic claims-like episodes), `jn_sequence()`
  (episodes -> journey), `journey()`, `jn_states()`, `jn_flows()`,
  `jn_tree()`, `plot_sankey()`, `plot_sunburst()`, `plot_icicle()`,
  `scale_fill_journey()`, `theme_journey()` and `tbl_pathways()`.
* The journey is an S7 class (`class_journey`) with a validator and
  `print()`, `summary()`, `as.data.frame()` and `plot()` methods.
* Removed the phase-0 `sp_*` functions (`sp_sankey()`, `sp_sunburst()`,
  `sp_synth_pathways()` and their data / plot functions); use the journey
  API instead.

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
