# Decisions (provisional)

Choices made for the first minimal version (branch `feat/mvp-journey`,
2026-10-26), following the concept memo
`C:/Yrepo/_archive/sanpo-concept-v2-2026-10-25.md`. Each can be changed.

## Provisional choices

| # | Topic | Choice now | Alternatives |
|---|---|---|---|
| 1 | ggplot2 version | `ggplot2 (>= 3.5.0)`; the sunburst uses `coord_radial()` with only the arguments that exist in 3.5 (`theta`, `expand`, `inner.radius`) | `coord_polar()` fallback |
| 2 | Lazy tables (dbplyr) | MVP is data.frame only, base R, no dplyr | memo's recommendation: accept lazy tables from the start, DuckDB in CI (planned for 0.2) |
| 3 | Defaults with parameters recorded | `grace` defaults to 30 days / 1 month, `max_steps = 4`, `end_state = "(end)"`, `collapse = TRUE`; all stored in `meta` and printed by `print(j)` | required arguments |
| 4 | Suppression | `tbl_pathways(min_cell =)` only, primary suppression per row, default none; no secondary suppression; not yet in plots | `jn_suppress()` hook shared by tables and plots (memo, later) |
| 5 | Synthetic codes | `jn_simulate()` codes are fictional `"6299"` + 5 digits (claims-code shape, no meaning); classes `"Drug A"`..`"Drug E"`, Drug E never used (empty node demo) | fully dummy codes |
| 6 | Interactive output | none | `as_plotly()` in 0.4 |
| 7 | Alluvial (per-patient lines) | none; Sankey draws aggregated flows only | small-n option |
| 8 | Package name | **provisional**: work continues under `sanpo`, but the name will be chosen later to fit the concept ("plots often used in claims / database research that other packages cannot make well, as one family"). The package name is kept out of all function and class names (`jn_*`, `plot_*`, `tbl_*`, class `journey`) so a rename only touches DESCRIPTION, README, docs and the repo | ggjourney, kiseki, others |
| 9 | Object system | S3 class `journey` (list: `sequence`, `states`, `meta`) | S7 as in the memo (also course material) |
| 10 | Function names | `jn_*` for data, `plot_*` for figures, `tbl_*` for tables, `scale_fill_journey()`, `theme_journey()` | memo also lists `geom_journey_*` (later) |
| 11 | Episode rules in `jn_sequence()` | same-state rows with gap <= `grace` merge; gaps measured from the latest end so far; optional `gap_state`; consecutive equal states collapse; overlaps of different states are ordered by start (no combination states yet) | `jn_lines()` (switch / add-on) in 0.2 |
| 12 | End of follow-up | one end state `"(end)"`, appended once after the last step when the patient has fewer than `max_steps` steps (not carried to later steps) | separate exit reasons (censor / death / end of data) |
| 13 | Empty nodes | Sankey `empty = "show"` (default) draws a thin node labelled 0 for every unused state at every step, except the end and gap states; tree plots show no empty branches | empty branches in the tree |
| 14 | Colours | Okabe-Ito for ordinary states, greys for states whose name starts with `"("`; one colour per level everywhere | group-based palettes (memo) |
| 15 | Pathway strings | `tbl_pathways()` drops the end state by default (`include_end = FALSE`), separator `" -> "` | keep the end state |
| 16 | Old `sp_*` functions | left untouched (still exported) next to the new API | remove them (see Open) |

## Timings (R 4.6.1, Windows, this PC)

100,000 simulated patients (580,707 prescription rows):

| Step | Seconds (elapsed) |
|---|---|
| `jn_simulate(1e5)` | 0.55 |
| `jn_sequence()` | 2.39 |
| `plot_sankey()` + build grob | 1.11 |
| `ggsave()` PNG 1600x1000 (ragg) | 0.61 |
| `plot_sunburst()` + build grob | 0.36 |
| `tbl_pathways(min_cell = 10)` | 0.11 |

The aggregated-flow design is fast enough at this size; no need for an
alternative yet.

## Open (needs a decision from the user)

1. **Old `sp_*` functions** (`sp_sankey()`, `sp_sunburst()`,
   `sp_synth_pathways()`, ... moved from tflspec in phase 0): remove them now
   (no aliases before CRAN), or keep until the new API is approved?
   Recommendation: remove in a follow-up once the screens are accepted.
2. **`AGENTS.md` / `CONTRIBUTING.md`** still describe the phase-0
   "three layers per figure, `sp_` prefix" rule; rewrite to the journey
   design (`jn_` / `plot_` / `tbl_`) together with item 1.
3. **S3 or S7** for the journey object (memo says S7; S3 used now for
   simplicity).
4. **Package name**: provisional (see choice 8); to be chosen after the
   concept is settled.
5. **Version bump**: still `0.0.0.9000`; bump to `0.0.0.9001` with the PR.
