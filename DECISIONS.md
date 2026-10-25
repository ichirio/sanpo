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
| 9 | Object system | **S7** class `class_journey` (`@sequence`, `@states`, `@meta`, computed `@n_persons`); decided 2026-10-26, see "S3 or S7" below | S3 list |
| 10 | Function names | `jn_*` for data, `plot_*` for figures, `tbl_*` for tables, `scale_fill_journey()`, `theme_journey()` | memo also lists `geom_journey_*` (later) |
| 11 | Episode rules in `jn_sequence()` | same-state rows with gap <= `grace` merge; gaps measured from the latest end so far; optional `gap_state`; consecutive equal states collapse; overlaps of different states are ordered by start (no combination states yet) | `jn_lines()` (switch / add-on) in 0.2 |
| 12 | End of follow-up | one end state `"(end)"`, appended once after the last step when the patient has fewer than `max_steps` steps (not carried to later steps) | separate exit reasons (censor / death / end of data) |
| 13 | Empty nodes | Sankey `empty = "show"` (default) draws a thin node labelled 0 for every unused state at every step, except the end and gap states; tree plots show no empty branches | empty branches in the tree |
| 14 | Colours | Okabe-Ito for ordinary states, greys for states whose name starts with `"("`; one colour per level everywhere | group-based palettes (memo) |
| 15 | Pathway strings | `tbl_pathways()` drops the end state by default (`include_end = FALSE`), separator `" -> "` | keep the end state |
| 16 | Old `sp_*` functions | **removed** (2026-10-26, user approved), with their tests and docs; no aliases (pre-CRAN) | -- |

## S3 or S7 (decided: S7)

| Criterion | Finding |
|---|---|
| Validated properties | The class validator now holds the invariants every plot relies on: required sequence columns, `state` factor levels identical to the dictionary levels (empty nodes), `step` 1, 2, 3, ... per patient, dictionary columns and unique levels, named `meta`. It runs on construction **and** on every `j@x <- value`, which S3 cannot give without hand-written setters. |
| Generics | `print()`, `summary()` (patients per step and state), `as.data.frame()` (the sequence) and `plot()` (`plot_sankey()`) are S7 methods; `@n_persons` is a read-only computed property, so it cannot go stale. |
| Extension | Other packages (or a later rtfreporter / tflspec bridge) can add methods for the exported `class_journey` with `S7::method()` without depending on our internals. |
| Cost: dependency | `S7` is one extra `Imports:`; ggplot2 >= 4.0 already imports S7, so for current installs it adds nothing. For R < 4.3, `@` is imported from S7 (`@rawNamespace`). |
| Cost: maturity | S7 0.2.x on CRAN, maintained by the R Consortium OOP working group and used by ggplot2 4.0; API stable enough for a pre-CRAN package. |
| Cost: performance | 100k patients: `jn_sequence()` 2.66 s vs 2.39 s with S3 (validator about 0.1-0.3 s); `@n_persons` about 18 ms per call. Acceptable. |
| Teaching | A real example of S7 classes, validators, computed properties and methods on base generics for the package-development course. |

Note: the S7 class is created without a `package =` argument, so the class
name is `journey`, not `<pkg>::journey`; this keeps a later package rename
cheap.

## Timings (R 4.6.1, Windows, this PC)

100,000 simulated patients (580,707 prescription rows):

| Step | Seconds (elapsed) |
|---|---|
| `jn_simulate(1e5)` | 0.55 |
| `jn_sequence()` | 2.39 (S3) / 2.66 (S7) |
| `plot_sankey()` + build grob | 1.11 (first ggplot call in the session) / 0.37 |
| `ggsave()` PNG 1600x1000 (ragg) | 0.61 / 0.31 |
| `plot_sunburst()` + build grob | 0.36 |
| `tbl_pathways(min_cell = 10)` | 0.11 |

The aggregated-flow design is fast enough at this size; no need for an
alternative yet.

## Open (needs a decision from the user)

1. **Package name**: provisional (see choice 8); to be chosen after the
   concept is settled.
2. **dbplyr timing** (choice 2): MVP is data.frame only; the memo
   recommends lazy tables from the start. Planned for 0.2 unless decided
   otherwise.

## Done

- Old `sp_*` functions removed; `AGENTS.md` / `CONTRIBUTING.md` rewritten
  for the journey design; S7 adopted; version 0.0.0.9001.
