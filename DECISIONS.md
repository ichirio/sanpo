# Decisions (provisional)

Choices made for the first minimal version (branch `feat/mvp-journey`,
2026-10-26), following the concept memo
`C:/Yrepo/_archive/sanpo-concept-v2-2026-10-25.md`. Each can be changed.

## Provisional choices

| # | Topic | Choice now | Alternatives |
|---|---|---|---|
| 1 | ggplot2 version | `ggplot2 (>= 3.5.0)`; the sunburst uses `coord_radial()` with only the arguments that exist in 3.5 (`theta`, `expand`, `inner.radius`) | `coord_polar()` fallback |
| 2 | Lazy tables (dbplyr) | **one code path** (2026-10-26): every data verb accepts a data frame or a `dbplyr` lazy table and is written only with `dplyr` verbs that `dbplyr` translates; `engine = c("auto", "db", "memory")`; see "Memory or database" below | data.frame only (MVP) |
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

## Memory or database (one code path)

| # | Topic | Choice now | Alternatives |
|---|---|---|---|
| 17 | Shared path | `jn_sequence()`, `jn_flows()`, `jn_tree()`, `tbl_pathways()` (and the plots, through the same internal workers) run one `dplyr` pipeline on a data frame or a lazy table: window functions (`lag()`, `lead()`, `row_number()`, `cumsum()`, `cummax()`), `filter()`, `count()`, `left_join()`, `union_all()`. Only small aggregates (node, flow and pathway counts) are reshaped in R after `collect()`, with the same R code for both engines. Two lines differ by engine, and neither changes the result: the window order (`dbplyr::window_order()` / `arrange()`) and the window partition (per patient in a database, so it runs in parallel; none in memory, one vectorised pass) | separate base-R and SQL implementations |
| 17a | Windows, not grouped summaries | Episodes and collapsed steps are "blocks" of consecutive rows (`cumsum()` of a start flag). The largest `t1` of a block is `cummax(block * K + t1) - block * K` with `K = 2^22`; first-row values are carried the same way; the block's last row is kept with `lead()`. This gives exactly what `group_by() + summarise()` gives, but grouped `summarise()` / `lag()` in dplyr run once per group (100k patients: 27.5 s instead of 1.1 s), and self-joins repeat the whole query in SQL. Exact up to 2^31 blocks per partition (per patient in a database) | `group_by() + summarise()` (slow in memory) |
| 17b | Temporary tables | in a database the pipeline is materialised (`compute()`) at three points (after episodes, after gaps, before the end state) when the database allows it; otherwise the query stays lazy (correct, slower) | one long query |
| 18 | `dplyr` in `Imports` | **added** (`dplyr (>= 1.1.0)`): the shared path is written in `dplyr` verbs, so it is needed for data frames too. `dbplyr`, `duckdb` and `DBI` are in `Suggests` (needed only for lazy tables / tests) | keep base R for memory (two code paths) |
| 19 | Dates in the shared path | days: `as.integer(start - as.Date("1970-01-01"))` and back with `as.Date("1970-01-01") + t`; months: `year()`, `month()`, `make_date()` -- DuckDB / SQL function names, defined internally in R with the same meaning so that one expression runs in both | per-backend SQL |
| 20 | Ties | rows of a patient are ordered by `start`, then `end`, then `state` (was: input row order), so that ties give the same sequence in memory and in a database | input order (not defined in SQL) |
| 21 | `engine = "auto"` | lazy table: count rows in the database, estimate bytes = rows x (4 per logical / integer / factor, 8 per numeric / Date / date-time, 64 per character column); `<=` limit -> collect and run in memory; `>` limit -> stay in the database. After `jn_sequence()` has built the sequence in the database, the sequence is materialised (`compute()`, a temporary table, when the database allows it) and collected when it fits under the limit, so later steps run in memory. A data frame always runs in memory; `engine = "db"` with a data frame is an error | sample-based size estimate; `object.size()` after a trial collect |
| 22 | Memory limit | argument `memory_limit` on every verb, default from `options(sanpo.memory_limit = "2GB")`; strings `"500MB"`, `"2GB"` (1024 based) or bytes. The option name is built from the package name at run time, so a rename does not touch the code. Collecting above the limit is refused (`engine = "memory"`, `jn_collect()`) with a message that says to aggregate in the database or raise the limit | environment variable |
| 23 | Engine message | every verb and plot sends one message (class `jn_engine_message`): engine chosen, data frame / lazy table, rows, size estimate, limit; `options(sanpo.engine_messages = FALSE)` silences it | quiet by default |
| 24 | Lazy journeys | `class_journey@sequence` may be a lazy table (`state` character there; the validator checks the columns only, `jn_sequence()` guarantees the rest by construction); `print()`, `summary()`, `@n_persons`, the verbs and the plots aggregate it in the database; `jn_collect()` brings it into memory | always collect the sequence |

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

## Timings: memory or database (2026-10-26)

Linux container, 4 cores, 15 GB RAM, R 4.3.3, dplyr 1.1.4, dbplyr 2.5,
duckdb (CRAN, built from source), in-memory DuckDB with default settings.
Seconds elapsed; peak memory is the peak RSS of the whole R process
(`/usr/bin/time`), DuckDB included.

100,000 simulated patients (580,707 rows):

| Step | Memory | DuckDB (`engine = "db"`) |
|---|---|---|
| `jn_sequence()` | 1.09 | 3.42 |
| `jn_flows()` | 0.15 | 0.35 |
| `jn_tree()` | 0.13 | 0.40 |
| `tbl_pathways(min_cell = 10)` | 0.14 | 0.41 |
| `plot_sankey()` + grob | 0.71 | 0.78 |
| `plot_sunburst()` + grob | 0.49 | 0.86 |
| `jn_sequence()` with `engine = "auto"` on the lazy table (under the limit: collect, then memory) | -- | 1.77 |
| peak RSS of the run | 0.30 GB | 0.46 GB |

About 10 million rows (9,988,588 rows, 1,720,000 patients; two
`jn_simulate(860000)` sets in an on-disk DuckDB file):

| Run | Seconds | Peak RSS |
|---|---|---|
| `jn_sequence(engine = "db")`; sequence stays in the database | 18.97 | 1.48 GB (whole run) |
| then `jn_flows()` / `jn_tree()` / `tbl_pathways()` (`engine = "db"`) | 1.09 / 0.96 / 0.86 | |
| then `plot_sankey()` / `plot_sunburst()` + grob (aggregated in DuckDB) | 1.58 / 1.35 | |
| `jn_sequence()` with `engine = "auto"` (estimate 1.3 GB < 2 GB: collected, then memory) | 18.80 | 2.58 GB |

Notes: the first DuckDB version (grouped windows and summaries) took
1.7 s at 100k but 27.5 s in memory; un-partitioned windows in DuckDB
pushed the 10M-row peak to 10.7 GB. The partitioned / vectorised split
(choice 17) fixed both. Peak memory in memory is about twice the size
estimate (2.58 GB for a 1.3 GB estimate), which is why the default limit
(2 GB) is well below the RAM of a typical PC.

## Open (needs a decision from the user)

1. **Package name**: provisional (see choice 8); to be chosen after the
   concept is settled.
2. **`dplyr` in `Imports`** (choice 18): added because the one code path
   for memory and database is written in `dplyr` verbs (requested for this
   change); AGENTS.md asks for maintainer agreement on new imports --
   please confirm.
3. **Size estimate** (choice 21): bytes per character value (64) is a
   rough, conservative guess; peak memory during the in-memory pipeline is
   a few times the estimate (see the timings). Should the limit compare
   the estimate times a safety factor?
4. **Temporary tables** (choice 21): a sequence that stays in the database
   is materialised with `compute()` when allowed. On a read-only production
   database this falls back to a lazy query (slower: each verb re-runs the
   pipeline). Is a writable scratch schema available on the target
   databases?

## Done

- Old `sp_*` functions removed; `AGENTS.md` / `CONTRIBUTING.md` rewritten
  for the journey design; S7 adopted; version 0.0.0.9001.
