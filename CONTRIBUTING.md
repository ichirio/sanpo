# Contributing

Issues and pull requests are welcome at <https://github.com/ichirio/sanpo>.

## Workflow: issue -> pull request

1. Open (or comment on) an issue describing the bug or the change. For
   anything non-trivial, agree the approach there first.
2. Create a topic branch off `main` (from your fork if you do not have write
   access), e.g. `feat/trend-plot` or `fix/sankey-gap`.
3. Make the change with its tests and documentation.
4. Check locally:

   ```r
   roxygen2::roxygenise()
   testthat::test_local()
   lintr::lint_package()
   rcmdcheck::rcmdcheck(args = "--as-cran")   # once, at the end
   ```

5. Open a pull request against `main` that references the issue. CI
   (R-CMD-check and lint) must be green before it is merged.

## Versioning

Standard R scheme: a release is `X.Y.Z`, development is `X.Y.Z.9000`.
**Every pull request bumps the fourth number by one** (`0.0.0.9000` ->
`0.0.0.9001`) and adds an entry to `NEWS.md`. Changing `X`, `Y` or `Z` is a
release, done by the maintainer.

## Design and code style

- The package name `sanpo` is provisional; never put it in function,
  class or argument names.
- Everything is built on one journey object (`class_journey`, S7) made by
  `jn_sequence()`. Data verbs start with `jn_`, figures with `plot_`,
  tables with `tbl_`. Column names are passed as strings.
- Figures are static `ggplot2` objects drawn from aggregated data (flows,
  prefix trees), with order and colours from the states dictionary
  (`jn_states()`, `scale_fill_journey()`). Empty states stay as empty
  nodes.
- Readable tidyverse style: two-space indent, `<-`, `snake_case`. `.lintr`
  is the reference.
- R source is ASCII only; Japanese strings go in as `\uXXXX` escapes.
- Examples and tests use synthetic data only (`jn_simulate()` or small
  inline data frames). Never real patient data and never real code masters
  (YJ, receipt codes, ATC, disease codes); users bring their own maps.
- New `Imports:` need agreement first (one-line reason).

## Local resources

Run one R process at a time and keep sessions short; install into a
private library and keep `HOME` / `R_USER_*` in a private work folder. Run
`R CMD check --as-cran` once, just before opening the pull request.

## License

By contributing you agree that your contribution is licensed under the
Apache License 2.0 (see `LICENSE.md`).
