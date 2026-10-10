# Contributing to sanpo

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
   rcmdcheck::rcmdcheck(args = "--as-cran")
   lintr::lint_package()
   ```

5. Open a pull request against `main` that references the issue. CI
   (R-CMD-check and lint) must be green before it is merged.

## Versioning

Standard R scheme: a release is `X.Y.Z`, development is `X.Y.Z.9000`.
**Every pull request bumps the fourth number by one** (`0.0.0.9000` ->
`0.0.0.9001`) and adds an entry to `NEWS.md`. Changing `X`, `Y` or `Z` is a
release, done by the maintainer.

## Code style

- Readable tidyverse style: native pipe `|>`, two-space indent, `<-`,
  `snake_case`. `.lintr` is the reference.
- Exported functions start with `sp_`. Column names are passed as strings.
- R source is ASCII only; Japanese strings go in as `\uXXXX` escapes.
- New figures follow the three layers: `sp_<fig>_data()`,
  `sp_plot_<fig>()`, `sp_<fig>()`.
- Examples and tests use made-up data only (`sp_synth_pathways()` or small
  inline data frames); never real patient data.

## License

By contributing you agree that your contribution is licensed under the
Apache License 2.0 (see `LICENSE.md`).
