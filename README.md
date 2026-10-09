# sanpo

<!-- badges: start -->
[![R-CMD-check](https://github.com/ichirio/sanpo/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ichirio/sanpo/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**Plots for epidemiology on Japanese claims and medical databases.**

*sanpo* (散歩, "a walk") follows the patient journey: what patients were
treated with, in which order, how many there were over time and how long
they stayed on treatment. Each figure is a static `ggplot2` plot with good
defaults, drawn in one call from patient-level data.

It is a sibling of [rtfreporter](https://github.com/ichirio/rtfreporter)
(RTF output) and [tflspec](https://github.com/ichirio/tflspec)
(specification-driven clinical TFLs).

> Status: early development (phase 0). The API may still change.

## Installation

```r
# install.packages("remotes")
remotes::install_github("ichirio/sanpo")
```

## Treatment-pathway Sankey and sunburst

```r
library(sanpo)

lot <- sp_synth_pathways(n = 500)   # made-up data: patient_id, sex, line, drug_class
sp_sankey(lot)
sp_sunburst(lot, center_label = "N = {n}")
```

A selling point of the Sankey: **it can draw nodes that have no links.**
Nodes are laid out from the node table rather than inferred from the links,
so a group that no patient enters or leaves (in the example data, patients
whose only "line" is *No treatment*) is still drawn at its own size. Tools
that infer the nodes from the links cannot show such a group.

There is no hidden step: every figure is a data function and a plot
function, and the one-call function only chains the two.

| One call       | Data                 | Plot                                         |
|----------------|----------------------|----------------------------------------------|
| `sp_sankey()`  | `sp_sankey_data()`   | `sp_plot_sankey()`, `sp_plot_sankey_batch()` |
| `sp_sunburst()`| `sp_sunburst_data()` | `sp_plot_sunburst()`                         |

## Release 1: a plot collection

| Figure                                         | Status    |
|------------------------------------------------|-----------|
| Treatment-pathway Sankey (nodes without links) | available |
| Treatment-pathway sunburst                     | available |
| Trends over time                               | planned   |
| Age-sex pyramid                                | planned   |
| Rates with confidence intervals                | planned   |
| Forest plot                                    | planned   |
| Kaplan-Meier / cumulative incidence            | planned   |

Design choices:

* Static `ggplot2` plots; the result is an ordinary `ggplot` you can add to.
* Labels default to English (`lang = "en"`); Japanese is one option away.
* Light dependencies: `ggplot2`, `dplyr`, `rlang`, `tidyr` (and `survival`
  for survival curves).

Later phases: small-cell suppression (off by default, selectable), Excel
output through `openxlsx2` and RTF output through `rtfreporter` (both
optional).

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md). Issues and pull requests are welcome.

## License

Apache License 2.0.
