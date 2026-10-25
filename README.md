# sanpo

<!-- badges: start -->
[![R-CMD-check](https://github.com/ichirio/sanpo/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ichirio/sanpo/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

sanpo（散歩）は、レセプトなどの医療データベースから「患者の歩み（journey）」を作り、
サンキー・サンバースト・アイシクル・経路頻度表を同じ順序・同じ色で描く R パッケージです。
図はすべて ggplot2 の静的な図で、誰も通らなかった状態も「空のノード」として残ります。

**Patient journeys from claims and medical databases.** *sanpo* ("a walk")
turns episode data into one journey object -- the sequence of states per
patient, a states dictionary and the parameters used -- and draws Sankey
diagrams, sunbursts, icicles and pathway tables from it with the same node
order and the same colours. Every plot is a static `ggplot`; states no
patient reaches stay as empty nodes.

> Status: early development. The API may still change.

## Installation

```r
# install.packages("remotes")
remotes::install_github("ichirio/sanpo")
```

## From episodes to a journey

```r
library(sanpo)

ep <- jn_simulate(n = 2000)      # synthetic, claims-like rows (fictional codes)
j  <- jn_sequence(ep, grace = 30, max_steps = 4)
j                                # states, empty states and every parameter
```

## Sankey

```r
plot_sankey(j)                   # aggregated flows; "Drug E" is an empty node
```

## Sunburst

```r
plot_sunburst(j, steps = 3)      # rings = steps, coord_radial()
```

## Icicle

```r
plot_icicle(j, steps = 3)        # the same rectangles as the sunburst
```

## Pathway table

```r
tbl_pathways(j, min_cell = 10)   # counts below 10 shown as "<10"
```

The colours come from the states dictionary (`jn_states()`) through
`scale_fill_journey()`, so a state has the same colour in every figure.

## License

Apache License 2.0.
