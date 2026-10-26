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

## Same code in memory and in a database

Every data verb takes a data frame or a lazy database table (`dbplyr`,
for example DuckDB) and runs the same `dplyr` code on both, with the same
result. `engine = "auto"` (default) counts the rows in the database,
estimates their size in memory and collects them only when they fit under
the memory limit; otherwise the work stays in the database and only the
aggregated nodes, flows and pathway counts are collected.

```r
con <- DBI::dbConnect(duckdb::duckdb())
ep_db <- dplyr::copy_to(con, ep, "episodes")          # a lazy table
j_db <- jn_sequence(ep_db, states = paste("Drug", LETTERS[1:5]))
#> jn_sequence(): engine = memory (lazy table, 11,746 rows, about 1.7 MB; limit 2.0 GB); collected

options(sanpo.memory_limit = "8GB")    # production: only the setting changes
j_db <- jn_sequence(ep_db, states = paste("Drug", LETTERS[1:5]), engine = "db")
plot_sankey(j_db)                      # aggregated in the database
jn_collect(j_db)                       # bring the sequence into memory (checked against the limit)
```

The colours come from the states dictionary (`jn_states()`) through
`scale_fill_journey()`, so a state has the same colour in every figure.

## License

Apache License 2.0.
