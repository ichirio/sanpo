# Regenerate the README example images in man/figures/ (made-up data only).
# Run from the package root after installing the package:
#   Rscript data-raw/readme-figures.R
library(sanpo)

out <- file.path("man", "figures")
dir.create(out, showWarnings = FALSE, recursive = TRUE)
save_fig <- function(p, name, width = 160, height = 100) {
  sp_save(p, file.path(out, paste0(name, ".png")), width = width, height = height, dpi = 120)
}

lot <- sp_synth_pathways(n = 500)
claims <- sp_synth_claims(n = 2000)
patients <- claims[!duplicated(claims$patient_id), ]
surv <- sp_synth_survival(n = 400)
rx <- sp_synth_rx(n = 300)

save_fig(sp_sankey(lot), "sankey", height = 110)
save_fig(sp_sunburst(lot, center_label = "N = {n}"), "sunburst", width = 120, height = 120)
save_fig(sp_trend(claims, by = "sex"), "trend")
save_fig(sp_trend(claims, unit = "fiscal_year", by = "sex"), "trend-fy")
save_fig(sp_pyramid(patients), "pyramid", height = 110)
save_fig(sp_rate(sp_synth_rates(), by = "sex", per = 1e5), "rate")
save_fig(sp_forest(sp_synth_estimates(), group = "group"), "forest", height = 90)
save_fig(sp_km(surv, group = "group", risk_table = TRUE, time_lab = "Days"), "km", height = 120)
save_fig(sp_km(surv, status = "status_cr", group = "group", time_lab = "Days"), "cuminc")
save_fig(sp_persistence(sp_episodes(rx, gap = 30, by = "drug"), group = "drug",
                        risk_table = TRUE), "persistence", height = 120)
save_fig(sp_pref_tile(sp_synth_pref(), value = "rate", legend_title = "Per\n100,000"),
         "pref-tile", width = 140, height = 120)
