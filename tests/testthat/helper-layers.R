# Geoms of a ggplot's layers, e.g. c("GeomLine", "GeomPoint").
geoms <- function(p) vapply(p$layers, function(l) class(l$geom)[1], character(1))
