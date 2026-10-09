#' Sankey / sunburst input from treatment-line data
#'
#' Turn one-row-per-patient-per-line data (e.g. lines of therapy built from
#' claims) into the inputs of [sp_plot_sankey()] and [sp_plot_sunburst()].
#'
#' @param data Data frame with one row per patient and stage (line).
#' @param id Patient identifier column.
#' @param stage Stage column (e.g. line number); stages are ordered by
#'   their values (numeric order when numeric, level order when a factor).
#' @param category Category drawn as nodes / arcs (e.g. drug class).
#' @param by Optional subgroup column: nodes and links are built for all
#'   patients (`"ALL"`) and for each value of `by`, marked in column `grp`.
#' @param levels Optional order of the categories (default: order of first
#'   appearance, stage by stage).
#' @param stage_prefix Prefix of the node labels (`"L"` gives `"L1: Chemo"`).
#' @return `sp_sankey_data()`: a list with `nodes` (`id`, `stage`, `category`,
#'   `label`, `n`, and `grp` when `by` is given) and `links` (`source`,
#'   `target`, `value`, `grp`). `sp_sunburst_data()`: a data frame with one
#'   column per stage (`L1`, `L2`, ...) and the patient count `n`.
#' @seealso [sp_sankey()] and `sp_sunburst()` call these and the plot
#'   functions in one step.
#' @examples
#' lot <- data.frame(
#'   patient_id = c("1", "1", "2", "3", "3", "3"),
#'   line       = c(1, 2, 1, 1, 2, 3),
#'   drug_class = c("Class A", "Class B", "Class A", "Class B", "Class A", "Class C")
#' )
#' sk <- sp_sankey_data(lot)
#' sp_plot_sankey(sk$nodes, sk$links, node_label = "label", node_value = "n",
#'                node_treatment = "category")
#' sp_plot_sunburst(sp_sunburst_data(lot))
#' @export
sp_sankey_data <- function(data, id = "patient_id", stage = "line", category = "drug_class",
                           by = NULL, levels = NULL, stage_prefix = "L") {
  d <- sp_seq_prepare(data, id, stage, category, levels)
  groups <- list(ALL = d)
  if (!is.null(by)) {
    if (!by %in% names(data)) stop("`by` column not found: ", by, call. = FALSE)
    g <- as.character(data[[by]])[d$.row]
    for (v in unique(stats::na.omit(g))) groups[[v]] <- d[!is.na(g) & g == v, , drop = FALSE]
  }
  build <- function(x, grp) {
    node_key <- paste(x$.stage_i, x$.cat, sep = "\r")
    cnt <- tapply(x$.id, node_key, function(v) length(unique(v)))
    nodes <- unique(x[c(".stage_i", ".cat")])
    nodes <- nodes[order(nodes$.stage_i, match(nodes$.cat, attr(d, "levels_order"))), , drop = FALSE]
    nodes <- data.frame(
      id = sp_seq_node_id(nodes$.stage_i, nodes$.cat),
      stage = paste0(stage_prefix, nodes$.stage_i),
      category = nodes$.cat,
      label = paste0(stage_prefix, nodes$.stage_i, ": ", nodes$.cat),
      n = as.numeric(cnt[paste(nodes$.stage_i, nodes$.cat, sep = "\r")]),
      stringsAsFactors = FALSE
    )
    # transitions between consecutive stages of the same subject
    x <- x[order(x$.id, x$.stage_i), , drop = FALSE]
    nxt <- match(paste(x$.id, x$.stage_i + 1), paste(x$.id, x$.stage_i))
    has <- !is.na(nxt)
    if (any(has)) {
      tr <- data.frame(source = sp_seq_node_id(x$.stage_i[has], x$.cat[has]),
                       target = sp_seq_node_id(x$.stage_i[nxt[has]], x$.cat[nxt[has]]),
                       stringsAsFactors = FALSE)
      links <- stats::aggregate(list(value = rep(1, nrow(tr))), tr, sum)
      links <- links[order(match(links$source, nodes$id), match(links$target, nodes$id)), , drop = FALSE]
    } else {
      links <- data.frame(source = character(), target = character(), value = numeric())
    }
    if (!is.null(by)) {
      nodes$grp <- grp
      links$grp <- rep(grp, nrow(links))
    }
    rownames(links) <- NULL
    list(nodes = nodes, links = links)
  }
  parts <- Map(build, groups, names(groups))
  out <- list(nodes = do.call(rbind, lapply(parts, `[[`, "nodes")),
              links = do.call(rbind, lapply(parts, `[[`, "links")))
  lapply(out, function(x) { rownames(x) <- NULL; x })
}

#' @rdname sp_sankey_data
#' @export
sp_sunburst_data <- function(data, id = "patient_id", stage = "line", category = "drug_class",
                             levels = NULL, stage_prefix = "L") {
  d <- sp_seq_prepare(data, id, stage, category, levels)
  n_stage <- max(d$.stage_i)
  ids <- unique(d$.id)
  wide <- matrix(NA_character_, length(ids), n_stage)
  wide[cbind(match(d$.id, ids), d$.stage_i)] <- d$.cat
  # a path stops at its first missing stage
  for (j in seq_len(n_stage)[-1]) wide[is.na(wide[, j - 1]), j] <- NA
  wide <- as.data.frame(wide, stringsAsFactors = FALSE)
  names(wide) <- paste0(stage_prefix, seq_len(n_stage))
  key <- do.call(paste, c(lapply(wide, function(v) ifelse(is.na(v), "\r", v)), sep = "\t"))
  out <- wide[!duplicated(key), , drop = FALSE]
  out$n <- as.numeric(table(factor(key, levels = unique(key))))
  rownames(out) <- NULL
  out
}

sp_seq_node_id <- function(stage_i, cat) paste0("S", stage_i, "_", cat)

sp_seq_prepare <- function(data, id, stage, category, levels) {
  data <- as.data.frame(data)
  miss <- setdiff(c(id, stage, category), names(data))
  if (length(miss)) stop("Column(s) not found: ", paste(miss, collapse = ", "), call. = FALSE)
  keep <- !is.na(data[[id]]) & !is.na(data[[stage]]) & !is.na(data[[category]]) & data[[category]] != ""
  s <- data[[stage]][keep]
  stages <- if (is.factor(s)) levels(droplevels(s)) else sort(unique(s))
  d <- data.frame(
    .row = which(keep),
    .id = as.character(data[[id]][keep]),
    .stage_i = match(s, stages),
    .cat = as.character(data[[category]][keep]),
    stringsAsFactors = FALSE
  )
  if (anyDuplicated(d[c(".id", ".stage_i")])) {
    stop("More than one row per patient and ", stage, "; keep one ", category, " per line.", call. = FALSE)
  }
  attr(d, "levels_order") <- levels %or% unique(d$.cat[order(d$.stage_i)])
  d
}
