
utils::globalVariables(c("."))
#' @importFrom dplyr %>% mutate bind_rows
#' @importFrom tidyr separate_wider_delim
#' @importFrom stats p.adjust setNames
#' @importFrom purrr pluck_depth
NULL


################################################################################
# Formatting mediation models for posterior analysis
################################################################################

#' Formatting results from causal mediation analysis
#'
#' @description `format_med()` extracts the per-model summary tables from a
#'   list of causal mediation analyses. The output mirrors what
#'   `summary()` prints for a single `mediate` object.
#'
#' @param mediation.list list of lists with as many sublists as different
#'   outcomes has been tested in the causal mediation analysis. The first level
#'   of this list will contain the different outcomes that have been analised.
#'   The second level, will contain the mediate objects with the analyses
#'   information.
#' @param split a boolean. This argument indicate if we are dealing with
#'   splitted data. Default: FALSE.
#'
#' @return A nested list. For `split = FALSE`: `list[outcome][model]` where
#'   each element is a data frame with columns Estimate, CI Lower, CI Upper,
#'   and p-value — one row per effect (ACME, ADE, Total Effect, Prop. Mediated).
#'   For `split = TRUE`: `list[outcome][split_condition][model]`.
#' @export
#'
#'
format_med <- function(
    mediation.list,
    split = FALSE
) {
  ## TODO: no está contemplada la posibilidad de covariates
  if (!"logical" %in% class(split)) {
    stop("split argument only admits logical")
  }
  if (split == FALSE) {
    filt_summary <- .format_med(mediation.list=mediation.list)
  }
  else {
    if (purrr::pluck_depth(mediation.list) != 7) {
      stop("Using split=TRUE, the first level in the mediation.list should be the outcomes, the second, the conditions used to split and the third, the models")
    }
    filt_summary <- lapply(names(mediation.list),
                           FUN = function(subl) {
                             .format_med(mediation.list=mediation.list[[subl]])
                           })
    names(filt_summary) <- names(mediation.list)
  }
  return(filt_summary)
}


.format_med <- function(mediation.list) {

  if (!"list" %in% class(mediation.list)) {
    stop("mediation.list is not a list")
  }
  if ( any(unlist(lapply(mediation.list, FUN=function(sublist) { class(sublist) })) != 'list') ) {
    stop("mediation.list is not a list of lists")
  }
  if ( any(unlist(lapply(mediation.list, FUN=function(outcome) { lapply(outcome, FUN=function(model) { !grepl('med*', class(model)) }) }))) ) {
    stop("Some of the models introduced are not mediation models")
  }

  for (subl in names(mediation.list)) {
    names_out <- sapply(mediation.list[[subl]],
                        FUN = function(model) {paste(model$mediator, '~', model$treat, sep=' ')})
    if (length(unique(names_out)) == length(names_out)) {
      names(mediation.list[[subl]]) <- names_out
    }
    else {
      dup_names <- names_out[duplicated(names_out)]
      message(paste('Duplicated models:', paste(dup_names, collapse=', ')))
      stop("Are you introducing the same model more than one time?")
    }
  }
  return(.med_summary_list(mediation.list))
}


.med_summary_list <- function(mediation.list) {
  lapply(mediation.list, function(outcome_models) {
    lapply(outcome_models, .mediation_summary)
  })
}


.pick_row <- function(x, primary_est, primary_ci, primary_p, fb_est, fb_ci, fb_p) {
  if (!is.null(x[[primary_est]])) {
    c(x[[primary_est]], x[[primary_ci]], x[[primary_p]])
  } else if (!is.null(x[[fb_est]])) {
    c(x[[fb_est]], x[[fb_ci]], x[[fb_p]])
  } else {
    c(NA_real_, NA_real_, NA_real_, NA_real_)
  }
}


.mediation_summary_ordinal <- function(x, clp) {
  labels <- x$model.y$lev

  groups <- list(
    "ACME (control)" = list(est = "d0",       ci = "d0.ci",     p = "d0.p"),
    "ACME (treated)" = list(est = "d1",       ci = "d1.ci",     p = "d1.p"),
    "ADE (control)"  = list(est = "z0",       ci = "z0.ci",     p = "z0.p"),
    "ADE (treated)"  = list(est = "z1",       ci = "z1.ci",     p = "z1.p"),
    "Total Effect"   = list(est = "tau.coef", ci = "tau.ci",    p = "tau.p")
  )

  rows      <- list()
  row_names <- character(0)
  for (group_name in names(groups)) {
    g   <- groups[[group_name]]
    est <- x[[g$est]]
    ci  <- x[[g$ci]]
    p   <- x[[g$p]]
    for (i in seq_along(est)) {
      rows[[length(rows) + 1]] <- c(est[i], ci[1, i], ci[2, i], p[i])
      row_names <- c(row_names, paste0(group_name, " (Pr(Y=", labels[i], "))"))
    }
  }

  stats_model <- do.call(rbind, rows)
  rownames(stats_model) <- row_names
  colnames(stats_model) <- c("Estimate",
                             paste0(clp, "% CI Lower"),
                             paste0(clp, "% CI Upper"),
                             "p-value")
  return(as.data.frame(stats_model))
}


.mediation_summary <- function(x) {
  clp <- 100 * x$conf.level

  if (inherits(x, "mediate.order")) {
    return(.mediation_summary_ordinal(x, clp))
  }

  has_split <- !isTRUE(all.equal(x$d0, x$d1))

  if (has_split) {
    rows <- stats::setNames(
      list(
        .pick_row(x, "d0",       "d0.ci",    "d0.p",    "d.avg",    "d.avg.ci", "d.avg.p"),
        .pick_row(x, "d1",       "d1.ci",    "d1.p",    "d.avg",    "d.avg.ci", "d.avg.p"),
        .pick_row(x, "z0",       "z0.ci",    "z0.p",    "z.avg",    "z.avg.ci", "z.avg.p"),
        .pick_row(x, "z1",       "z1.ci",    "z1.p",    "z.avg",    "z.avg.ci", "z.avg.p"),
        .pick_row(x, "tau.coef", "tau.ci",   "tau.p",   "tau.coef", "tau.ci",   "tau.p"),
        .pick_row(x, "n0",       "n0.ci",    "n0.p",    "n.avg",    "n.avg.ci", "n.avg.p"),
        .pick_row(x, "n1",       "n1.ci",    "n1.p",    "n.avg",    "n.avg.ci", "n.avg.p"),
        .pick_row(x, "d.avg",    "d.avg.ci", "d.avg.p", "d0",       "d0.ci",    "d0.p"),
        .pick_row(x, "z.avg",    "z.avg.ci", "z.avg.p", "z0",       "z0.ci",    "z0.p"),
        .pick_row(x, "n.avg",    "n.avg.ci", "n.avg.p", "n0",       "n0.ci",    "n0.p")
      ),
      c("ACME (control)", "ACME (treated)",
        "ADE (control)",  "ADE (treated)",
        "Total Effect",
        "Prop. Mediated (control)", "Prop. Mediated (treated)",
        "ACME (average)", "ADE (average)", "Prop. Mediated (average)")
    )
  } else {
    rows <- stats::setNames(
      list(
        .pick_row(x, "d.avg",   "d.avg.ci", "d.avg.p", "d0",       "d0.ci",  "d0.p"),
        .pick_row(x, "z.avg",   "z.avg.ci", "z.avg.p", "z0",       "z0.ci",  "z0.p"),
        .pick_row(x, "tau.coef","tau.ci",   "tau.p",   "tau.coef", "tau.ci", "tau.p"),
        .pick_row(x, "n.avg",   "n.avg.ci", "n.avg.p", "n0",       "n0.ci",  "n0.p")
      ),
      c("ACME", "ADE", "Total Effect", "Prop. Mediated")
    )
  }

  stats_model <- do.call(rbind, rows)
  colnames(stats_model) <- c("Estimate",
                             paste0(clp, "% CI Lower"),
                             paste0(clp, "% CI Upper"),
                             "p-value")
  return(as.data.frame(stats_model))
}


.filt_and_adjpval <- function(mediation_sum.list) {
  mediation_sum.df <- dplyr::bind_rows(mediation_sum.list, .id = 'outcome') %>%
    dplyr::mutate(outcome = as.factor(.data$outcome))

  # Detect which column name variant the models produced and use it throughout
  col_suffix <- if ('p-value_Prop.Mediated(average)' %in% names(mediation_sum.df)) {
    "(average)"
  } else if ('p-value_Prop.Mediated(control)' %in% names(mediation_sum.df)) {
    "(control)"
  } else if ('p-value_Prop.Mediated(treated)' %in% names(mediation_sum.df)) {
    "(treated)"
  } else {
    ""
  }
  pval_col     <- paste0('p-value_Prop.Mediated', col_suffix)
  est_acme_col <- paste0('ACME',                  col_suffix)
  est_ade_col  <- paste0('ADE',                   col_suffix)
  est_prop_col <- paste0('Prop.Mediated',          col_suffix)

  # computing adjusted p-value for all analyses (Benjamini & Hochberg)
  mediation_sum.df <- mediation_sum.df %>%
    dplyr::mutate(`adj.pval.PropMed.all` = p.adjust(.data[[pval_col]], method='BH'))

  list_format <- list()
  for (i in levels(mediation_sum.df[['outcome']])) {
    list_format[[i]] <- mediation_sum.df %>% dplyr::filter(.data$outcome == i)
    list_format[[i]][['outcome']] <- NULL
  }

  results.list <- list()
  for (out in names(list_format)) {

    results <- list_format[[out]] %>%
      mutate(names = row.names(list_format[[out]])) %>%
      tidyr::separate_wider_delim(data=., cols=names, delim=' ~ ', names=c('mediator', 'treatment')) %>%
      # computing adjusted p.value by outcome (Benjamini & Hochberg)
      mutate(`adj.pval.PropMed.by_outcome` = p.adjust(.data[[pval_col]], method='BH')) %>%
      mutate(outcome = out) %>%
      dplyr::select(dplyr::all_of(c('outcome', 'mediator', 'treatment',
                                    est_acme_col, est_ade_col, 'TotalEffect',
                                    est_prop_col, pval_col,
                                    'adj.pval.PropMed.all', 'adj.pval.PropMed.by_outcome'))) %>%
      dplyr::rename(dplyr::all_of(c('ACME'          = est_acme_col,
                                    'ADE'           = est_ade_col,
                                    'Prop.Mediated' = est_prop_col,
                                    'pval.PropMed' = pval_col)))
    results.list[[out]] <- results
  }
  return(results.list)
}

