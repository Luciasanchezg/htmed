
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
#' @description `format_med()` extracts some useful information from the
#'   causal mediation analyses. This function also computes:
#'
#'   * Adjusted p-value computed for the models performed for one outcome.
#'
#'   * Adjusted p-value by all the analyses present object. In case there is
#'   just one outcome, both adjusted p-values will be the same.
#'
#' @param mediation.list list of lists with as many sublists as different
#'   outcomes has been tested in the causal mediation analysis. The first level
#'   of this list will contain the different outcomes that have been analised.
#'   The second level, will contain the mediate objects with the analyses
#'   information.
#' @param split a boolean. This argument indicate if we are dealing with
#'   splitted data. Default: FALSE.
#' @param estimate a character string indicating which estimate to report when
#'   the mediation model has a control/treated split. One of `"average"`
#'   (default), `"control"`, or `"treated"`. Ignored for models that only
#'   report a single estimate.
#'
#' @return lists of lists with the summary of the mediate analyses and the
#'   adjusted p-values.
#' @export
#'
#'
format_med <- function(
    mediation.list,
    split = FALSE,
    estimate = "average"
) {
  ## TODO: no está contemplada la posibilidad de covariates
  if (!"logical" %in% class(split)) {
    stop("split argument only admits logical")
  }
  if (!estimate %in% c("average", "control", "treated")) {
    stop("estimate must be one of: 'average', 'control', 'treated'")
  }
  if (split == FALSE) {
    filt_summary <- .format_med(mediation.list=mediation.list, estimate=estimate)
  }
  else {
    if (purrr::pluck_depth(mediation.list) != 7) {
      stop("Using split=TRUE, the first level in the mediation.list should be the outcomes, the second, the conditions used to split and the third, the models")
    }
    filt_summary <- lapply(names(mediation.list),
                           FUN = function(subl) {
                             formatted.list <- .format_med(mediation.list=mediation.list[[subl]], estimate=estimate)
                             formatted.df <- lapply(names(formatted.list),
                                                    FUN = function(subl.subl) {
                                                      formatted.list[[subl.subl]] %>% mutate(split = subl.subl)
                                                      }
                                                    )
                             do.call(rbind, formatted.df)
                             }
                           )
    names(filt_summary) <- names(mediation.list)
  }
  return(filt_summary)
}


.format_med <- function(
    mediation.list,
    estimate = "average"
    ) {

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
  onerow_summary <- .med_summary_list(mediation.list, estimate=estimate)
  filt_summary <- .filt_and_adjpval(onerow_summary)

  return(filt_summary)
}


.med_summary_list <- function(mediation.list, estimate = "average") {

  summary.list <- list()
  for (subl in names(mediation.list)) {

    model.stats.list <- list()
    for (med in names(mediation.list[[subl]])) {

      # getting the summary for the mediation
      model.stats <- .mediation_summary(mediation.list[[subl]][[med]], estimate=estimate)
      row_names   <- gsub(' ', '', rownames(model.stats))

      # reshaping summaries into one row: estimates as plain column names,
      # p-values prefixed with "p-value_" — no "Estimate_" prefix generated
      est_vec  <- stats::setNames(model.stats[["Estimate"]], row_names)
      pval_vec <- stats::setNames(model.stats[["p-value"]], paste0("p-value_", row_names))
      model.reshape <- data.frame(as.list(c(est_vec, pval_vec)), check.names = FALSE)
      rownames(model.reshape) <- med

      model.stats.list[[med]] <- model.reshape
    }
    model.stats.df <- do.call(rbind, model.stats.list)
    summary.list[[subl]] <- model.stats.df
  }
  return(summary.list)
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


.mediation_summary <- function(x, estimate = "average") {
  clp       <- 100 * x$conf.level
  has_split <- isTRUE(x$d0 != x$d1)

  if (!has_split && estimate %in% c("control", "treated")) {
    warning("estimate='", estimate, "' has no effect: this model only reports a single ",
            "estimate (no control/treated split). Ignoring the estimate argument.")
  }

  if (!has_split) {
    acme_label <- "ACME"
    ade_label  <- "ADE"
    prop_label <- "Prop. Mediated"
    acme_row   <- .pick_row(x, "d.avg", "d.avg.ci", "d.avg.p", "d0", "d0.ci", "d0.p")
    ade_row    <- .pick_row(x, "z.avg", "z.avg.ci", "z.avg.p", "z0", "z0.ci", "z0.p")
    prop_row   <- .pick_row(x, "n.avg", "n.avg.ci", "n.avg.p", "n0", "n0.ci", "n0.p")
  } else if (estimate == "control") {
    acme_label <- "ACME (control)"
    ade_label  <- "ADE (control)"
    prop_label <- "Prop. Mediated (control)"
    acme_row   <- .pick_row(x, "d0", "d0.ci", "d0.p", "d.avg", "d.avg.ci", "d.avg.p")
    ade_row    <- .pick_row(x, "z0", "z0.ci", "z0.p", "z.avg", "z.avg.ci", "z.avg.p")
    prop_row   <- .pick_row(x, "n0", "n0.ci", "n0.p", "n.avg", "n.avg.ci", "n.avg.p")
  } else if (estimate == "treated") {
    acme_label <- "ACME (treated)"
    ade_label  <- "ADE (treated)"
    prop_label <- "Prop. Mediated (treated)"
    acme_row   <- .pick_row(x, "d1", "d1.ci", "d1.p", "d.avg", "d.avg.ci", "d.avg.p")
    ade_row    <- .pick_row(x, "z1", "z1.ci", "z1.p", "z.avg", "z.avg.ci", "z.avg.p")
    prop_row   <- .pick_row(x, "n1", "n1.ci", "n1.p", "n.avg", "n.avg.ci", "n.avg.p")
  } else {
    acme_label <- "ACME (average)"
    ade_label  <- "ADE (average)"
    prop_label <- "Prop. Mediated (average)"
    acme_row   <- .pick_row(x, "d.avg", "d.avg.ci", "d.avg.p", "d0", "d0.ci", "d0.p")
    ade_row    <- .pick_row(x, "z.avg", "z.avg.ci", "z.avg.p", "z0", "z0.ci", "z0.p")
    prop_row   <- .pick_row(x, "n.avg", "n.avg.ci", "n.avg.p", "n0", "n0.ci", "n0.p")
  }

  # Total Effect is always a single estimate regardless of model type
  tau_row <- .pick_row(x, "tau.coef", "tau.ci", "tau.p", "tau.coef", "tau.ci", "tau.p")

  rows <- stats::setNames(
    list(acme_row, ade_row, tau_row, prop_row),
    c(acme_label, ade_label, "Total Effect", prop_label)
  )
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
    dplyr::mutate(`adj.p-value.all` = p.adjust(.data[[pval_col]], method='BH'))

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
      mutate(`adj.p-value.by_outcome` = p.adjust(.data[[pval_col]], method='BH')) %>%
      mutate(outcome = out) %>%
      dplyr::select(dplyr::all_of(c('outcome', 'mediator', 'treatment',
                                    est_acme_col, est_ade_col, 'TotalEffect',
                                    est_prop_col, pval_col,
                                    'adj.p-value.all', 'adj.p-value.by_outcome'))) %>%
      dplyr::rename(dplyr::all_of(c('ACME'          = est_acme_col,
                                    'ADE'           = est_ade_col,
                                    'Prop.Mediated' = est_prop_col,
                                    'p-value'       = pval_col)))
    results.list[[out]] <- results
  }
  return(results.list)
}

