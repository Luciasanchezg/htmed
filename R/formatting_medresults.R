
#' @importFrom dplyr %>% mutate bind_rows
#' @importFrom reshape2 dcast melt
#' @importFrom tidyr separate_wider_delim
#' @importFrom stats p.adjust
#' @importFrom tibble rownames_to_column
NULL


################################################################################
# Formatting mediation models for posterior analysis
################################################################################

#' Formatting results from causal mediation analysis
#'
#' @description `formatting_med()` extracts some useful information from the
#'   causal mediation analyses. This function also computes:
#'
#'   * Adjusted p-value computed for the models in the same level.
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
#'
#' @return lists of lists with the summary of the mediate analyses and the
#'   adjusted p-values.
#' @export
#'
#'
formatting_med <- function(
    mediation.list,
    split = FALSE
) {
  ## TODO: no está contemplada la posibilidad de covariates
  if (!"logical" %in% class(split)) {
    stop("split argument only admits logical")
  }
  if (split == FALSE) {
    filt_summary <- .formatting_med(mediation.list=mediation.list)
  }
  else {
    if (purrr::pluck_depth(mediation.list) != 7) {
      stop("Using split=TRUE, the first level in the mediation.list should be the outcomes, the second, the conditions used to split and the third, the models")
    }
    filt_summary <- lapply(names(mediation.list),
                           FUN = function(subl) {
                             formatted.list <- .formatting_med(mediation.list=mediation.list[[subl]])
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


.formatting_med <- function(
    mediation.list
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
  onerow_summary <- .med_summary_list(mediation.list)
  filt_summary <- .filt_and_adjpval(onerow_summary)

  return(filt_summary)
}


.med_summary_list <- function(mediation.list) {

  summary.list <- list()
  for (i in names(mediation.list)) {

    model.stats.list <- list()
    for (med in names(mediation.list[[i]])) {

      # getting the summary for the mediation
      model.stats <- .mediation_summary(mediation.list[[i]][[med]]) %>%
        as.data.frame() %>%
        # dplyr::mutate(row.names = row.names(.)) %>%
        tibble::rownames_to_column(var = 'row.names') %>%
        dplyr::mutate(row.names = gsub(' ', '_', row.names))

      # reshaping summaries into one row
      model.reshape <- reshape2::dcast(reshape2::melt(model.stats, id.var="row.names"), formula = 1~variable+row.names) %>% dplyr::select(-c('1'))
      rownames(model.reshape) <- med

      model.stats.list[[med]] <- model.reshape
    }
    model.stats.df <- do.call(rbind, model.stats.list)
    summary.list[[i]] <- model.stats.df
  }
  return(summary.list)
}


.mediation_summary <- function (x) {
  clp <- 100 * x$conf.level

  stats_model <- c(x$d0, x$d0.ci, x$d0.p)
  stats_model <- rbind(stats_model, c(x$d1, x$d1.ci, x$d1.p))
  stats_model <- rbind(stats_model, c(x$n0, x$n0.ci, x$n0.p))
  stats_model <- rbind(stats_model, c(x$n1, x$n1.ci, x$n1.p))
  stats_model <- rbind(stats_model, c(x$d.avg, x$d.avg.ci, x$d.avg.p))
  stats_model <- rbind(stats_model, c(x$n.avg, x$n.avg.ci, x$n.avg.p))
  #stablishing rownames and colnames
  rownames(stats_model) <- c("ACME (control)", "ACME (treated)",
                             "Prop. Mediated (control)", "Prop. Mediated (treated)",
                             "ACME (average)",
                             "Prop. Mediated (average)")
  colnames(stats_model) <- c("Estimate", paste(clp, "% CI Lower", sep = ""),
                             paste(clp, "% CI Upper", sep = ""), "p-value")
  return(as.data.frame(stats_model))
}


.filt_and_adjpval <- function(mediation_sum.list) {
  # computing adjusted p-value for all analyses (Benjamini & Hochberg)
  mediation_sum.df <- dplyr::bind_rows(mediation_sum.list, .id = 'outcome') %>%
    dplyr::mutate(outcome = as.factor(.data$outcome)) %>%
    dplyr::mutate(`adj.p-value.all` = p.adjust(.data$`p-value_Prop._Mediated_(average)`, method='BH'))

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
      mutate(`adj.p-value.by_outcome` = p.adjust(.data$`p-value_Prop._Mediated_(average)`, method='BH')) %>%
      dplyr::select(c('p-value_Prop._Mediated_(average)', 'adj.p-value.all', 'adj.p-value.by_outcome',
                      'Estimate_Prop._Mediated_(average)', 'Estimate_ACME_(average)',
                      'mediator', 'treatment'))
    results.list[[out]] <- results
  }
  return(results.list)
}

