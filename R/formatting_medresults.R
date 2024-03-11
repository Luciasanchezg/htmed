
#' @import dplyr
#' @importFrom reshape2 dcast melt
#' @importFrom tidyr separate_wider_delim
#' @impo stats p.adjust
NULL


################################################################################
# Formatting mediation models for posterior analysis
################################################################################

#' Formatting results from causal mediation analysis
#'
#' @description `formatting_med()` extracts the information from the causal
#'   mediation analysis to filter statistically significant mediation results
#' @param mediation.list list of lists, with as many sublists as different
#'   outcomes has been tested in the causal mediation analysis.
#'
#' @return lists of lists with the mediate analysis filtered
#' @export
#'
formatting_med <- function(
    mediation.list
    ) {
  ## TODO: no está contemplada la posibilidad de covariates

  if (!"list" %in% class(mediation.list)) {
    stop("mediation.list is not a list")
  }

  if ( any(unlist(lapply(mediation.list, FUN=function(sublist) { class(sublist) })) != 'list') ) {
    stop("mediation.list is not a list of lists")
  }

  if ( any(unlist(lapply(mediation.list, FUN=function(outcome) { lapply(outcome, FUN=function(model) { !grepl('med*', class(model)) }) }))) ) {
    stop("Some of the models introduced are not mediation models")
  }

  for (out in names(mediation.list)) {
    names_out <- sapply(mediation.list[[out]],
                        FUN = function(model) {paste(model$mediator, '~', model$treat, sep=' ')})
    if (length(unique(names_out)) == length(names_out)) {
      names(mediation.list[[out]]) <- names_out
    }
    else {
      dup_names <- names_out[duplicated(names_out)]
      message(paste('Duplicated models:', paste(dup_names, collapse=', ')))
      stop("Are you introducing the same model more than one time?")
    }
  }

  onerow_summary <- .med_summary_list(mediation.list)
  filt_summary <- .filtering_summary.list(onerow_summary)

  return(filt_summary)
}


.med_summary_list <- function(mediation.list) {

  summary.list <- list()
  for (i in names(mediation.list)) {

    model.stats.list <- list()
    for (med in names(mediation.list[[i]])) {

      # getting the summary for the mediation
      model.stats <- .mediation_summary(mediation.list[[i]][[med]]) %>%
        as.data.frame(.) %>%
        mutate(row.names = row.names(.)) %>%
        mutate(row.names = gsub(' ', '_', row.names))

      # reshaping summaries into one row
      model.reshape <- reshape2::dcast(reshape2::melt(model.stats, id.var="row.names"),
                                       formula = 1~variable+row.names) %>% dplyr::select(-c('1'))
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

  rownames(stats_model) <- c("ACME (control)", "ACME (treated)",
                             "Prop. Mediated (control)", "Prop. Mediated (treated)",
                             "ACME (average)",
                             "Prop. Mediated (average)")

  colnames(stats_model) <- c("Estimate", paste(clp, "% CI Lower", sep = ""),
                             paste(clp, "% CI Upper", sep = ""), "p-value")

  return(as.data.frame(stats_model))
}


.filtering_summary.list <- function(mediation_sum.list) {

  results.list <- list()
  for (out in names(mediation_sum.list)) {

    results <- mediation_sum.list[[out]] %>%
      mutate(names = row.names(mediation_sum.list[[out]])) %>%
      tidyr::separate_wider_delim(data=., cols=names, delim=' ~ ', names=c('mediator', 'treatment')) %>%
      #mutate(outcome = out) %>%

      # computing adjusted p.value (Benjamini & Hochberg)
      mutate(`adj.p-value_Prop._Mediated_(average)` = p.adjust (`p-value_Prop._Mediated_(average)`, method='BH')) %>%

      dplyr::select(c('p-value_Prop._Mediated_(average)',
                      'adj.p-value_Prop._Mediated_(average)',
                      'Estimate_Prop._Mediated_(average)',
                      'Estimate_ACME_(average)',
                      'mediator', 'treatment'))

    results.list[[out]] <- results

  }
  return(results.list)
}

