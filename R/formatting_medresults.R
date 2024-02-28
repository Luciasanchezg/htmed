
#' @import dplyr
#' @importFrom reshape2 dcast melt
#' @importFrom tidyr separate_wider_delim
NULL

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
formatting_med <- function(mediation.list) {

  ## TODO: no está contemplada la posibilidad de covariates

  if (!"list" %in% class(mediation.list)) {
    stop("mediation.list is not a list")
  }

  if ( any(unlist(lapply(mediation.list, FUN=function(sublist) {class(sublist)})) != 'list') ) {
    stop("mediation.list is not a list of lists")
  }

  # if ( any(unlist(lapply(mediation.list, FUN=function(outcome) {lapply(outcome, FUN=function(model) {class(model)})})) != 'mediate') ) {
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
      stop("Are you introducing the same model more than one time?")
    }
  }

  onerow_summary <- .med_summary_list(mediation.list)

  filt_summary <- .filtering_summary.list(onerow_summary)

  return(filt_summary)
}
# p <- formatting_med(mediation_surv)

################################################################################
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
                                       formula = 1~variable+row.names) %>% dplyr::select(-c(`1`))
      rownames(model.reshape) <- med

      model.stats.list[[med]] <- model.reshape
    }
    model.stats.df <- do.call(rbind, model.stats.list)
    summary.list[[i]] <- model.stats.df
  }
  return(summary.list)
}


.mediation_summary <- function (x) {

  ## TODO: controlar cuando no hay control y treated, sino solo un ACME y un Prop.Mediated
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
# .mediation_summary(mediation_res$tsurvHF$`mediator.1 ~ treatment.1`)

.filtering_summary.list <- function(mediation_sum.list) {

  results.list <- list()
  for (out in names(mediation_sum.list)) {
    results <- mediation_sum.list[[out]] %>%
      mutate(names = row.names(.)) %>%
      tidyr::separate_wider_delim(data=., cols=names, delim=' ~ ', names=c('mediator', 'treatment')) %>%
      #mutate(outcome = out) %>%

      dplyr::select(c(`p-value_Prop._Mediated_(average)`,
                      `Estimate_Prop._Mediated_(average)`,
                      `Estimate_ACME_(average)`,
                      mediator, treatment)) %>%

      # Replacing new columns with NAs if p-value_Prop.Mediated_(average) < 0.05
      mutate(`p-value_Prop._Mediated_(average)` = as.numeric(`p-value_Prop._Mediated_(average)`),
             `p-value_Prop._Mediated_(average)` = replace(`p-value_Prop._Mediated_(average)`, `p-value_Prop._Mediated_(average)` > 0.05, NA)) %>%

      mutate(`Estimate_Prop._Mediated_(average)` = as.numeric(`Estimate_Prop._Mediated_(average)`),
             `Estimate_Prop._Mediated_(average)` = case_when(is.na(`p-value_Prop._Mediated_(average)`)~NA, TRUE ~ `Estimate_Prop._Mediated_(average)`)) %>%

      mutate(`Estimate_ACME_(average)` = as.numeric(`Estimate_ACME_(average)`),
             `Estimate_ACME_(average)` = case_when(is.na(`p-value_Prop._Mediated_(average)`)~NA, TRUE ~ `Estimate_ACME_(average)`)) # %>%

      # mutate_if(is.numeric, ~ na_if(., 0))

    results.list[[out]] <- results
  }
  return(results.list)
}
#.filt.mediation.summary <- .filtering_summary.list(results.mediation.summary)

