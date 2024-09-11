
utils::globalVariables(c(".env"))
#' @importFrom dplyr %>% filter
#' @importFrom mediation mediate
#' @importFrom parallel mcmapply detectCores
NULL


################################################################################
# Generation of mediation models
################################################################################

#' High-throughput causal mediation analysis
#'
#' @description `htmed()` allows to perform high-throughput causal mediation
#'   analysis, using the \code{\link[mediation]{mediate}} for each model.
#'
#' @param data.models a dataframe with, at least, five columns: for the
#'   treatments, mediators, outcomes, fitted models for treatments and
#'   mediators, respectively.
#' @param column.modelm a character. Name of the column with the fitted models
#'   for mediators.
#' @param column.modely a character. Name of the column with the fitted models
#'   for outcomes.
#' @param treat a character. Name of the column that contains the treatments.
#' @param mediator a character. Name of the column that contains the mediators.
#' @param outcome a character. Name of the column that contains the outcomes.
#' @param seed integer to set a seed (for reproducibility). Default: NULL
#' @param data.split a character indicating the column from data.models to split
#'   the mediation analysis. Default: NULL.
#' @param ncores numeric. It refers to the number of cores used during the
#'   mediation analyses. In the default mode, it will detect automatically the
#'   number of cores to choose. Default: NULL.
#' @param ... other arguments passed to \code{\link[mediation]{mediate}}
#'   function.
#'
#' @return returns a list of lists with the results of mediation for each
#'   combination of outcome, mediator and treatment variables.
#'
#' @export
#'
htmed <- function(
    data.models,
    column.modelm,
    column.modely,
    treat,
    mediator,
    outcome,
    seed = NULL,
    data.split = NULL,
    ncores = NULL,
    ...
    ) {
  ## TODO:
  ## Posibilidad de ajuste (multiple linear regression: GENDER)

  if (!is.character(c(treat, mediator, outcome, column.modelm, column.modely))) {
    stop("Please, provide the name of the corresponding columns as characters")
  }

  if ((!"numeric" %in% class(seed)) & (!is.null(seed))) {
    stop("Seed must be numeric or not provided")
  }

  if (!"data.frame" %in% class(data.models)) {
    stop("data.models must contain a DataFrame")
  }

  if (!all(c(treat, mediator, outcome) %in% colnames(data.models))) {
    stop("Wrong column names for treat, mediator or outcome")
  }

  if (!all(c(column.modelm, column.modely) %in% colnames(data.models))) {
    stop("Wrong column names for fitted models for mediator or outcome")
  }

  if ( all(grepl(pattern = 'Warning|Error', x = data.models[[column.modely]])) || all(grepl(pattern = 'Warning|Error', x = data.models[[column.modelm]])) ) {
    stop("All models for the outcome or the mediator contain warnings or errors")
  }

  if ( any(grepl(pattern = 'Warning|Error', x = data.models[[column.modelm]])) ) {
    message("Some models for the mediator contain warnings or errors. These rows will be removed")

    data.models <- data.models[-grepl(pattern = 'Warning', x = data.models[[column.modelm]]),]
  }

  if ( any(grepl(pattern = 'Warning|Error', x = data.models[[column.modely]])) ) {
    message("Some models for the outcome contain warnings or errors. These rows will be removed")

    data.models <- data.models[!grepl(pattern = 'Warning', x = data.models[[column.modely]]),]
  }

  if (!is.null(ncores)) {
    if (!"numeric" %in% class(ncores)) {
      stop("ncores must be numeric if provided")
    }
    else {
      ncores <- ncores
      message(paste0("Number of cores provided by the user: ", ncores))
    }
  }
  else {
    # getting the number of cores available
    ncores <- .ncores()
  }

  # Applying mediation
  results.med <- list()
  for (out in levels(data.models[[outcome]])) {

    data.models.subs <- data.models %>% dplyr::filter(!!rlang::sym(outcome) == .env$out)

    treat.subset <- data.models.subs[[treat]]
    mediator.subset <- data.models.subs[[mediator]]

    models.m <- data.models.subs[[column.modelm]]
    models.y <- data.models.subs[[column.modely]]

    if (!is.null(data.split)) {
      if (!"character" %in% class(data.split)) {
        stop("data.split is not a character")
      }
      if (!as.character(data.split) %in% colnames(data.models.subs)) {
        stop("data.split argument is not in data")
      }
      tosplit <- data.models %>% dplyr::select(!!rlang::sym(data.split)) %>% pull(!!rlang::sym(data.split)) %>% unique()
      for (split in tosplit) {
        data.models.subs.spl <- data.models.subs %>% dplyr::filter(!!rlang::sym(data.split) == .env$split)

        treat.subset <- data.models.subs.spl[[treat]]
        mediator.subset <- data.models.subs.spl[[mediator]]

        models.m <- data.models.subs.spl[[column.modelm]]
        models.y <- data.models.subs.spl[[column.modely]]

        results.med[[out]][[split]] <- .mediationHT(models.m=models.m, models.y=models.y,
                                                    treat=treat.subset, mediator=mediator.subset,
                                                    ncores=ncores, seed=seed, ...)
      }
    }
    else {
      results.med[[out]] <- .mediationHT(models.m=models.m, models.y=models.y,
                                         treat=treat.subset, mediator=mediator.subset,
                                         ncores=ncores, seed=seed, ...)
    }
  }
  return(results.med)
}


.mediationHT <- function(
    models.m,
    models.y,
    treat,
    mediator,
    ncores,
    seed,
    ...
    ) {
  results.med <- parallel::mcmapply(models.m, models.y, treat, mediator,

                          FUN = function(m, y, tr, me) {
                            # mediation
                            tryCatch(
                              {
                                set.seed(seed)
                                #model <- mediation::mediate(model.m = m, model.y = y, treat = as.character(tr), mediator = as.character(me), ...)
                                model <- mediation::mediate(model.m = m, model.y = y, treat = tr, mediator = me, ...)
                                return(model)
                              },
                              warning=function(w) { return(paste("Warning message:", w, sep=' ')) },
                              error=function(e) { return(paste("Error message:", e, sep=' ')) }
                              )
                            }, mc.cores = ncores, SIMPLIFY = FALSE)
  return(results.med)
}


.ncores <- function(...) {
  if(.Platform$OS.type == "windows")
  {
    print("Windows does not support mcl parallelisation. Setting conversion on a single CPU. This will take much more time")
    ncores <- 1
  } else
  {
    ncores <- min(c(1000,parallel::detectCores()-1))
  }
  message(paste0("Number of cores that will be used: ", ncores))
  return(ncores)
}

