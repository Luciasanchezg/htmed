

#' High-throughput causal mediation analysis
#'
#' @description `hightmed()` allows to perform high-throughput causal mediation
#'   analysis, using the mediate package to perform the mediation analysis. This
#'   function has to different functionalities, depending on the input provided:
#'
#'   * Performing the mediation analysis, given the fitted models for mediator
#'   and outcome, respectively.
#'
#'   * Performing the fitted models for mediator and outcome. After that,
#'   mediation analysis will be also performed.
#'
#' @param model.generator Default: FALSE. Boolean indicating if models for
#'   mediator and outcome need to be performed (TRUE) or they will be provided
#'   by the user (FALSE)
#' @param sims number of Monte Carlo draws for nonparametric bootstrap or
#'   quasi-Bayesian approximation. Default: 1000
#' @param model.m.type a character indicating the fitted models for mediator
#'   that are needed to be done in case model.m argument is not provided. Only
#'   lm and survreg models are implemented.
#' @param model.y.type a character indicating the fitted models for outcome that
#'   are needed to be done in case model.y argument is not provided. Only lm and
#'   survreg models are implemented.
#' @param data a dataframe with the values of the outcome (dependent), the
#'   mediator and the mediator and the treatment (independent) variable
#' @param data.models a dataframe with, at least, three columns: treatment,
#'   mediator and outcome. If model.generator=FALSE. This dataframe needs to
#'   have two aditional columns, with the models for the mediator (model.M) and
#'   the models for the outcome (model.Y)
#' @param treat a list indicating the name of the treatment variables used in
#'   the models. The treatment can be either binary (integer or a two-valued
#'   factor) or continuous (numeric).
#' @param mediator a list indicating the name of the mediator variables used in
#'   the models.
#' @param dependent a list indicating the name of the outcome variables used in
#'   the models.
#' @param ... other arguments passed to mediate package.
#'
#' @return returns a list of lists with the results of mediation for each combination of
#'   outcome, mediator and treatment variables
#' @export
#'
hightmed <- function(sims = 1000
                     , data.models
                     , column.modelm
                     , column.modely
                     , treat
                     , mediator
                     , outcome
                     # , adjust = NULL
                     , ...) {

  ## TODO:
  ## controlar que los modelos que meto en model.m o model.y sean los soportados por mediate (esto lo hace el paquete mediate?)
  ## Introducir posibilidad de que no realice los modelos de mediación, sino que formatee los modelos (para la visualización posterior)
  ## Posibilidad de ajuste (multiple linear regression: GENDER)
  ## Posibilidad de generar subgrupos (CIH, Ctl, ...)
  ## hacer export/library/require de mediate ¿dónde?

  # getting the number of cores available
  if(.Platform$OS.type == "windows")
    {
    print("Windows does not support mcl parallelisation. Setting conversion on a single CPU. This will take much more time")
    ncores <- 1
    } else
      {
    ncores <- min(c(1000,parallel::detectCores()-1))
      }
  print(paste0("Number of cores that will be used: ", ncores))

  if (!all(c(treat, mediator, outcome) %in% colnames(data.models))) {
    stop("Wrong column names for treat, mediator or outcome")
  }

  if (!all(c(column.modelm, column.modely) %in% colnames(data.models))) {
    stop("Wrong column names for fitted models for mediator or outcome")
  }

  results.med <- list()
  for (i in levels(data.models[[outcome]])) {

    data.models.subset <- data.models %>% filter(get(outcome) == i)

    treat.subset <- data.models.subset[[treat]]
    mediator.subset <- data.models.subset[[mediator]]
    outcome.subset <- data.models.subset[[outcome]]

    models.m <- data.models.subset[[column.modelm]]
    models.y <- data.models.subset[[column.modely]]

    results.med[[i]] <- .mediationHT(models.m=models.m, models.y=models.y,
                                treat=treat.subset, mediator=mediator.subset, outcome=outcome.subset,
                                sims=sims, ncores=ncores)
  }

  return(results.med)
}

# q <- hightmed(data.models=results,
#               column.modelm='model.M', column.modely='model.Y',
#               treat = "independent.var", mediator = "mediator.var", outcome = "dependent.var")



################################################################################

.mediationHT <- function(models.m, models.y, treat, mediator, outcome, ncores, sims) {

  results.med <- parallel::mcmapply(models.m, models.y, treat, mediator,

                          FUN = function(m, y, tr, me) {
                            # mediation
                            tryCatch(
                              {
                                model <- mediation::mediate(model.m = m, model.y = y, treat = as.character(tr), mediator = as.character(me), sims = sims)
                                #stats.model <- extract_mediation_summary(summary(model))
                                return(model)
                              },
                              warning=function(w) { return(paste("Warning message:", w, sep=' ')) },
                              error=function(e) { return(paste("Error message:", e, sep=' ')) }
                              )
                            }, mc.cores = ncores, SIMPLIFY = FALSE)
  return(results.med)
}

