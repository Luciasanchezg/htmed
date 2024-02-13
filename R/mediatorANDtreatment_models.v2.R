
#' @importFrom stats setNames as.formula
#' @importFrom tibble column_to_rownames
#' @importFrom parallel mclapply
NULL

#' Merging the fitted models for mediator and outcome
#'
#' @description `providing_models()` generates a single dataframe with as many
#'   rows as different models to perform mediation and two columns (one with the
#'   fitted models for mediator and another with fitted models for outcome)
#'
#' @param model.m a list with the fitted models for mediator
#' @param model.y a list with the fitted models for outcome
#'
#' @return returns a dataframe with two different columns
#'
#'   * model.M: column with the fitted models for mediator
#'
#'   * model.Y: column with the fitted models for outcome
#' @export
#'
providing_models <- function(model.m, model.y) {

  if (length(model.m) != length(model.y)) {
    stop("The fitted models for mediator and treatment does not have the same length")
  }

  # model.m
  model_names.m <- .extracting_terms(model.m)

  model.m.df <- stats::setNames(data.frame(matrix(ncol = 1, nrow = length(model.m))), c('model.M'))
  model.m.df$model.M <- model.m
  rownames(model.m.df) <- model_names.m

  # model.y
  model_names.y <- .extracting_terms(model.y)

  model.y.df <- stats::setNames(data.frame(matrix(ncol = 1, nrow = length(model.y))), c('model.Y'))
  model.y.df$model.Y <- model.y
  rownames(model.y.df) <- model_names.y

  # merging results from fitted models for mediator and outcome in a single dataframe
  results.models <- merge(model.m.df, model.y.df, by=0) %>% tibble::column_to_rownames(var='Row.names')
  return(results.models)
}


################################################################################
#' Generating the fitted models for mediator OR outcome
#'
#' @description `generating_models()` generates a single dataframe with as many
#'   rows as different models to perform mediation and a column (with the fitted
#'   models for mediator OR the fitted models for outcome)
#' @param column.models a character indicating the name of the column containing
#'   the fitted models for mediator
#' @param model.type a function indicating the kind of analysis that will be
#'   performed, taking into account the ones allowed by mediate()
#' @param data a dataframe with the information to perform the models
#' @param data.models a dataframe with the column indicated in column.models
#' @param model.m Default: TRUE. A boolean for choosing if we are going to
#'   perform the fitted models for mediator (TRUE) or outcome (FALSE)
#' @param ... other arguments that the models performed will need
#'
#' @return returns a dataframe with a column, named model.M or model.Y,
#'   depending on the fitted models performed
#' @export
#'
generating_models <- function(column.models, model.type, data, data.models, model.m = TRUE, ...) {

  # getting the number of cores available
  ncores <- .ncores()

  if (!as.character(column.models) %in% colnames(data.models)) {
    stop("Incorrect column name for the models")
  }

  if (!is.data.frame(data)) {
    stop("Your data is not stored in a dataframe")
  }

  # checking if the models can be converted in a formula
  models <- .check_formula(column.models=column.models, data.models=data.models)

  if (is.null(models)) {
    stop("There are no right formulas in the columns selected")
  }

  data.models <- data.models[data.models[[column.models]] %in% as.character(models),]

  if (model.m == TRUE) {
    model_name <- 'model.M'
    message("Performing fitted models for mediator")
    }
  else {
    model_name <- 'model.Y'
    message("Performing fitted models for outcome")
    }

  # generating the models

  models <- .model_MY(list.models=data.models[[column.models]], model.type=model.type, data=data, ncores=ncores, ...)
  models[grep(x = names(models), pattern = 'Error') ] <- NULL

  if (length(models) == 0) {
    stop("All analysis performed gave an error")
  }

  results.models <- stats::setNames(data.frame(matrix(ncol = 1, nrow = length(data.models[[column.models]]))), c(model_name))
  results.models[[model_name]] <- models
  rownames(results.models) <- names(models)

  results <- merge(data.models, results.models, by.x=column.models, by.y ='row.names')

  return(results)

}


# results <- generating_models(column.models='model.m.formula', model.type=lm,
#                   data=df, data.models=models, model.m = TRUE)
# library(survival)
# results <- generating_models(column.models='model.y.formula', model.type=survreg,
#                              data=df, data.models=results, model.m = FALSE)

################################################################################
.check_formula <- function(column.models, data.models) {
  models <- c()
  for (model in data.models[[column.models]]) {

    tryCatch(
      {
        m <- stats::as.formula(model)
        models <- c(m, models)
      }
      , warning=function(w) {
        return(w)
      },
      error=function(e) {
        return(e)
      }
    )
  }
  return(models)
}


.model_MY <- function(list.models, model.type, data, ncores, ...) {

  # parallelizing model generation
  models <- parallel::mclapply(list.models, function(formula) {
    tryCatch(
      {
        model <-  model.type(as.formula(formula), data=data, ...)
      }
      # , warning=function(w) {
      #   #print(w)
      #   return(paste("Warning message:", w, sep=' '))
      # },
      # error=function(e) {
      #   #print(e)
      #   return(paste("Error message:", e, sep=' '))
      # }
    )

  }, mc.cores = ncores)

  # extracting the formula performed for each model
  model_names <- .extracting_terms(models)
  names(models) <- model_names

  return(models)
}


.extracting_terms <- function(models) {
  sapply(models, function(x) {
    if ( any(grepl('Error', x)) ){
      message("The model introduced has given an error")
      mod_name <- 'Error'
    }
    else {
      terms_model <- as.character(x[['terms']])
      mod_name <- paste(terms_model[2], terms_model[1], terms_model[3])
    }
  })
}

