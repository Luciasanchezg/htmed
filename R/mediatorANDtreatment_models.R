
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
  ## TODO: no estoy implementando esta función de momento

  if (length(model.m) != length(model.y)) {
    stop("The fitted models for mediator and treatment does not have the same length")
  }

  if (!setequal(names(model.m),names(model.y))) {
    stop("Provide the same names in both lists")
  }

  # model.m
  #model_names.m <- .extracting_terms(model.m)

  model.m.df <- stats::setNames(data.frame(matrix(ncol = 1, nrow = length(model.m))), c('model.M'))
  model.m.df$model.M <- model.m
  rownames(model.m.df) <- names(model.m)

  # model.y
  #model_names.y <- .extracting_terms(model.y)

  model.y.df <- stats::setNames(data.frame(matrix(ncol = 1, nrow = length(model.y))), c('model.Y'))
  model.y.df$model.Y <- model.y
  rownames(model.y.df) <- names(model.y)

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
generating_models <- function(
    column.models,
    model.type,
    data,
    data.models,
    model.m = TRUE,
    outcome = NULL,
    ...
    ) {
  ## TODO: eval(parse(text="lm(M ~ I + gender, data=df)"))
  ## controlar error todos los modelos (porque no converja, no porque los parámetros iniciales estén errados)
  ## ¿Debo controlar si le introduzco los modelos que únicamente mediate permite? ¿Debo tener estos paquetes en DESCRIPTION de hightmed?

  # getting the number of cores available
  ncores <- .ncores()

  # checking input
  if (!"character" %in% class(column.models)) {
    stop("Please, provide the name of the corresponding column as character")
  }

  if (!"function" %in% class(model.type)) {
    stop("model.type is not a function")
  }

  if ((!"data.frame" %in% class(data)) | (!"data.frame" %in% class(data.models))) {
    stop("Your data or data.models are not stored in a DataFrame")
  }

  if (!"logical" %in% class(model.m)) {
    stop("model.m argument only admits logical")
  }

  if (!as.character(column.models) %in% colnames(data.models)) {
    stop("Incorrect column name for the models")
  }

  if (!is.null(outcome)) {
    if (!"character" %in% class(outcome)) {
      stop("Please, provide the name of the treatment column as character")
    }
    if (!as.character(outcome) %in% colnames(data.models)) {
      stop("Incorrect column name for the treatment")
    }
  }

  if (is.null(outcome)) {
    dup_mods <- data.models %>%
      group_by(!!rlang::sym(column.models)) %>%
      filter(n()>1) %>%
      pull(!!rlang::sym(column.models)) %>% unique

    if (rlang::is_empty(dup_mods) == FALSE) { stop("Some models are duplicated") }
  } else {
    dup_mods <- data.models %>%
      #group_by(get('column.models'), get('outcome')) %>%
      group_by(!!rlang::sym(outcome), !!rlang::sym(column.models)) %>%
      filter(n()>1) %>%
      pull(!!rlang::sym(column.models)) %>% unique
    if (rlang::is_empty(dup_mods) == FALSE) { stop("Some models are duplicated") }
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

  if (!is.null(outcome)) {
    results <- .more_outcomes(column.models=column.models, model.type=model.type, data=data, data.models=data.models, model_name=model_name, ncores=ncores, outcome=outcome, ...)
    return(results)
    }
  else {
    results <- .one_outcome(column.models=column.models, model.type=model.type, data=data, data.models=data.models, model_name=model_name, ncores=ncores, ...)
    return(results)
  }
}

################################################################################
.one_outcome <- function(
    column.models,
    model.type,
    data,
    data.models,
    model_name,
    ncores,
    ...
    ) {

  # generating the models
  models <- .model_MY(list.models=data.models[[column.models]], model.type=model.type, data=data, ncores=ncores, ...)

  results.models <- stats::setNames(data.frame(matrix(ncol = 1, nrow = length(data.models[[column.models]]))), c(model_name))
  results.models[[model_name]] <- models
  rownames(results.models) <- names(models)

  results <- merge(data.models, results.models, by.x=column.models, by.y ='row.names')

  return(results)
}

.more_outcomes <- function(
    column.models,
    model.type,
    data,
    data.models,
    model_name,
    ncores,
    outcome,
    ...
    ) {

  results.list <- list()
  for (out in levels(data.models[[outcome]])) {
    subset.models <- data.models %>% dplyr::filter(get('outcome') == out)

    results <- .one_outcome(column.models=column.models, model.type=model.type, data=data, data.models=subset.models, model_name=model_name, ncores=ncores, ...)

    results.list[[out]] <- results
  }
  return(results.list)
}

################################################################################
.check_formula <- function(
    column.models,
    data.models
    ) {
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


.modeling <- function(
    model.type=model.type,
    formula=formula,
    data=data,
    ...
    ) {
  model <- tryCatch(
    {
      model.type(as.formula(formula), data=data, ...)
    },
    warning=function(w) {
      return(paste('Warning message: ', w$message))
    },
    error=function(e) {
      return(paste('Error message: ', e$message))
    }
  )
}


.model_MY <- function(
    list.models,
    model.type,
    data,
    ncores,
    ...
    ) {

  # parallelizing model generation
  models <- tryCatch(
    {
      parallel::mclapply(list.models, function(formula) {

        .modeling(model.type=model.type, formula=formula, data=data, ...)

      }, mc.cores = ncores)
    }
  )

  # extracting the formula performed for each model
  # model_names <- .extracting_terms(models)
  # names(models) <- model_names
  names(models) <- list.models

  return(models)
}



# .extracting_terms <- function(models) {
#   sapply(models, function(x) {
#     if ( any(grepl('Error', x)) ){
#       message("The model introduced has given an error. This row will be removed")
#       mod_name <- 'Error'
#     }
#     else if ( any(grepl('Warning', x)) ){
#       message("The model introduced has given a warning. This row will be removed")
#       mod_name <- 'Warning'
#     }
#     else {
#       terms_model <- as.character(x[['terms']])
#       mod_name <- paste(terms_model[2], terms_model[1], terms_model[3])
#     }
#   })
# }

