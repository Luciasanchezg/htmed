
#' @importFrom stats setNames as.formula
#' @importFrom parallel mclapply
#' @importFrom rlang is_empty sym
#' @importFrom dplyr group_by filter n pull
NULL


################################################################################
# Generation of fitted models for the outcome and the mediator
################################################################################

#' Generating the fitted models for mediator OR outcome
#'
#' @description `generating_models()` generates a single dataframe with as many
#'   rows as different models to perform mediation and a column (with the fitted
#'   models for mediator OR the fitted models for outcome)
#'
#' @param column.models a character indicating the name of the column containing
#'   the fitted models for mediator
#' @param model.type a function indicating the kind of analysis that will be
#'   performed, taking into account the ones allowed by mediate()
#' @param data a dataframe with the information to perform the models
#' @param data.models a dataframe with the column indicated in column.models
#' @param model.m Default: TRUE. A boolean for choosing if we are going to
#'   perform the fitted models for mediator (TRUE) or outcome (FALSE)
#' @param outcome a string with the name of the column that contains the outcome
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

  if (any(is.na(data))) {
    message("Your data contains NA. These rows will not be taken into account for the models")
    data <- data[rowSums(is.na(data)) == 0, ]
  }

  if (!"logical" %in% class(model.m)) {
    stop("model.m argument only admits logical")
  }

  if (!as.character(column.models) %in% colnames(data.models)) {
    stop("Incorrect column name for the models")
  }

  if (!is.null(outcome)) {
    if (!"character" %in% class(outcome)) {
      stop("Please, provide the name of the outcome column as character")
    }
    if (!as.character(outcome) %in% colnames(data.models)) {
      stop("Incorrect column name for the outcome")
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
    subset.models <- data.models %>% dplyr::filter(!!rlang::sym(outcome) == out)

    results <- .one_outcome(column.models=column.models, model.type=model.type, data=data, data.models=subset.models, model_name=model_name, ncores=ncores, ...)

    results.list[[out]] <- results
  }
  results.df <- do.call(rbind, results.list)
  rownames(results.df) <- NULL
  return(results.df)
}


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
  names(models) <- list.models
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






