
#' @importFrom stats setNames as.formula
#' @importFrom parallel mclapply
#' @importFrom rlang is_empty sym
#' @importFrom dplyr group_by filter n pull mutate summarise
NULL


################################################################################
# Generation of fitted models for the outcome and the mediator
################################################################################

#' Generating the fitted models for mediators OR outcomes
#'
#' @description This function generates a dataframe with the fitted models
#'   performed for the mediators or the outcomes (depending on the input
#'   provided). To do so, we will need two objects: * data: a dataframe with the
#'   values to perform the statistical models. * data.models: a dataframe with
#'   the information of the models that need to be performed.
#'
#' @param column.models a character indicating the name of the column containing
#'   the fitted models for mediators OR outcomes.
#' @param model.type a function indicating the kind of analysis that will be
#'   performed, taking into account the ones allowed by
#'   \code{\link[mediation]{mediate}}.
#' @param data a dataframe with the information to perform the statistical
#'   models.
#' @param data.models a dataframe with the column specified in column.models.
#'   This will contain the formulas for the models as characters.
#' @param model.m a boolean for choosing if we are going to perform the fitted
#'   models for mediator (TRUE) or outcome (FALSE). Default: TRUE
#' @param outcome a string. This will refer to the name of the column that
#'   contains the outcome. Run in the default mode, the function understands
#'   that all analyses share the same outcome. Default: NULL.
#' @param ... other arguments that models will need. Some functions, as
#'   \code{\link[stats]{glm}}, requires additional arguments, such as family,
#'   that can be specified here.
#' @param data.split a character indicating the column from data to split the
#'   statistical analysis. Default: NULL.
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
    data.split = NULL,
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

  if ((model.m == TRUE) & ('model.M' %in% colnames(data.models))) {
    stop("You are performing fitted models for mediator, but the column model.M already exists")
  }

  if ((model.m == FALSE) & ('model.Y' %in% colnames(data.models))) {
    stop("You are performing fitted models for outcome, but the column model.Y already exists")
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

  if (!is.null(data.split)) {
    if (!"character" %in% class(data.split)) {
      stop("data.split is not a character")
    }
    if (!as.character(data.split) %in% colnames(data)) {
      stop("data.split argument is not in data")
    }
    tosplit <- data %>% dplyr::select(!!rlang::sym(data.split)) %>% pull(!!rlang::sym(data.split)) %>% unique()

    results <- data.frame()
    for (split in tosplit) {
      data.subs <- df %>% dplyr::filter(!!rlang::sym(data.split) == .env$split)
      # subsetting data.models
      data.models.subs <- data.models %>% dplyr::filter(!!rlang::sym(data.split) == .env$split)

      # performing models
      if (!is.null(outcome)) {

        results.subs <- .more_outcomes(column.models=column.models, model.type=model.type, data=data.subs, data.models=data.models.subs, model_name=model_name, ncores=ncores, outcome=outcome)
        results.subs[[data.split]] <- split
        results <- rbind(results, results.subs)
      }
      else {
        results.subs <- .one_outcome(column.models=column.models, model.type=model.type, data=data.subs, data.models=data.models.subs, model_name=model_name, ncores=ncores)
        results.subs[[data.split]] <- split
        results <- rbind(results, results.subs)
      }
    }
  }
  else {
    # performing models
    if (!is.null(outcome)) {
      results <- .more_outcomes(column.models=column.models, model.type=model.type, data=data, data.models=data.models, model_name=model_name, ncores=ncores, outcome=outcome, ...)
    }
    else {
      results <- .one_outcome(column.models=column.models, model.type=model.type, data=data, data.models=data.models, model_name=model_name, ncores=ncores, ...)
    }
  }
  return(results)
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
  # finding duplicate models
  dup_mods <- data.models %>% group_by(!!rlang::sym(column.models)) %>% summarise(n=n()) %>% filter(n>1) %>% pull(!!rlang::sym(column.models)) %>% unique
  if (rlang::is_empty(dup_mods) == FALSE) { stop("Some models are duplicated") }
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
    subset.models <- data.models %>% dplyr::filter(!!rlang::sym(outcome) == .env$out)
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






