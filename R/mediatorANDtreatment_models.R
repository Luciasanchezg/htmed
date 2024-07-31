
utils::globalVariables(c(".env"))
#' @importFrom stats setNames as.formula
#' @importFrom parallel mclapply
#' @importFrom rlang is_empty sym
#' @importFrom dplyr group_by filter n pull mutate summarise
NULL

################################################################################
# Generation of dataframe with the models to perform
################################################################################

#' Generate the dataframe stablishing the fitted models for mediator and the
#' fitted models for outcome that need to be perform
#'
#' @param outcome a vector with the outcomes
#' @param mediator a vector with the mediators
#' @param treatment a vector with the treatments
#'
#' @return It returns a dataframe with five columns. The first two columns will
#'   include the outcomes, treatments and mediators tested, respectively. The
#'   last two columns will indicate the fitted models for mediators and outcome
#'   that need to be performed.
#' @export
#'
data_models <- function(outcome, mediator, treatment) {

  if (!is.vector(outcome)) { print('Please, provide a vector for the outcome') }
  if (!is.vector(mediator)) { print('Please, provide a vector for the mediator') }
  if (!is.vector(treatment)) { print('Please, provide a vector for the treatment') }

  models_df <- expand.grid(outcome = outcome, treatment = treatment, mediator = mediator) %>%
    mutate(model.m.formula = paste(mediator, '~', treatment)) %>%
    mutate(model.y.formula = paste(outcome, '~', mediator, '+', treatment))
  return(models_df)
}


################################################################################
# Generation of fitted models for mediators
################################################################################

#' Generating the fitted models for mediators
#'
#' @description This function generates a dataframe with the fitted models
#'   performed for the mediators. To do so, we will need two objects: * data: a
#'   dataframe with the values to perform the statistical models. * data.models:
#'   a dataframe with the information of the models that need to be performed.
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
#' @param model.name a string referring to the name of the column that will
#'   contain the fitted models for the mediators.
#' @param outcome a string. This will refer to the name of the column that
#'   contains the outcome. Run in the default mode, the function understands
#'   that all analyses share the same outcome. Default: NULL.
#' @param data.split a character indicating the column from data to split the
#'   statistical analysis. Default: NULL.
#' @param ncores numeric. It refers to the number of cores used during the
#'   mediation analyses. In the default mode, it will detect automatically the
#'   number of cores to choose. Default: NULL.
#' @param ... other arguments that models will need. Some functions, as
#'   \code{\link[stats]{glm}}, requires additional arguments, such as family,
#'   that can be specified here.
#'
#' @return returns a dataframe with a column, named model.M or model.Y,
#'   depending on the fitted models performed
#' @export
#'
mediator_models <- function(
    column.models,
    model.type,
    data,
    data.models,
    model.name,
    outcome = NULL,
    data.split = NULL,
    ncores = NULL,
    ...
) {
  message("Performing fitted models for mediators")
  results <- .generate_models(column.models=column.models, model.type=model.type, data=data, data.models=data.models, model.name=model.name, outcome=outcome, data.split=data.split, ncores=ncores, ...)
}



################################################################################
# Generation of fitted models for outcomes
################################################################################

#' Generating the fitted models for outcomes
#'
#' @description This function generates a dataframe with the fitted models
#'   performed for the outcomes. To do so, we will need two objects: * data: a
#'   dataframe with the values to perform the statistical models. * data.models:
#'   a dataframe with the information of the models that need to be performed.
#'
#' @param column.models a character indicating the name of the column containing
#'   the fitted models for outcomes OR outcomes.
#' @param model.type a function indicating the kind of analysis that will be
#'   performed, taking into account the ones allowed by
#'   \code{\link[mediation]{mediate}}.
#' @param data a dataframe with the information to perform the statistical
#'   models.
#' @param data.models a dataframe with the column specified in column.models.
#'   This will contain the formulas for the models as characters.
#' @param model.name a string referring to the name of the column that will
#'   contain the fitted models for the outcomes.
#' @param outcome a string. This will refer to the name of the column that
#'   contains the outcome. Run in the default mode, the function understands
#'   that all analyses share the same outcome. Default: NULL.
#' @param data.split a character indicating the column from data to split the
#'   statistical analysis. Default: NULL.
#' @param ncores numeric. It refers to the number of cores used during the
#'   mediation analyses. In the default mode, it will detect automatically the
#'   number of cores to choose. Default: NULL.
#' @param ... other arguments that models will need. Some functions, as
#'   \code{\link[stats]{glm}}, requires additional arguments, such as family,
#'   that can be specified here.
#'
#' @return returns a dataframe with a column, named model.M or model.Y,
#'   depending on the fitted models performed
#' @export
#'
outcome_models <- function(
    column.models,
    model.type,
    data,
    data.models,
    model.name,
    outcome = NULL,
    data.split = NULL,
    ncores = NULL,
    ...
) {
  message("Performing fitted models for outcomes")
  results <- .generate_models(column.models=column.models, model.type=model.type, data=data, data.models=data.models, model.name=model.name, outcome=outcome, data.split=data.split, ncores=ncores, ...)
}



.generate_models <- function(
    column.models,
    model.type,
    data,
    data.models,
    model.name,
    outcome = NULL,
    data.split = NULL,
    ncores = NULL,
    ...
) {
  ## TODO: eval(parse(text="lm(M ~ I + gender, data=df)"))

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

  if (!"character" %in% class(model.name)) {
    stop("model.name is not a character")
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

  if (model.name %in% colnames(data.models)) {
    stop(paste("You are performing fitted models, but the column", model.name, "already exists", sep=' '))
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

  # checking if the models can be converted in a formula
  models <- .check_formula(column.models=column.models, data.models=data.models)
  if (is.null(models)) {
    stop("There are no right formulas in the columns selected")
  }
  data.models <- data.models[data.models[[column.models]] %in% as.character(models),]

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
      data.subs <- data %>% dplyr::filter(!!rlang::sym(data.split) == .env$split)
      # subsetting data.models
      data.models.subs <- data.models %>% dplyr::filter(!!rlang::sym(data.split) == .env$split)

      # performing models
      if (!is.null(outcome)) {

        results.subs <- .more_outcomes(column.models=column.models, model.type=model.type, data=data.subs, data.models=data.models.subs, model.name=model.name, ncores=ncores, outcome=outcome)
        results.subs[[data.split]] <- split
        results <- rbind(results, results.subs)
      }
      else {
        results.subs <- .one_outcome(column.models=column.models, model.type=model.type, data=data.subs, data.models=data.models.subs, model.name=model.name, ncores=ncores)
        results.subs[[data.split]] <- split
        results <- rbind(results, results.subs)
      }
    }
  }
  else {
    # performing models
    if (!is.null(outcome)) {
      results <- .more_outcomes(column.models=column.models, model.type=model.type, data=data, data.models=data.models, model.name=model.name, ncores=ncores, outcome=outcome, ...)
    }
    else {
      results <- .one_outcome(column.models=column.models, model.type=model.type, data=data, data.models=data.models, model.name=model.name, ncores=ncores, ...)
    }
  }
  return(results)
}



.one_outcome <- function(
    column.models,
    model.type,
    data,
    data.models,
    model.name,
    ncores,
    ...
) {
  # finding duplicate models
  dup_mods <- data.models %>% group_by(!!rlang::sym(column.models)) %>% summarise(n=n()) %>% filter(n>1) %>% pull(!!rlang::sym(column.models)) %>% unique
  if (rlang::is_empty(dup_mods) == FALSE) { stop("Some models are duplicated") }
  # generating the models
  models <- .model_MY(list.models=data.models[[column.models]], model.type=model.type, data=data, ncores=ncores, ...)

  results.models <- stats::setNames(data.frame(matrix(ncol = 1, nrow = length(data.models[[column.models]]))), c(model.name))
  results.models[[model.name]] <- models
  rownames(results.models) <- names(models)

  results <- merge(data.models, results.models, by.x=column.models, by.y ='row.names')
  return(results)
}


.more_outcomes <- function(
    column.models,
    model.type,
    data,
    data.models,
    model.name,
    ncores,
    outcome,
    ...
) {
  results.list <- list()
  for (out in levels(data.models[[outcome]])) {
    subset.models <- data.models %>% dplyr::filter(!!rlang::sym(outcome) == .env$out)
    results <- .one_outcome(column.models=column.models, model.type=model.type, data=data, data.models=subset.models, model.name=model.name, ncores=ncores, ...)
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

