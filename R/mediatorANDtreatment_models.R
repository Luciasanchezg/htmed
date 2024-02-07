.metout_models <- function(model.generator, model.m.type = NULL, model.y.type = NULL, data = NULL, data.models, ncores, treat, outcome, mediator, ...) {

  data.models$model_name <- paste(paste0('treat:', treat),
                                  paste0('mediator:', mediator),
                                  paste0('outcome:', outcome), sep='|')

  # building a dataframe with model.M and model.Y analysis
  if (model.generator == FALSE) {
    model.m <- data.models$model.M
    model.y <- data.models$model.Y

    results.models <- .providing_models(model.m=model.m, model.y=model.y)
  }

  else {
    list.m.models <- data.models$model.m.formula
    list.y.models <- data.models$model.y.formula

    results.models <- .generating_models(list.m.models=list.m.models, list.y.models=list.y.models,
                                         model.m.type=model.m.type, model.y.type=model.y.type, data=data, ncores=ncores, ...)
  }

  rownames(results.models) <- data.models$model_name
  return(results.models)
}

# p <- metout_models(model.generator = TRUE,
#                    data.models = results.models,
#                    model.m.type = lm,
#                    model.y.type = survreg,
#                    data = df,
#                    treat = results.models$independent.var,
#                    mediator = results.models$mediator.var,
#                    outcome = results.models$dependent.var,
#                    ncores = 5)

# q <- metout_models(model.generator = FALSE,
#                    data.models = p,
#                    data = df,
#                    treat = 'independent.var',
#                    mediator = 'mediator.var',
#                    outcome = 'dependent.var',
#                    ncores = 5)

################################################################################
.providing_models <- function(model.m, model.y) {

  ## TODO: quizás se puede mejorar el merge de los modelos (al final)

  if (length(model.m) != length(model.y)) {
    stop("The fitted models for mediator and treatment does not have the same length")
  }
  else if (is.null(names(model.m)) | is.null(names(model.y)) ) {
    warning(paste("Names of mediator and/or treatment models are null.",
                  "They will be treated as if the order of models in both lists would be the same") )
  }

  results.models <- stats::setNames(data.frame(matrix(ncol = 1, nrow = length(model.m))), c('model.M'))
  results.models$model.M <- model.m
  results.models$model.Y <- model.y

  return(results.models)
}
#k <- .providing_models(model.m = model.m, model.y =  model.y)

################################################################################
.generating_models <- function(list.m.models, list.y.models, model.m.type, model.y.type, data, ncores, ...) {

  if (length(list.m.models) != length(list.y.models)) {
    stop("The lists with the formulas for the fitted models for mediator and treatment does not have the same length")
  }
  else if (is.null(names(list.m.models)) | is.null(names(list.y.models)) ) {
    warning(paste("Names in any of the lists provided are null.",
                  "They will be treated as if the order of models in both lists would be the same") )
  }

  # mediator model
  results.models <- stats::setNames(data.frame(matrix(ncol = 1, nrow = length(list.m.models))), c('model.M'))
  results.models$model.M <- .model_MY(list.models=list.m.models, model.type=model.m.type, data=data, ncores=ncores, ...)

  # treatment model
  results.models$model.Y <- .model_MY(list.models=list.y.models, model.type=model.y.type, data=data, ncores=ncores, ...)

  return(results.models)
}
# kk <- .generating_models(list.m.models=q$model.m.formula, model.m.type=lm,
#                          list.y.models=q$model.y.formula, model.y.type=survreg, data=df, ncores=5)

################################################################################
.model_MY <- function(list.models, model.type, data, ncores, ...) {
  #print(deparse(substitute(model.type)))
  #print(model.type)

  # if (!model.type %in% c('lm', 'polr', 'bayespolr', 'glm', 'bayesglm', 'gam', 'rq', 'survreg',
  #                         'merMod', 'vglm'))
  #   stop("You have asked for odels not supported by mediate package.
  #        Causal mediation analysis will not be performed.")

  parallel::mclapply(list.models, function(formula) {
    # if (model.type == 'lm') {
    #   model <- stats::lm(as.formula(formula), data=data, ...)
    # }
    model <-  model.type(as.formula(formula), data=data, ...)
  }, mc.cores = ncores)
}
# k <- .model_MY(list.models = q$model.y.formula, model.type = survreg, data=df, ncores = 5)
# k <- .model_MY(list.models = q$model.m.formula, model.type = lm, data=df, ncores = 5)
