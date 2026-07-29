
## ----------------------------------------------------------------------------
## Loading data
## ----------------------------------------------------------------------------
#### Data
file.tests <- "../testdata"
load(file.path(file.tests, 'df.RData'))
load(file.path(file.tests, 'medANDout_1out.RData'))

models_1out <- data_models(outcome = 'MVO', mediator = c('LV.EDV', 'RV.EDV'), treatment = c('LDL', 'Chol'))

## ----------------------------------------------------------------------------
## Tests for generating the mediation models
## ----------------------------------------------------------------------------
#TODO:regexp = "Some models for the outcome contain warnings or errors. These rows will be removed"

test_that(
  desc = "checking if htmed() generates the high-throughput mediation tests (one outcome)",
  code = {
    # reading expected results
    file.tests <- "../testdata"
    load(file.path(file.tests, 'mediation_1out.RData'))

    mediation_results <- htmed(data.models=medANDout_1out,
                               column.modelm = 'model.M',
                               column.modely = 'model.Y',
                               treat='treatment',
                               mediator='mediator',
                               outcome='outcome',
                               seed=1,
                               ncores=1)
    expect_equal(mediation_results, mediation_1out)
  }
)


test_that(
  desc = "checking if htmed() generates the high-throughput mediation tests (more than one outcomes)",
  code = {
    # reading expected results
    file.tests <- "../testdata"
    load(file.path(file.tests, 'medANDout_2out.RData'))
    load(file.path(file.tests, 'mediation_2out.RData'))

    mediation_results <- htmed(data.models=medANDout_2out,
                               column.modelm='model.M',
                               column.modely='model.Y',
                               treat='treatment',
                               mediator='mediator',
                               outcome='outcome',
                               seed=1,
                               ncores=1)
    expect_equal(mediation_results, mediation_2out)
  }
)


## ----------------------------------------------------------------------------
## Checking for errors
## ----------------------------------------------------------------------------
test_that(
  desc = "Catch errors related to wrong arguments passed to htmed()",
  code = {
    expect_error(
      htmed(data.models=medANDout_1out,
            column.modelm = medANDout_1out,
            column.modely = 'model.Y',
            treat='treatment',
            mediator='mediator',
            outcome='outcome',
            seed=1),
      regexp = "Please, provide the name of the corresponding columns as characters"
    )
    expect_error(
      htmed(data.models=medANDout_1out,
            column.modelm = 'model.M',
            column.modely = 'model.Y',
            treat='treatment',
            mediator='mediator',
            outcome='outcome',
            seed='1'),
      regexp = "Seed must be numeric or not provided"
    )
    expect_error(
      htmed(data.models=as.matrix(medANDout_1out),
            column.modelm = 'model.M',
            column.modely = 'model.Y',
            treat='treatment',
            mediator='mediator',
            outcome='outcome',
            seed=1),
      regexp = "data.models must contain a DataFrame"
    )
    expect_error(
      htmed(data.models=medANDout_1out,
            column.modelm = 'model.X',
            column.modely = 'model.Y',
            treat='treatment',
            mediator='mediator',
            outcome='outcome',
            seed=1),
      regexp = "Wrong column names for fitted models for mediator or outcome"
    )
    expect_error(
      htmed(data.models=medANDout_1out,
            column.modelm = 'model.M',
            column.modely = 'model.Y',
            treat='treat',
            mediator='mediator',
            outcome='outcome',
            seed=1),
      regexp = "Wrong column names for treat, mediator or outcome"
    )
    expect_error(
      htmed(data.models=medANDout_1out,
            column.modelm = 'model.M',
            column.modely = 'model.Y',
            treat='treatment',
             mediator='mediator.column',
            outcome='outcome',
            seed=1),
      regexp = "Wrong column names for treat, mediator or outcome"
    )
    expect_error(
      htmed(data.models=medANDout_1out,
            column.modelm = 'model.M',
            column.modely = 'model.Y',
            treat='treatment',
            mediator='mediator',
            outcome='outcome',
            seed=1,
            data.split='split.1'),
      regexp = "data.split argument is not in data"
    )
    expect_error(
      htmed(data.models=medANDout_1out,
            column.modelm = 'model.M',
            column.modely = 'model.Y',
            treat='treatment',
            mediator='mediator',
            outcome='outcome',
            seed=1,
            data.split=1),
      regexp = "data.split is not a character"
    )
  }
)


test_that(
  desc = "Errors related to warnings or errors in all the fitted models for outcome or mediator",
  code = {
    withr::local_package("survival")

    df <- df[1, ]
    models <- mediator_models(
      column.models='model.m.formula',
      model.type=lm,
      data=df,
      data.models=models_1out,
      model.name = 'model.M',
      ncores=1
    )
    models <- outcome_models(
      column.models='model.y.formula',
      model.type=survreg,
      data=df,
      data.models=models,
      model.name = 'model.Y',
      ncores=1
    )
    expect_error(
      htmed(data.models=models,
            column.modelm = 'model.M',
            column.modely = 'model.Y',
            treat='treatment',
            mediator='mediator',
            outcome='outcome',
            seed=1,
            ncores=1),
      regexp = "All models for the outcome or the mediator contain errors"
    )
  }
)


## ----------------------------------------------------------------------------
## Checking error/warning model filtering behaviour
## ----------------------------------------------------------------------------
test_that(
  desc = "htmed() removes rows where mediator model has errors, runs on the rest, and emits a message",
  code = {
    load(file.path("../testdata", "medANDout_1out.RData"))
    load(file.path("../testdata", "mediation_1out.RData"))

    bad <- medANDout_1out
    bad$model.M[[1]] <- "Error message: intentional test error"

    expect_message(
      result <- htmed(data.models = bad,
                      column.modelm = 'model.M',
                      column.modely = 'model.Y',
                      treat = 'treatment',
                      mediator = 'mediator',
                      outcome = 'outcome',
                      seed = 1,
                      ncores = 1),
      regexp = "Some models for the mediator contain errors"
    )
    expect_equal(length(result[[1]]), length(mediation_1out[[1]]) - 1)
  }
)


test_that(
  desc = "htmed() removes rows where outcome model has errors, runs on the rest, and emits a message",
  code = {
    load(file.path("../testdata", "medANDout_1out.RData"))
    load(file.path("../testdata", "mediation_1out.RData"))

    bad <- medANDout_1out
    bad$model.Y[[1]] <- "Error message: intentional test error"

    expect_message(
      result <- htmed(data.models = bad,
                      column.modelm = 'model.M',
                      column.modely = 'model.Y',
                      treat = 'treatment',
                      mediator = 'mediator',
                      outcome = 'outcome',
                      seed = 1,
                      ncores = 1),
      regexp = "Some models for the outcome contain errors"
    )
    expect_equal(length(result[[1]]), length(mediation_1out[[1]]) - 1)
  }
)


test_that(
  desc = "htmed() keeps warning models and runs mediation on all rows",
  code = {
    load(file.path("../testdata", "medANDout_1out.RData"))
    load(file.path("../testdata", "mediation_1out.RData"))

    medANDout_1out$model.M[[1]]$warningsModel <- "simulated warning"

    result <- htmed(data.models = medANDout_1out,
                    column.modelm = 'model.M',
                    column.modely = 'model.Y',
                    treat = 'treatment',
                    mediator = 'mediator',
                    outcome = 'outcome',
                    seed = 1,
                    ncores = 1)
    expect_equal(length(result[[1]]), length(mediation_1out[[1]]))
  }
)


test_that(
  desc = "htmed() works when the outcome column is a character vector, not a factor",
  code = {
    load(file.path("../testdata", "medANDout_1out.RData"))
    load(file.path("../testdata", "mediation_1out.RData"))

    medANDout_1out$outcome <- as.character(medANDout_1out$outcome)

    result <- htmed(data.models = medANDout_1out,
                    column.modelm = 'model.M',
                    column.modely = 'model.Y',
                    treat = 'treatment',
                    mediator = 'mediator',
                    outcome = 'outcome',
                    seed = 1,
                    ncores = 1)
    expect_equal(length(result), length(mediation_1out))
  }
)


## ----------------------------------------------------------------------------
## Regression: polr outcome + nonparametric bootstrap must not fail with
## "could not find function 'model.type'"
## ----------------------------------------------------------------------------
test_that(
  desc = paste(
    "htmed() with MASS::polr outcome runs nonparametric bootstrap",
    "without 'could not find function model.type' error"
  ),
  code = {
    skip_if_not_installed("MASS")

    df_ord <- df
    df_ord$MVO_ord <- cut(
      df$MVO, breaks = 3,
      labels = c("low", "mid", "high"), ordered = TRUE
    )
    models_ord <- data_models(
      outcome = 'MVO_ord', mediator = 'LV.EDV', treatment = 'LDL'
    )

    preprocess <- mediator_models(
      column.models = 'model.m.formula',
      model.type = lm,
      data = df_ord,
      data.models = models_ord,
      model.name = 'model.M',
      ncores = 1
    )
    preprocess <- outcome_models(
      column.models = 'model.y.formula',
      model.type = MASS::polr,
      data = df_ord,
      data.models = preprocess,
      model.name = 'model.Y',
      ncores = 1
    )

    # boot = TRUE triggers the nonparametric bootstrap refit path in
    # mediation::mediate(). Before the fix, this would fail with
    # "could not find function 'model.type'" because model$call[[1]]
    # held the local symbol rather than the actual function object.
    expect_no_error(
      htmed(
        data.models = preprocess,
        column.modelm = 'model.M',
        column.modely = 'model.Y',
        treat = 'treatment',
        mediator = 'mediator',
        outcome = 'outcome',
        seed = 1,
        ncores = 1,
        boot = TRUE,
        sims = 10
      )
    )
  }
)
