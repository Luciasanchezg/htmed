
## ----------------------------------------------------------------------------
## Loading data
## ----------------------------------------------------------------------------
#### Data
data("df", package = "htmed")
models_1out <- data_models(outcome = 'outcome.1', mediator = c('mediator.1', 'mediator.2'), treatment = c('treatment.1', 'treatment.2'))

file.tests <- "../testdata"
load(file.path(file.tests, 'medANDout_surv.RData'))

## ----------------------------------------------------------------------------
## Tests for generating the mediation models
## ----------------------------------------------------------------------------
#TODO:regexp = "Some models for the outcome contain warnings or errors. These rows will be removed"

test_that(
  desc = "checking if htmed() generates the high-throughput mediation tests (one outcome)",
  code = {
    # reading expected results
    file.tests <- "../testdata"
    load(file.path(file.tests, 'mediation_surv.RData'))

    mediation_results <- htmed(data.models=medANDout_surv,
                               column.modelm = 'model.M',
                               column.modely = 'model.Y',
                               treat='treatment',
                               mediator='mediator',
                               outcome='outcome',
                               seed=1,
                               ncores=1)
    expect_equal(mediation_results, mediation_surv)
  }
)


test_that(
  desc = "checking if htmed() generates the high-throughput mediation tests (more than one outcomes)",
  code = {
    # reading expected results
    file.tests <- "../testdata"
    load(file.path(file.tests, 'mediation_lm.RData'))
    load(file.path(file.tests, 'medANDout_lm.RData'))

    mediation_results <- htmed(data.models=medANDout_lm,
                               column.modelm='model.M',
                               column.modely='model.Y',
                               treat='treatment',
                               mediator='mediator',
                               outcome='outcome',
                               seed=1,
                               ncores=1)
    expect_equal(mediation_results, mediation_lm)
  }
)


## ----------------------------------------------------------------------------
## Checking for errors
## ----------------------------------------------------------------------------
test_that(
  desc = "Catch errors related to wrong arguments passed to htmed()",
  code = {
    expect_error(
      htmed(data.models=medANDout_surv,
            column.modelm = medANDout_surv,
            column.modely = 'model.Y',
            treat='treatment',
            mediator='mediator',
            outcome='outcome',
            seed=1),
      regexp = "Please, provide the name of the corresponding columns as characters"
    )
    expect_error(
      htmed(data.models=medANDout_surv,
            column.modelm = 'model.M',
            column.modely = 'model.Y',
            treat='treatment',
            mediator='mediator',
            outcome='outcome',
            seed='1'),
      regexp = "Seed must be numeric or not provided"
    )
    expect_error(
      htmed(data.models=as.matrix(medANDout_surv),
            column.modelm = 'model.M',
            column.modely = 'model.Y',
            treat='treatment',
            mediator='mediator',
            outcome='outcome',
            seed=1),
      regexp = "data.models must contain a DataFrame"
    )
    expect_error(
      htmed(data.models=medANDout_surv,
            column.modelm = 'model.X',
            column.modely = 'model.Y',
            treat='treatment',
            mediator='mediator',
            outcome='outcome',
            seed=1),
      regexp = "Wrong column names for fitted models for mediator or outcome"
    )
    expect_error(
      htmed(data.models=medANDout_surv,
            column.modelm = 'model.M',
            column.modely = 'model.Y',
            treat='treat',
            mediator='mediator',
            outcome='outcome',
            seed=1),
      regexp = "Wrong column names for treat, mediator or outcome"
    )
    expect_error(
      htmed(data.models=medANDout_surv,
            column.modelm = 'model.M',
            column.modely = 'model.Y',
            treat='treatment',
             mediator='mediator.column',
            outcome='outcome',
            seed=1),
      regexp = "Wrong column names for treat, mediator or outcome"
    )
    expect_error(
      htmed(data.models=medANDout_surv,
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
      htmed(data.models=medANDout_surv,
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
    models <- generate_models(
      column.models='model.m.formula',
      model.type=lm,
      data=df,
      data.models=models_1out,
      model.m = TRUE,
      ncores=1
    )
    models <- generate_models(
      column.models='model.y.formula',
      model.type=survreg,
      data=df,
      data.models=models,
      model.m = FALSE,
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
      regexp = "All models for the outcome or the mediator contain warnings or errors"
    )
  }
)




