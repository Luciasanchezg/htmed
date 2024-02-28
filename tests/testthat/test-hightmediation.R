
## ----------------------------------------------------------------------------
## Generating data
## ----------------------------------------------------------------------------
#### Data
data("models_surv", package = "hightmed")
data("df", package = "hightmed")

## fitted models for the outcome and mediator
medANDout_surv <- generating_models(column.models='model.m.formula', model.type=lm,
                                    data=df, data.models=models_surv, model.m = TRUE)
medANDout_surv <- generating_models(column.models='model.y.formula', model.type=survival::survreg,
                                    data=df, data.models=medANDout_surv, model.m = FALSE)


## ----------------------------------------------------------------------------
## Tests for generating the mediation models
## ----------------------------------------------------------------------------
test_that(
  desc = "checking if hightmed() generates the high-throughput mediation tests",
  code = {

    # reading expected results
    file.tests <- "../testdata"
    load(file.path(file.tests, 'mediation_surv.RData'))

    mediation_results <- hightmed(sims=1000,
                                  data.models=medANDout_surv,
                                  column.modelm = 'model.M',
                                  column.modely = 'model.Y',
                                  treat='treatments',
                                  mediator='mediators',
                                  outcome='outcome',
                                  seed=1)

    expect_equal(mediation_results, mediation_surv)
  })


# test_that(
#   desc = "Catch message when some models are warnings or errors",
#   code = {
#     expect_message(
#       hightmed(sims=1000,
#                data.models=medANDtreat,
#                column.modelm = 'model.M',
#                column.modely = 'model.Y',
#                treat='treatments',
#                mediator='mediators',
#                outcome='outcome',
#                seed=1),
#       regexp = "Some models for the outcome contain warnings or errors. These rows will be removed"
#     )
#   }
# )


test_that(
  desc = "Catch errors related to wrong arguments passed to hightmed()",
  code = {
    expect_error(
      hightmed(sims=1000,
               data.models=medANDout_surv,
               column.modelm = medANDout_surv,
               column.modely = 'model.Y',
               treat='treatments',
               mediator='mediators',
               outcome='outcome',
               seed=1),
      regexp = "Please, provide the name of the corresponding columns as characters"
    )
    expect_error(
      hightmed(sims='1000',
               data.models=medANDout_surv,
               column.modelm = 'model.M',
               column.modely = 'model.Y',
               treat='treatments',
               mediator='mediators',
               outcome='outcome',
               seed=1),
      regexp = "Number of simulations must be numeric"
    )
    expect_error(
      hightmed(sims=1000,
               data.models=medANDout_surv,
               column.modelm = 'model.M',
               column.modely = 'model.Y',
               treat='treatments',
               mediator='mediators',
               outcome='outcome',
               seed='1'),
      regexp = "Seed must be numeric"
    )
    expect_error(
      hightmed(sims=1000,
               data.models=as.matrix(medANDout_surv),
               column.modelm = 'model.M',
               column.modely = 'model.Y',
               treat='treatments',
               mediator='mediators',
               outcome='outcome',
               seed=1),
      regexp = "data.models must contain a DataFrame"
    )
    expect_error(
      hightmed(sims=1000,
               data.models=medANDout_surv,
               column.modelm = 'model.X',
               column.modely = 'model.Y',
               treat='treatments',
               mediator='mediators',
               outcome='outcome',
               seed=1),
      regexp = "Wrong column names for fitted models for mediator or outcome"
    )
    expect_error(
      hightmed(sims=1000,
               data.models=medANDout_surv,
               column.modelm = 'model.M',
               column.modely = 'model.Y',
               treat='treatments',
               mediator='mediator.column',
               outcome='outcome',
               seed=1),
      regexp = "Wrong column names for treat, mediator or outcome"
    )
  }
)


