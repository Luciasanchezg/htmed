
## ----------------------------------------------------------------------------
## Loading data
## ----------------------------------------------------------------------------
#### Data
data("df", package = "htmed")

file.tests <- "../testdata"
load(file.path(file.tests, 'mediation_surv.RData'))
load(file.path(file.tests, 'mediation_lm.RData'))

## ----------------------------------------------------------------------------
## Tests for formatting the mediation models produced by htmed()
## ----------------------------------------------------------------------------
test_that(
  desc = "checking if format_med() produces the expected outcome",
  code = {
    # reading expected results
    file.tests <- "../testdata"
    load(file.path(file.tests, 'format_surv.RData'))
    format_results <- format_med(mediation_surv)

    expect_equal(format_results, format_surv)
  }
)


test_that(
  desc = "Catch errors related to wrong arguments passed to format_med()",
  code = {
    # reading expected results
    models_1out <- data_models(outcome = 'outcome.1', mediator = c('mediator.1', 'mediator.2'), treatment = c('treatment.1', 'treatment.2'))

    expect_error(
      format_med('mediation_surv'),
      regexp = "mediation.list is not a list"
    )
    expect_error(
      format_med(mediation_surv$outcome.1),
      regexp = "mediation.list is not a list of lists"
    )
    results <- list()
    results[['outcome']] <- lapply(models_1out$model.m.formula, FUN = function(x) {lm(as.formula(x), data=df)})
    expect_error(
      format_med(results),
      regexp = "Some of the models introduced are not mediation models"
    )
    one_list <- list()
    results <- unlist(mediation_lm, recursive=FALSE)
    out <- 'results'
    one_list[[out]] <- results
    expect_error(
      format_med(one_list),
      regexp = "Are you introducing the same model more than one time?"
    )
    expect_error(
      format_med(mediation_surv, split=TRUE),
      regexp = "Using split=TRUE, the first level in the mediation.list should be the outcomes, the second, the conditions used to split and the third, the models"
    )
    expect_error(
      format_med(mediation_surv, split='TRUE'),
      regexp = "split argument only admits logical"
    )
  }
)

