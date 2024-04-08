
## ----------------------------------------------------------------------------
## Loading data
## ----------------------------------------------------------------------------
#### Data
file.tests <- "../testdata"
load(file.path(file.tests, 'mediation_surv.RData'))
load(file.path(file.tests, 'mediation_lm.RData'))

## ----------------------------------------------------------------------------
## Tests for formatting the mediation models produced by hightmed()
## ----------------------------------------------------------------------------
test_that(
  desc = "checking if formatting_medresults() produces the expected outcome",
  code = {
    # reading expected results
    file.tests <- "../testdata"
    load(file.path(file.tests, 'format_surv.RData'))
    format_results <- formatting_med(mediation_surv)

    expect_equal(format_results, format_surv)
  }
)


test_that(
  desc = "Catch errors related to wrong arguments passed to formatting_med()",
  code = {
    # reading expected results
    file.tests <- "../../data"
    load(file.path(file.tests, 'df.RData'))
    load(file.path(file.tests, 'models_surv.RData'))

    expect_error(
      formatting_med('mediation_surv'),
      regexp = "mediation.list is not a list"
    )
    expect_error(
      formatting_med(mediation_surv$outcome.1),
      regexp = "mediation.list is not a list of lists"
    )

    results <- list()
    results[['outcome']] <- lapply(models_surv$model.m.formula, FUN = function(x) {lm(as.formula(x), data=df)})
    expect_error(
      formatting_med(results),
      regexp = "Some of the models introduced are not mediation models"
    )

    one_list <- list()
    results <- unlist(mediation_lm, recursive=FALSE)
    out <- 'results'
    one_list[[out]] <- results
    expect_error(
      formatting_med(one_list),
      regexp = "Are you introducing the same model more than one time?"
    )
  }
)

