
## ----------------------------------------------------------------------------
## Loading data
## ----------------------------------------------------------------------------
#### Data
file.tests <- "../testdata"
load(file.path(file.tests, 'df.RData'))

load(file.path(file.tests, 'mediation_1out.RData'))
load(file.path(file.tests, 'mediation_2out.RData'))

## ----------------------------------------------------------------------------
## Tests for formatting the mediation models produced by htmed()
## ----------------------------------------------------------------------------
test_that(
  desc = "checking if format_med() produces the expected outcome",
  code = {
    # reading expected results
    file.tests <- "../testdata"
    load(file.path(file.tests, 'format_1out.RData'))
    format_results <- format_med(mediation_1out)

    expect_equal(format_results, format_1out)
  }
)


test_that(
  desc = "Catch errors related to wrong arguments passed to format_med()",
  code = {

    expect_error(
      format_med('mediation_1out'),
      regexp = "mediation.list is not a list"
    )
    expect_error(
      format_med(mediation_1out$outcome.1),
      regexp = "mediation.list is not a list of lists"
    )
    load(file.path(file.tests, 'med_1out.RData'))
    results <- list()
    results[['outcome']] <- lapply(med_1out$model.m.formula, FUN = function(x) {lm(as.formula(x), data=df)})
    expect_error(
      format_med(results),
      regexp = "Some of the models introduced are not mediation models"
    )
    one_list <- list()
    results <- unlist(mediation_2out, recursive=FALSE)
    out <- 'results'
    one_list[[out]] <- results
    expect_error(
      format_med(one_list),
      regexp = "Are you introducing the same model more than one time?"
    )
    expect_error(
      format_med(mediation_1out, split=TRUE),
      regexp = "Using split=TRUE, the first level in the mediation.list should be the outcomes, the second, the conditions used to split and the third, the models"
    )
    expect_error(
      format_med(mediation_1out, split='TRUE'),
      regexp = "split argument only admits logical"
    )
  }
)

