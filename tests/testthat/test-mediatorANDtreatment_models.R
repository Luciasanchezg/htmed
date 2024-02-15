context("Generating fitted models for mediator and outcome")

## ----------------------------------------------------------------------------
## Parameters
## ----------------------------------------------------------------------------
# Load reference objects

load("/data3/lsanchezg/PhD/mediation_package/hightmed/tests/testthat/out_models.RData")


## ----------------------------------------------------------------------------
## CV example
## ----------------------------------------------------------------------------

set.seed(2024)
data("df", package = "hightmed")
data("models", package = "hightmed")


## ----------------------------------------------------------------------------
## Tests for the mediator and the outcome
## ----------------------------------------------------------------------------
test_that(
  desc = "checking if generating_models() computes models for the mediator",
  code = {
    # reading expected results
    file.tests <- "tests/testdata"
    load(file.path(file.tests, 'med_models.RData'))

    # generating results
    med <- generating_models(
      column.models='model.m.formula',
      model.type=lm,
      data=df,
      data.models=models,
      model.m = TRUE
      ) %>%
      arrange(model.m.formula)

    expect_equal(med$model.M, med_models$model.M)
  })


test_that(
  desc = "checking if generating_models() computes models for the outcome",
  code = {
    withr::local_package("survival")

    # reading expected results
    file.tests <- "tests/testdata"
    load(file.path(file.tests, 'out_models.RData'))

    # generating results
    out <- generating_models(
      column.models='model.y.formula',
      model.type=survreg,
      data=df,
      data.models=models,
      model.m = FALSE) %>%
      arrange(model.y.formula)

    expect_equal(out$model.Y, out_models$model.Y)
})


test_that(
  desc = "Catch errors related to wrong arguments passed to generating_models()",
  code = {
    expect_error(
      generating_models(
        column.models='model.x.formula',
        model.type=lm,
        data=df,
        data.models=models,
        model.m = TRUE
        ),
      regexp = "Incorrect column name"
    )
    expect_error(
      generating_models(
        column.models='model.x.formula',
        model.type=lm,
        data=df,
        data.models=models,
        model.m = FALSE
        ),
      regexp = "Incorrect column name"
    )
    expect_error(
      generating_models(
        column.models='model.m.formula',
        model.type=lm,
        data=as.matrix(df),
        data.models=models,
        model.m = TRUE
        ),
      regexp = "Your data is not stored in a dataframe"
    )
    expect_error(
      generating_models(
        column.models='dependent.var',
        model.type=lm,
        data=df,
        data.models=models,
        model.m = TRUE
      ),
      regexp = "no right formulas in the columns selected"
    )
    expect_error(
      generating_models(
        column.models='model.m.formula',
        model.type=glm,
        data=df,
        data.models=models,
        model.m = TRUE,
        family=binomial
      ),
      regexp = "All analysis performed gave an error"
    )
  }
)
