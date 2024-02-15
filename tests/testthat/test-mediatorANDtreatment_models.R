

## ----------------------------------------------------------------------------
## CV example
## ----------------------------------------------------------------------------

# set.seed(2024)
data("df", package = "hightmed")
data("models", package = "hightmed")


## ----------------------------------------------------------------------------
## Tests for generating the fitted models for the mediator and the outcome
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
      regexp = "Incorrect column name for the models"
    )
    expect_error(
      generating_models(
        column.models='model.x.formula',
        model.type=lm,
        data=df,
        data.models=models,
        model.m = FALSE
        ),
      regexp = "Incorrect column name for the models"
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
      regexp = "There are no right formulas in the columns selected"
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


## ----------------------------------------------------------------------------
## Tests for providing the fitted models for the mediator and the outcome
## ----------------------------------------------------------------------------
test_that(
  desc = "checking if providing_models() generates a dataframe with the fitted models for the mediator and the outcome",
  code = {
    withr::local_package("survival")

    # reading results
    file.tests <- "tests/testdata"
    load(file.path(file.tests, 'medANDtreat.RData'))

    load(file.path(file.tests, 'med_models.RData'))
    load(file.path(file.tests, 'out_models.RData'))

    names(med_models$model.M) <- rownames(med_models)
    names(out_models$model.Y) <- rownames(out_models)
    prov_mod <- providing_models(model.m=med_models$model.M, model.y=out_models$model.Y)

    expect_equal(prov_mod, medANDtreat)
  }
)


test_that(
  desc = "Catch errors related to wrong arguments passed to providing_models()",
  code = {
    withr::local_package("survival")

    file.tests <- "tests/testdata"

    load(file.path(file.tests, 'med_models.RData'))
    load(file.path(file.tests, 'out_models.RData'))

    expect_error(
      providing_models(
        model.m=med_models$model.M,
        model.y=out_models$model.Y
      ),
      regexp = "Provide the same names in both lists"
    )

    names(med_models$model.M) <- rownames(med_models)
    names(out_models$model.Y) <- rownames(out_models)

    expect_error(
      providing_models(
        model.m=med_models$model.M[1:5],
        model.y=out_models$model.Y
        ),
      regexp = "The fitted models for mediator and treatment does not have the same length"
    )
  }
)



