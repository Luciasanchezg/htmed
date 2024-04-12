
# skip_if_not("survival" %in% tolower((.packages())))

## ----------------------------------------------------------------------------
## Loading data
## ----------------------------------------------------------------------------
#### Data
data("models_surv", package = "hightmed")
data("models_lm", package = "hightmed")
data("df", package = "hightmed")

## ----------------------------------------------------------------------------
## Tests for generating the fitted models for the mediator or the outcome
## ----------------------------------------------------------------------------
test_that(
  desc = "checking if generating_models() computes models for the mediator",
  code = {

    # reading expected results
    file.tests <- "../testdata"
    load(file.path(file.tests, 'med_surv.RData'))

    med <- generating_models(
      column.models='model.m.formula',
      model.type=lm,
      data=df,
      data.models=models_surv,
      model.m = TRUE
      ) %>%
      dplyr::arrange(model.m.formula)

    expect_equal(med$model.M, med_surv$model.M)
  }
)


test_that(
  desc = "checking if generating_models() computes models for the outcome",
  code = {
    withr::local_package("survival")

    # reading expected results
    file.tests <- "../testdata"
    load(file.path(file.tests, 'out_surv.RData'))

    # generating results
    out <- generating_models(
      column.models='model.y.formula',
      model.type=survreg,
      data=df,
      data.models=models_surv,
      model.m = FALSE) %>%
      dplyr::arrange(model.y.formula)

    expect_equal(out$model.Y, out_surv$model.Y)
    }
  )


## ----------------------------------------------------------------------------
## Tests for generating the fitted models for the mediator and the outcome iteratively (with one or more outcomes)
## ----------------------------------------------------------------------------
test_that(
  desc = "checking if generating_models() can be called iterativelly to compute fitted models for treatment and mediator (for one outcome)",
  code = {
    withr::local_package("survival")

    # reading expected results
    file.tests <- "../testdata"
    load(file.path(file.tests, 'medANDout_surv.RData'))

    preprocess <- generating_models(
      column.models='model.m.formula',
      model.type=lm,
      data=df,
      data.models=models_surv,
      model.m = TRUE
    ) %>%
      dplyr::arrange(model.m.formula)

    preprocess <- generating_models(
      column.models='model.y.formula',
      model.type=survreg,
      data=df,
      data.models=preprocess,
      model.m = FALSE
    ) %>%
      dplyr::arrange(model.y.formula)

    expect_equal(preprocess, medANDout_surv)
  }
)


test_that(
  desc = "checking if generating_models() can be called iterativelly to compute fitted models for treatment and mediator (for more than one outcome)",
  code = {

    # reading expected results
    file.tests <- "../testdata"
    load(file.path(file.tests, 'medANDout_lm.RData'))

    preprocess <- generating_models(
      column.models='model.m.formula',
      model.type=lm,
      data=df,
      data.models=models_lm,
      model.m = TRUE,
      outcome='outcome'
    ) %>%
      dplyr::arrange(model.m.formula)

    preprocess <- generating_models(
      column.models='model.y.formula',
      model.type=lm,
      data=df,
      data.models=preprocess,
      model.m = FALSE,
      outcome='outcome'
    ) %>%
      dplyr::arrange(model.y.formula)

    expect_equal(preprocess, medANDout_lm)
  })



## ----------------------------------------------------------------------------
## Checking for errors
## ----------------------------------------------------------------------------
test_that(
  desc = "Introducing arguments with the wrong class to generating_models()",
  code = {
    expect_error(
      generating_models(
        column.models='model.m.formula',
        model.type=lm,
        data=as.matrix(df),
        data.models=models_surv,
        model.m = TRUE
      ),
      regexp = "Your data or data.models are not stored in a DataFrame"
    )
    expect_error(
      generating_models(
        column.models='model.m.formula',
        model.type=lm,
        data=models_surv,
        data.models=as.matrix(models_surv),
        model.m = TRUE
      ),
      regexp = "Your data or data.models are not stored in a DataFrame"
    )
    expect_error(
      generating_models(
        column.models='model.m.formula',
        model.type='lm',
        data=models_surv,
        data.models=models_surv,
        model.m = TRUE
      ),
      regexp = "model.type is not a function"
    )
    expect_error(
      generating_models(
        column.models=lm,
        model.type=lm,
        data=models_surv,
        data.models=models_surv,
        model.m = TRUE
      ),
      regexp = "Please, provide the name of the corresponding column as character"
    )
    expect_error(
      generating_models(
        column.models='model.m.formula',
        model.type=lm,
        data=models_surv,
        data.models=models_surv,
        model.m = 'TRUE'
      ),
      regexp = "model.m argument only admits logical"
    )
  }
)


test_that(
  desc = "Catch errors related to wrong arguments passed to generating_models() with one outcome",
  code = {
    expect_error(
      generating_models(
        column.models='model.x.formula',
        model.type=lm,
        data=df,
        data.models=models_surv,
        model.m = TRUE
        ),
      regexp = "Incorrect column name for the models"
    )
    expect_error(
      generating_models(
        column.models='model.x.formula',
        model.type=lm,
        data=df,
        data.models=models_surv,
        model.m = FALSE
        ),
      regexp = "Incorrect column name for the models"
    )
    models_subset <- models_surv[1:4,]
    expect_error(
      generating_models(
        column.models='mediators',
        model.type=lm,
        data=df,
        data.models=models_subset,
        model.m = TRUE
      ),
      regexp = "There are no right formulas in the columns selected"
    )
    expect_error(
      generating_models(
        column.models='model.m.formula',
        model.type=lm,
        data=df,
        data.models=models_lm,
        model.m = TRUE
      ),
      regexp = "Some models are duplicated"
    )
  }
)


test_that(
  desc = "Catch errors related to wrong arguments passed to generating_models() with more than outcome",
  code = {
    expect_error(
      generating_models(
        column.models='model.m.formula',
        model.type=lm,
        data=df,
        data.models=models_lm,
        model.m = TRUE,
        outcome=1
      ),
      regexp = "Please, provide the name of the outcome column as character"
    )
    expect_error(
      generating_models(
        column.models='model.m.formula',
        model.type=lm,
        data=df,
        data.models=models_lm,
        model.m = TRUE,
        outcome='outcomes'
      ),
      regexp = "Incorrect column name for the outcome"
    )
    expect_error(
      generating_models(
        column.models='model.m.formula',
        model.type=lm,
        data=df,
        data.models=rbind(models_lm, models_lm),
        model.m = TRUE,
        outcome='outcome'
      ),
      regexp = "Some models are duplicated"
    )
  }
)


