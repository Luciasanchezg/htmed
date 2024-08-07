
# skip_if_not("survival" %in% tolower((.packages())))

## ----------------------------------------------------------------------------
## Loading data
## ----------------------------------------------------------------------------
#### Data
data("df", package = "htmed")
models_1out <- data_models(outcome = 'outcome.1', mediator = c('mediator.1', 'mediator.2'), treatment = c('treatment.1', 'treatment.2'))
models_2out <- data_models(outcome = c('outcome.2', 'outcome.3'), mediator = c('mediator.1', 'mediator.2'), treatment = 'treatment.1')

## ----------------------------------------------------------------------------
## Tests for generating the fitted models for the mediator or the outcome
## ----------------------------------------------------------------------------
test_that(
  desc = "checking if generate_models() computes models for the mediator",
  code = {

    # reading expected results
    file.tests <- "../testdata"
    load(file.path(file.tests, 'med_surv.RData'))

    med <- generate_models(
      column.models='model.m.formula',
      model.type=lm,
      data=df,
      data.models=models_1out,
      model.m = TRUE,
      ncores = 1
      ) %>%
      dplyr::arrange(model.m.formula)

    expect_equal(med$model.M, med_surv$model.M)
  }
)


test_that(
  desc = "checking if generate_models() computes models for the outcome",
  code = {
    withr::local_package("survival")

    # reading expected results
    file.tests <- "../testdata"
    load(file.path(file.tests, 'out_surv.RData'))

    # generating results
    out <- generate_models(
      column.models='model.y.formula',
      model.type=survreg,
      data=df,
      data.models=models_2out,
      model.m = FALSE,
      ncores = 1
      ) %>%
      dplyr::arrange(model.y.formula)

    expect_equal(out$model.Y, out_surv$model.Y)
    }
  )


## ----------------------------------------------------------------------------
## Tests for generating the fitted models for the mediator and the outcome iteratively (with one or more outcomes)
## ----------------------------------------------------------------------------
test_that(
  desc = "checking if generate_models() can be called iterativelly to compute fitted models for treatment and mediator (for one outcome)",
  code = {
    withr::local_package("survival")

    # reading expected results
    file.tests <- "../testdata"
    load(file.path(file.tests, 'medANDout_surv.RData'))

    preprocess <- generate_models(
      column.models='model.m.formula',
      model.type=lm,
      data=df,
      data.models=models_1out,
      model.m = TRUE,
      ncores = 1
    ) %>%
      dplyr::arrange(model.m.formula)

    preprocess <- generate_models(
      column.models='model.y.formula',
      model.type=survreg,
      data=df,
      data.models=preprocess,
      model.m = FALSE,
      ncores = 1
    ) %>%
      dplyr::arrange(model.y.formula)

    expect_equal(preprocess, medANDout_surv)
  }
)


test_that(
  desc = "checking if generate_models() can be called iterativelly to compute fitted models for treatment and mediator (for more than one outcome)",
  code = {

    # reading expected results
    file.tests <- "../testdata"
    load(file.path(file.tests, 'medANDout_lm.RData'))

    preprocess <- generate_models(
      column.models='model.m.formula',
      model.type=lm,
      data=df,
      data.models=models_2out,
      model.m = TRUE,
      outcome='outcome',
      ncores = 1
    ) %>%
      dplyr::arrange(model.m.formula)

    preprocess <- generate_models(
      column.models='model.y.formula',
      model.type=lm,
      data=df,
      data.models=preprocess,
      model.m = FALSE,
      outcome='outcome',
      ncores = 1
    ) %>%
      dplyr::arrange(model.y.formula)

    expect_equal(preprocess, medANDout_lm)
  })



## ----------------------------------------------------------------------------
## Checking for errors
## ----------------------------------------------------------------------------
test_that(
  desc = "Introducing arguments with the wrong class to generate_models()",
  code = {
    expect_error(
      generate_models(
        column.models='model.m.formula',
        model.type=lm,
        data=as.matrix(df),
        data.models=models_1out,
        model.m = TRUE
      ),
      regexp = "Your data or data.models are not stored in a DataFrame"
    )
    expect_error(
      generate_models(
        column.models='model.m.formula',
        model.type=lm,
        data=models_1out,
        data.models=as.matrix(models_1out),
        model.m = TRUE
      ),
      regexp = "Your data or data.models are not stored in a DataFrame"
    )
    expect_error(
      generate_models(
        column.models='model.m.formula',
        model.type='lm',
        data=models,
        data.models=models_1out,
        model.m = TRUE
      ),
      regexp = "model.type is not a function"
    )
    expect_error(
      generate_models(
        column.models=lm,
        model.type=lm,
        data=models,
        data.models=models_1out,
        model.m = TRUE
      ),
      regexp = "Please, provide the name of the corresponding column as character"
    )
    expect_error(
      generate_models(
        column.models='model.m.formula',
        model.type=lm,
        data=models_1out,
        data.models=models_1out,
        model.m = 'TRUE'
      ),
      regexp = "model.m argument only admits logical"
    )
  }
)


test_that(
  desc = "Catch errors related to wrong arguments passed to generate_models() with one outcome",
  code = {
    expect_error(
      generate_models(
        column.models='model.x.formula',
        model.type=lm,
        data=df,
        data.models=models_1out,
        model.m = TRUE
        ),
      regexp = "Incorrect column name for the models"
    )
    models_subset <- models_1out[1:4,]
    expect_error(
      generate_models(
        column.models='mediator',
        model.type=lm,
        data=df,
        data.models=models_subset,
        model.m = TRUE
      ),
      regexp = "There are no right formulas in the columns selected"
    )
    expect_error(
      generate_models(
        column.models='model.m.formula',
        model.type=lm,
        data=df,
        data.models=models_2out,
        model.m = TRUE
      ),
      regexp = "Some models are duplicated"
    )
    expect_error(
      generate_models(
        column.models='model.m.formula',
        model.type=lm,
        data=df,
        data.models=models_2out,
        model.m = TRUE,
        data.split='split.1'
      ),
      regexp = "data.split argument is not in data"
    )
    expect_error(
      generate_models(
        column.models='model.m.formula',
        model.type=lm,
        data=df,
        data.models=models_2out,
        model.m = TRUE,
        data.split=1,
      ),
      regexp = "data.split is not a character"
    )
    expect_error(
      generate_models(
        column.models='model.m.formula',
        model.type=lm,
        data=df,
        data.models=models_2out,
        model.m = TRUE,
        data.split=1,
        ncores='1'
      ),
      regexp = "ncores must be numeric if provided"
    )
  }
)


test_that(
  desc = "Catch errors related to wrong arguments passed to generate_models() with more than outcome",
  code = {
    expect_error(
      generate_models(
        column.models='model.m.formula',
        model.type=lm,
        data=df,
        data.models=models_2out,
        model.m = TRUE,
        outcome=1
      ),
      regexp = "Please, provide the name of the outcome column as character"
    )
    expect_error(
      generate_models(
        column.models='model.m.formula',
        model.type=lm,
        data=df,
        data.models=models_2out,
        model.m = TRUE,
        outcome='outcomes'
      ),
      regexp = "Incorrect column name for the outcome"
    )
    expect_error(
      generate_models(
        column.models='model.m.formula',
        model.type=lm,
        data=df,
        data.models=rbind(models_2out, models_2out),
        model.m = TRUE,
        outcome='outcome'
      ),
      regexp = "Some models are duplicated"
    )
  }
)


