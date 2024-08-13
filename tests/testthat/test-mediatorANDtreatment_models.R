
# skip_if_not("survival" %in% tolower((.packages())))

## ----------------------------------------------------------------------------
## Loading data
## ----------------------------------------------------------------------------
#### Data
file.tests <- "../testdata"
load(file.path(file.tests, 'df.RData'))
load(file.path(file.tests, 'df2.RData'))

models_1out <- data_models(outcome = 'outcome.1', mediator = c('mediator.1', 'mediator.2'), treatment = c('treatment.1', 'treatment.2'))
models_2out <- data_models(outcome = c('outcome.1', 'outcome.2'), mediator = c('mediator.1', 'mediator.2'), treatment = 'treatment.1')

## ----------------------------------------------------------------------------
## Tests for generating the fitted models for the mediator or the outcome
## ----------------------------------------------------------------------------
test_that(
  desc = "checking if mediator_models() computes models for the mediator",
  code = {

    # reading expected results
    file.tests <- "../testdata"
    load(file.path(file.tests, 'med_1out.RData'))

    med <- mediator_models(
      column.models='model.m.formula',
      model.type=lm,
      data=df,
      data.models=models_1out,
      model.name = 'model.M',
      ncores = 1
      ) %>%
      dplyr::arrange(model.m.formula)

    expect_equal(med$model.M, med_1out$model.M)
  }
)


test_that(
  desc = "checking if outcome_models() computes models for the outcome",
  code = {

    # reading expected results
    file.tests <- "../testdata"
    load(file.path(file.tests, 'out_1out.RData'))

    # generating results
    out <- outcome_models(
      column.models='model.y.formula',
      model.type=lm,
      data=df,
      data.models=models_1out,
      model.name = 'model.Y',
      ncores = 1
      ) %>%
      dplyr::arrange(model.y.formula)

    expect_equal(out$model.Y, out_1out$model.Y)
    }
  )


## ----------------------------------------------------------------------------
## Tests for generating the fitted models for the mediator and the outcome iteratively (with one or more outcomes)
## ----------------------------------------------------------------------------
test_that(
  desc = "checking if mediator_models() and outcome_models() can be called iterativelly to compute fitted models for treatment and mediator (for one outcome)",
  code = {

    # reading expected results
    file.tests <- "../testdata"
    load(file.path(file.tests, 'medANDout_1out.RData'))

    preprocess <- mediator_models(
      column.models='model.m.formula',
      model.type=lm,
      data=df,
      data.models=models_1out,
      model.name = 'model.M',
      ncores = 1
    ) %>%
      dplyr::arrange(model.m.formula)

    preprocess <- outcome_models(
      column.models='model.y.formula',
      model.type=lm,
      data=df,
      data.models=preprocess,
      model.name = 'model.Y',
      ncores = 1
    ) %>%
      dplyr::arrange(model.y.formula)

    expect_equal(preprocess, medANDout_1out)
  }
)


test_that(
  desc = "checking if mediator_models() and outcome_models() can be called iterativelly to compute fitted models for treatment and mediator (for more than one outcome)",
  code = {

    # reading expected results
    file.tests <- "../testdata"
    load(file.path(file.tests, 'medANDout_2out.RData'))

    preprocess <- mediator_models(
      column.models='model.m.formula',
      model.type=lm,
      data=df2,
      data.models=models_2out,
      model.name = 'model.M',
      outcome='outcome',
      ncores = 1
    ) %>%
      dplyr::arrange(model.m.formula)

    preprocess <- outcome_models(
      column.models='model.y.formula',
      model.type=lm,
      data=df2,
      data.models=preprocess,
      model.name = 'model.Y',
      outcome='outcome',
      ncores = 1
    ) %>%
      dplyr::arrange(model.y.formula)

    expect_equal(preprocess, medANDout_2out)
  })



## ----------------------------------------------------------------------------
## Checking for errors
## ----------------------------------------------------------------------------
test_that(
  desc = "Introducing arguments with the wrong class to mediator_models()",
  code = {
    expect_error(
      mediator_models(
        column.models='model.m.formula',
        model.type=lm,
        data=as.matrix(df),
        data.models=models_1out,
        model.name = 'model.M',
      ),
      regexp = "Your data or data.models are not stored in a DataFrame"
    )
    expect_error(
      mediator_models(
        column.models='model.m.formula',
        model.type=lm,
        data=models_1out,
        data.models=as.matrix(models_1out),
        model.name = 'model.M',
      ),
      regexp = "Your data or data.models are not stored in a DataFrame"
    )
    expect_error(
      mediator_models(
        column.models='model.m.formula',
        model.type='lm',
        data=models,
        data.models=models_1out,
        model.name = 'model.M',
      ),
      regexp = "model.type is not a function"
    )
    expect_error(
      mediator_models(
        column.models=lm,
        model.type=lm,
        data=models,
        data.models=models_1out,
        model.name = 'model.M',
      ),
      regexp = "Please, provide the name of the corresponding column as character"
    )
    expect_error(
      mediator_models(
        column.models='model.m.formula',
        model.type=lm,
        data=models_1out,
        data.models=models_1out,
        model.name = TRUE,
      ),
      regexp = "model.name is not a character"
    )
  }
)


test_that(
  desc = "Catch errors related to wrong arguments passed to mediator_models() with one outcome",
  code = {
    expect_error(
      mediator_models(
        column.models='model.x.formula',
        model.type=lm,
        data=df,
        data.models=models_1out,
        model.name = 'model.M',
        ),
      regexp = "Incorrect column name for the models"
    )
    models_subset <- models_1out[1:4,]
    expect_error(
      mediator_models(
        column.models='mediator',
        model.type=lm,
        data=df,
        data.models=models_subset,
        model.name = 'model.M',
      ),
      regexp = "There are no right formulas in the columns selected"
    )
    expect_error(
      mediator_models(
        column.models='model.m.formula',
        model.type=lm,
        data=df,
        data.models=models_2out,
        model.name = 'model.M',
      ),
      regexp = "Some models are duplicated"
    )
    expect_error(
      mediator_models(
        column.models='model.m.formula',
        model.type=lm,
        data=df,
        data.models=models_2out,
        model.name = 'model.M',
        data.split='split.1'
      ),
      regexp = "data.split argument is not in data"
    )
    expect_error(
      mediator_models(
        column.models='model.m.formula',
        model.type=lm,
        data=df,
        data.models=models_2out,
        model.name = 'model.M',
        data.split=1,
      ),
      regexp = "data.split is not a character"
    )
    expect_error(
      mediator_models(
        column.models='model.m.formula',
        model.type=lm,
        data=df,
        data.models=models_2out,
        model.name = 'model.M',
        data.split=1,
        ncores='1'
      ),
      regexp = "ncores must be numeric if provided"
    )
  }
)


test_that(
  desc = "Catch errors related to wrong arguments passed to mediator_models() with more than outcome",
  code = {
    expect_error(
      mediator_models(
        column.models='model.m.formula',
        model.type=lm,
        data=df,
        data.models=models_2out,
        model.name = 'model.M',
        outcome=1
      ),
      regexp = "Please, provide the name of the outcome column as character"
    )
    expect_error(
      mediator_models(
        column.models='model.m.formula',
        model.type=lm,
        data=df,
        data.models=models_2out,
        model.name = 'model.M',
        outcome='outcomes'
      ),
      regexp = "Incorrect column name for the outcome"
    )
    expect_error(
      mediator_models(
        column.models='model.m.formula',
        model.type=lm,
        data=df,
        data.models=rbind(models_2out, models_2out),
        model.name = 'model.M',
        outcome='outcome'
      ),
      regexp = "Some models are duplicated"
    )
  }
)


