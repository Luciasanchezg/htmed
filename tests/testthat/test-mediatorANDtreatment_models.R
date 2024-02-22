


skip_if_not("survival" %in% tolower((.packages())))

## ----------------------------------------------------------------------------
## Generating data
## ----------------------------------------------------------------------------
#### Data
data("models", package = "hightmed")
data("df", package = "hightmed")

# set.seed(2024)
# df <- data.frame(
#   mediator.1 = runif(1:100, min=-1, max=1),
#   mediator.2 = runif(1:100, min=-1, max=1),
#   mediator.3 = runif(1:100, min=-1, max=1),
#   mediator.4 = runif(1:100, min=-1, max=1),
#   treatment.1 = runif(1:100, min=-1, max=1),
#   treatment.2 = runif(1:100, min=-1, max=1),
#   treatment.3 = runif(1:100, min=-1, max=1),
#   treatment.4 = runif(1:100, min=-1, max=1),
#   age_death = sample(15:150,100, replace=T),
#   HFpEF = sample(0:1,100, replace=T)
#   ) %>%
#   mutate(tsurvHF = with(., survival:::Surv(age_death, HFpEF == 1)))
#
# #### Models
# models <- expand.grid(mediators = colnames(df[,grepl(pattern = 'mediator', x = colnames(df))]),
#                       treatments =  colnames(df[,grepl(pattern = 'treatment', x = colnames(df))]),
#                       outcome = 'tsurvHF') %>%
#   mutate(model.m.formula = paste(mediators, '~', treatments)) %>%
#   mutate(model.y.formula = paste(outcome, '~', mediators, '+', treatments))


## ----------------------------------------------------------------------------
## Tests for generating the fitted models for the mediator and the outcome
## ----------------------------------------------------------------------------
test_that(
  desc = "checking if generating_models() computes models for the mediator",
  code = {

    # reading expected results
    file.tests <- "../testdata"
    load(file.path(file.tests, 'med_models.RData'))

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
    file.tests <- "../testdata"
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
  desc = "Introducing arguments with the wrong class to generating_models()",
  code = {
    expect_error(
      generating_models(
        column.models='model.m.formula',
        model.type=lm,
        data=as.matrix(df),
        data.models=models,
        model.m = TRUE
      ),
      regexp = "Your data or data.models are not stored in a DataFrame"
    )
    expect_error(
      generating_models(
        column.models='model.m.formula',
        model.type=lm,
        data=df,
        data.models=as.matrix(models),
        model.m = TRUE
      ),
      regexp = "Your data or data.models are not stored in a DataFrame"
    )
    expect_error(
      generating_models(
        column.models='model.m.formula',
        model.type='lm',
        data=df,
        data.models=models,
        model.m = TRUE
      ),
      regexp = "model.type is not a function"
    )
    expect_error(
      generating_models(
        column.models=lm,
        model.type=lm,
        data=df,
        data.models=models,
        model.m = TRUE
      ),
      regexp = "Please, provide the name of the corresponding column as character"
    )
    expect_error(
      generating_models(
        column.models='model.m.formula',
        model.type=lm,
        data=df,
        data.models=models,
        model.m = 'TRUE'
      ),
      regexp = "model.m argument only admits logical"
    )
  }
)


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
        column.models='treatments',
        model.type=lm,
        data=df,
        data.models=models,
        model.m = TRUE
      ),
      regexp = "There are no right formulas in the columns selected"
    )
  }
)


