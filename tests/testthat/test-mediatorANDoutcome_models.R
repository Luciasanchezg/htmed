
# skip_if_not("survival" %in% tolower((.packages())))

## ----------------------------------------------------------------------------
## Loading data
## ----------------------------------------------------------------------------
#### Data
file.tests <- "../testdata"
load(file.path(file.tests, 'df.RData'))

models_1out <- data_models(outcome = 'MVO', mediator = c('LV.EDV', 'RV.EDV'), treatment = c('LDL', 'Chol'))
models_2out <- data_models(outcome = c('MVO','Chol'), mediator = c('LV.EDV', 'RV.EDV'), treatment = c('LDL'))

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
      column.models = 'model.m.formula',
      model.type = lm,
      data = df,
      data.models = models_1out,
      model.name = 'model.M',
      ncores = 1
    ) %>%
      dplyr::arrange(model.m.formula)

    expect_models_equal(med$model.M, med_1out$model.M)
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
      column.models = 'model.y.formula',
      model.type = lm,
      data = df,
      data.models = models_1out,
      model.name = 'model.Y',
      ncores = 1
    ) %>%
      dplyr::arrange(model.y.formula)

    expect_models_equal(out$model.Y, out_1out$model.Y)
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
      column.models = 'model.m.formula',
      model.type = lm,
      data = df,
      data.models = models_1out,
      model.name = 'model.M',
      ncores = 1
    ) %>%
      dplyr::arrange(model.m.formula)

    preprocess <- outcome_models(
      column.models = 'model.y.formula',
      model.type = lm,
      data = df,
      data.models = preprocess,
      model.name = 'model.Y',
      ncores = 1
    ) %>%
      dplyr::arrange(model.y.formula)

    expect_equal(
      dplyr::select(preprocess, -model.M, -model.Y),
      dplyr::select(medANDout_1out, -model.M, -model.Y)
    )
    expect_models_equal(preprocess$model.M, medANDout_1out$model.M)
    expect_models_equal(preprocess$model.Y, medANDout_1out$model.Y)
  }
)


test_that(
  desc = "checking if mediator_models() and outcome_models() can be called iterativelly to compute fitted models for treatment and mediator (for more than one outcome)",
  code = {

    # reading expected results
    file.tests <- "../testdata"
    load(file.path(file.tests, 'medANDout_2out.RData'))

    preprocess <- mediator_models(
      column.models = 'model.m.formula',
      model.type = lm,
      data = df,
      data.models = models_2out,
      model.name = 'model.M',
      outcome = 'outcome',
      ncores = 1
    ) %>%
      dplyr::arrange(model.m.formula)

    preprocess <- outcome_models(
      column.models = 'model.y.formula',
      model.type = lm,
      data = df,
      data.models = preprocess,
      model.name = 'model.Y',
      outcome = 'outcome',
      ncores = 1
    ) %>%
      dplyr::arrange(model.y.formula)

    expect_models_equal(preprocess$model.M, medANDout_2out$model.M)
    expect_models_equal(preprocess$model.Y, medANDout_2out$model.Y)
  }
)


## ----------------------------------------------------------------------------
## Tests for data_models()
## ----------------------------------------------------------------------------
test_that(
  desc = "data_models() returns a dataframe with correct structure and formulas",
  code = {
    m <- data_models(
      outcome = 'MVO',
      mediator = c('LV.EDV', 'RV.EDV'),
      treatment = 'LDL'
    )

    expect_s3_class(m, "data.frame")
    expect_true(all(c("outcome", "treatment", "mediator", "model.m.formula", "model.y.formula") %in% colnames(m)))
    expect_equal(nrow(m), 2L)  # 1 outcome x 1 treatment x 2 mediators
    expect_true(all(grepl("~ `LDL`", m$model.m.formula)))
    expect_true(all(grepl("`MVO`", m$model.y.formula)))
  }
)


test_that(
  desc = "data_models() returns correct row count for multiple outcomes/treatments/mediators",
  code = {
    m <- data_models(
      outcome = c('MVO', 'Chol'),
      mediator = c('LV.EDV', 'RV.EDV'),
      treatment = c('LDL', 'FC')
    )

    expect_equal(nrow(m), 8L)  # 2 x 2 x 2
  }
)


test_that(
  desc = "data_models() accepts custom col_m and col_y names",
  code = {
    m <- data_models(
      outcome = 'MVO',
      mediator = 'LV.EDV',
      treatment = 'LDL',
      col_m = 'form.m',
      col_y = 'form.y'
    )

    expect_true(all(c("form.m", "form.y") %in% colnames(m)))
    expect_false(any(c("model.m.formula", "model.y.formula") %in% colnames(m)))
  }
)


test_that(
  desc = "data_models() throws errors for invalid inputs",
  code = {
    expect_error(
      data_models(outcome = data.frame(x = 'MVO'), mediator = 'LV.EDV', treatment = 'LDL'),
      regexp = "Please, provide a vector for the outcome"
    )
    expect_error(
      data_models(outcome = 'MVO', mediator = data.frame(x = 'LV.EDV'), treatment = 'LDL'),
      regexp = "Please, provide a vector for the mediator"
    )
    expect_error(
      data_models(outcome = 'MVO', mediator = 'LV.EDV', treatment = data.frame(x = 'LDL')),
      regexp = "Please, provide a vector for the treatment"
    )
    expect_error(
      data_models(outcome = 'MVO', mediator = 'LV.EDV', treatment = 'LDL',
                  col_m = 1, col_y = 'form.y'),
      regexp = "col_m and col_y must be character strings"
    )
    expect_error(
      data_models(outcome = 'MVO', mediator = 'LV.EDV', treatment = 'LDL',
                  col_m = 'same', col_y = 'same'),
      regexp = "col_m and col_y must be different"
    )
    expect_error(
      data_models(outcome = 'MVO', mediator = 'LV.EDV', treatment = 'LDL',
                  col_m = 'outcome', col_y = 'form.y'),
      regexp = "col_m and col_y must not use reserved column names"
    )
  }
)


## ----------------------------------------------------------------------------
## Checking for errors
## ----------------------------------------------------------------------------
test_that(
  desc = "Introducing arguments with the wrong class to mediator_models()",
  code = {
    expect_error(
      mediator_models(
        column.models = 'model.m.formula',
        model.type = lm,
        data = as.matrix(df),
        data.models = models_1out,
        model.name = 'model.M',
      ),
      regexp = "Your data or data.models are not stored in a DataFrame"
    )
    expect_error(
      mediator_models(
        column.models = 'model.m.formula',
        model.type = lm,
        data = models_1out,
        data.models = as.matrix(models_1out),
        model.name = 'model.M',
      ),
      regexp = "Your data or data.models are not stored in a DataFrame"
    )
    expect_error(
      mediator_models(
        column.models = 'model.m.formula',
        model.type = 'lm',
        data = models,
        data.models = models_1out,
        model.name = 'model.M',
      ),
      regexp = "model.type is not a function"
    )
    expect_error(
      mediator_models(
        column.models = lm,
        model.type = lm,
        data = models,
        data.models = models_1out,
        model.name = 'model.M',
      ),
      regexp = "Please, provide the name of the corresponding column as character"
    )
    expect_error(
      mediator_models(
        column.models = 'model.m.formula',
        model.type = lm,
        data = models_1out,
        data.models = models_1out,
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
        column.models = 'model.x.formula',
        model.type = lm,
        data = df,
        data.models = models_1out,
        model.name = 'model.M',
      ),
      regexp = "Incorrect column name for the models"
    )
    models_subset <- models_1out[1:4,]
    expect_error(
      mediator_models(
        column.models = 'mediator',
        model.type = lm,
        data = df,
        data.models = models_subset,
        model.name = 'model.M',
      ),
      regexp = "There are no right formulas in the columns selected"
    )
    expect_error(
      mediator_models(
        column.models = 'model.m.formula',
        model.type = lm,
        data = df,
        data.models = models_2out,
        model.name = 'model.M',
      ),
      regexp = "Some models are duplicated"
    )
    expect_error(
      mediator_models(
        column.models = 'model.m.formula',
        model.type = lm,
        data = df,
        data.models = models_2out,
        model.name = 'model.M',
        data.split = 'split.1'
      ),
      regexp = "data.split argument is not in data"
    )
    expect_error(
      mediator_models(
        column.models = 'model.m.formula',
        model.type = lm,
        data = df,
        data.models = models_2out,
        model.name = 'model.M',
        data.split = 1,
      ),
      regexp = "data.split is not a character"
    )
    expect_error(
      mediator_models(
        column.models = 'model.m.formula',
        model.type = lm,
        data = df,
        data.models = models_2out,
        model.name = 'model.M',
        data.split = 1,
        ncores = '1'
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
        column.models = 'model.m.formula',
        model.type = lm,
        data = df,
        data.models = models_2out,
        model.name = 'model.M',
        outcome = 1
      ),
      regexp = "Please, provide the name of the outcome column as character"
    )
    expect_error(
      mediator_models(
        column.models = 'model.m.formula',
        model.type = lm,
        data = df,
        data.models = models_2out,
        model.name = 'model.M',
        outcome = 'outcomes'
      ),
      regexp = "Incorrect column name for the outcome"
    )
    expect_error(
      mediator_models(
        column.models = 'model.m.formula',
        model.type = lm,
        data = df,
        data.models = rbind(models_2out, models_2out),
        model.name = 'model.M',
        outcome = 'outcome'
      ),
      regexp = "Some models are duplicated"
    )
  }
)


test_that(
  desc = "Introducing arguments with the wrong class to outcome_models()",
  code = {
    expect_error(
      outcome_models(
        column.models = 'model.y.formula',
        model.type = lm,
        data = as.matrix(df),
        data.models = models_1out,
        model.name = 'model.Y',
      ),
      regexp = "Your data or data.models are not stored in a DataFrame"
    )
    expect_error(
      outcome_models(
        column.models = 'model.y.formula',
        model.type = 'lm',
        data = df,
        data.models = models_1out,
        model.name = 'model.Y',
      ),
      regexp = "model.type is not a function"
    )
    expect_error(
      outcome_models(
        column.models = 'model.y.formula',
        model.type = lm,
        data = df,
        data.models = models_1out,
        model.name = TRUE,
      ),
      regexp = "model.name is not a character"
    )
    expect_error(
      outcome_models(
        column.models = 'model.x.formula',
        model.type = lm,
        data = df,
        data.models = models_1out,
        model.name = 'model.Y',
      ),
      regexp = "Incorrect column name for the models"
    )
  }
)


test_that(
  desc = "mediator_models() and outcome_models() throw an error when model.name column already exists",
  code = {
    preprocess <- mediator_models(
      column.models = 'model.m.formula',
      model.type = lm,
      data = df,
      data.models = models_1out,
      model.name = 'model.M',
      ncores = 1
    )

    expect_error(
      mediator_models(
        column.models = 'model.m.formula',
        model.type = lm,
        data = df,
        data.models = preprocess,
        model.name = 'model.M',
        ncores = 1
      ),
      regexp = "already exists"
    )

    preprocess2 <- outcome_models(
      column.models = 'model.y.formula',
      model.type = lm,
      data = df,
      data.models = preprocess,
      model.name = 'model.Y',
      ncores = 1
    )

    expect_error(
      outcome_models(
        column.models = 'model.y.formula',
        model.type = lm,
        data = df,
        data.models = preprocess2,
        model.name = 'model.Y',
        ncores = 1
      ),
      regexp = "already exists"
    )
  }
)


test_that(
  desc = "mediator_models() silently drops NA rows and still produces valid models",
  code = {
    df_na <- df
    df_na[1, "LDL"] <- NA
    result <- NULL
    expect_message(
      {result <- mediator_models(
        column.models = 'model.m.formula',
        model.type = lm,
        data = df_na,
        data.models = models_1out,
        model.name = 'model.M',
        ncores = 1
      )},
      regexp = "NA"
    )

    expect_true(all(sapply(result$model.M, function(m) inherits(m, "lm"))))
    expect_equal(length(result$model.M[[1]]$residuals), nrow(df) - 1L)
  }
)


test_that(
  desc = "mediator_models() works with a valid data.split column",
  code = {
    df_split <- df
    df_split$group <- rep(c("A", "B"), each = 50)
    models_split <- data_models(outcome = 'MVO', mediator = 'LV.EDV', treatment = 'LDL')
    models_split$group <- "A"
    models_split2 <- models_split
    models_split2$group <- "B"
    models_combined <- rbind(models_split, models_split2)

    result <- mediator_models(
      column.models = 'model.m.formula',
      model.type = lm,
      data = df_split,
      data.models = models_combined,
      model.name = 'model.M',
      data.split = 'group',
      ncores = 1
    )

    expect_true("group" %in% colnames(result))
    expect_equal(sort(unique(result$group)), c("A", "B"))
    expect_true(all(sapply(result$model.M, function(m) inherits(m, "lm"))))
  }
)


test_that(
  desc = "mediator_models() passes extra arguments (...) to the model function (glm family)",
  code = {
    df_bin <- df
    df_bin$bin_mediator <- as.integer(df$LV.EDV > 0)
    models_bin <- data_models(outcome = 'MVO', mediator = 'bin_mediator', treatment = 'LDL')

    result <- mediator_models(
      column.models = 'model.m.formula',
      model.type = glm,
      data = df_bin,
      data.models = models_bin,
      model.name = 'model.M',
      ncores = 1,
      family = binomial()
    )

    expect_true(inherits(result$model.M[[1]], "glm"))
    expect_equal(result$model.M[[1]]$family$family, "binomial")
  }
)


test_that(
  desc = "outcome_models() returns a character string in model column when model fitting fails",
  code = {
    bad_models <- data_models(outcome = 'nonexistent', mediator = 'LV.EDV', treatment = 'LDL')

    result <- outcome_models(
      column.models = 'model.y.formula',
      model.type = lm,
      data = df,
      data.models = bad_models,
      model.name = 'model.Y',
      ncores = 1
    )

    expect_true(is.character(result$model.Y[[1]]))
    expect_match(result$model.Y[[1]], "Error message")
  }
)


## ----------------------------------------------------------------------------
## Checking error/warning return behaviour of .modeling() via mediator_models()
## ----------------------------------------------------------------------------
test_that(
  desc = "mediator_models() returns a character string in model column when model fitting fails",
  code = {
    bad_models <- data_models(outcome = 'outcome.1', mediator = 'nonexistent', treatment = 'treatment.1')

    result <- mediator_models(
      column.models = 'model.m.formula',
      model.type = lm,
      data = df,
      data.models = bad_models,
      model.name = 'model.M',
      ncores = 1
    )

    expect_true(is.character(result$model.M[[1]]))
    expect_match(result$model.M[[1]], "Error message")
  }
)


test_that(
  desc = "mediator_models() stores warnings in $warningsModel and returns a valid model object",
  code = {
    # 1-row data forces lm to warn about rank deficiency / perfect fit
    result <- mediator_models(
      column.models = 'model.m.formula',
      model.type = lm,
      data = df[1, ],
      data.models = models_1out,
      model.name = 'model.M',
      ncores = 1
    )

    has_warning <- sapply(result$model.M, function(m) !is.null(m$warningsModel))
    expect_true(any(has_warning))
    expect_true(all(sapply(result$model.M[has_warning], function(m) inherits(m, "lm"))))
  }
)


