
## ----------------------------------------------------------------------------
## Loading data
## ----------------------------------------------------------------------------
#### Data
file.tests <- "../testdata"
load(file.path(file.tests, 'format_surv.RData'))
load(file.path(file.tests, 'format_lm.RData'))

## ----------------------------------------------------------------------------
## Tests for visualizing results
## ----------------------------------------------------------------------------


## ----------------------------------------------------------------------------
## Errors in visualizing results for visual_htmed()
## ----------------------------------------------------------------------------
test_that(
  desc = "Catch errors related to wrong arguments passed to visualization()",
  code = {
    expect_error(
      visual_htmed(mediation.form = format_surv$outcome.1, outcome = 'outcome.1'),
      regexp = "mediation.form is not a list"
    )
    format_surv_list <- list()
    format_surv_list[['surv']] <- format_surv
    expect_error(
      visual_htmed(mediation.form = format_surv_list, outcome = 'surv'),
      regexp = "mediation.form is not a list of DataFrames"
    )
    expect_error(
      visual_htmed(mediation.form = format_surv, outcome = lm),
      regexp = "outcome is not a character"
    )
    expect_error(
      visual_htmed(mediation.form = format_surv, outcome = 'outcome'),
      regexp = "The outcome is not in mediation.form"
    )
  }
)


test_that(
  desc = "Modifying DataFrame",
  code = {
    format_surv$outcome.1 <- format_surv$outcome.1 %>% rename('p-value_PropMed'='p-value_Prop._Mediated_(average)')
    expect_error(
      visual_htmed(mediation.form = format_surv, outcome = 'outcome.1'),
      regexp = "Wrong columns in the outcome chosen"
    )
  }
)


test_that(
  desc = "Modifying DataFrame",
  code = {
    format_surv$outcome.1 <- format_surv$outcome.1 %>% rename('treat'='treatment')
    expect_error(
      visual_htmed(mediation.form = format_surv, outcome = 'outcome.1'),
      regexp = "treatment and/or mediator columns do not exist"
    )
  }
)


test_that(
  desc = "Catch errors related to wrong arguments passed to visualization()",
  code = {
    out <- 'outcome.2'
    expect_error(
      visual_htmed(mediation.form = format_lm, outcome = out),
      regexp = paste('None of the mediation models for', out, 'presented statistically significant values')
    )
  }
)


## ----------------------------------------------------------------------------
## Errors in visualizing results for graph_htmed()
## ----------------------------------------------------------------------------
test_that(
  desc = "Catch errors related to wrong arguments passed to visualization()",
  code = {
    expect_error(
      graph_htmed(mediation.form = format_surv$outcome.1, outcome = 'outcome.1'),
      regexp = "mediation.form is not a list"
    )
    format_surv_list <- list()
    format_surv_list[['surv']] <- format_surv
    expect_error(
      graph_htmed(mediation.form = format_surv_list, outcome = 'surv'),
      regexp = "mediation.form is not a list of DataFrames"
    )
    expect_error(
      graph_htmed(mediation.form = format_surv, outcome = lm),
      regexp = "outcome is not a character"
    )
    expect_error(
      graph_htmed(mediation.form = format_surv, outcome = 'outcome'),
      regexp = "The outcome is not in mediation.form"
    )
  }
)


test_that(
  desc = "Modifying DataFrame",
  code = {
    format_surv$outcome.1 <- format_surv$outcome.1 %>% rename('p-value_PropMed'='p-value_Prop._Mediated_(average)')
    expect_error(
      graph_htmed(mediation.form = format_surv, outcome = 'outcome.1'),
      regexp = "Wrong columns in the outcome chosen"
    )
  }
)


test_that(
  desc = "Modifying DataFrame",
  code = {
    format_surv$outcome.1 <- format_surv$outcome.1 %>% rename('treat'='treatment')
    expect_error(
      graph_htmed(mediation.form = format_surv, outcome = 'outcome.1'),
      regexp = "treatment and/or mediator columns do not exist"
    )
  }
)


test_that(
  desc = "Catch errors related to wrong arguments passed to visualization()",
  code = {
    out <- 'outcome.2'
    expect_error(
      graph_htmed(mediation.form = format_lm, outcome = out),
      regexp = paste('None of the mediation models for', out, 'presented statistically significant values')
    )
  }
)

