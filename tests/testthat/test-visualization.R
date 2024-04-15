
## ----------------------------------------------------------------------------
## Loading data
## ----------------------------------------------------------------------------
#### Data
file.tests <- "../testdata"
load(file.path(file.tests, 'format_surv.RData'))
# load(file.path(file.tests, 'format_lm.RData'))

## ----------------------------------------------------------------------------
## Tests for visualizing results
## ----------------------------------------------------------------------------
test_that(
  desc = "checking if visual_htmed() produces the expected outcome",
  code = {

    # reading expected results
    file.tests <- "../testdata"
    load(file.path(file.tests, 'visual_surv_outcome1_noadj.RData'))

    visual_outcome1_noadj <- visual_htmed(mediation.form = format_surv, outcome = 'outcome.1')

    expect_equal(visual_surv_outcome1_noadj, visual_outcome1_noadj)
  }
)

# test_that(
#   desc = "checking if graph_htmed() produces the expected outcome",
#   code = {
#
#     # reading expected results
#     file.tests <- "../testdata"
#     load(file.path(file.tests, 'graph_surv_outcome1_noadj.RData'))
#     set.seed(1)
#     graph_outcome1_noadj <- graph_htmed(mediation.form = format_surv, outcome = 'outcome.1')
#
#     expect_equal(graph_surv_outcome1_noadj, graph_outcome1_noadj)
#   }
# )

## ----------------------------------------------------------------------------
## Errors in visualizing results for visual_htmed() OR graph_htmed()
## Format errors --------------------------------------------------------------
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
      visual_htmed(mediation.form = format_surv, outcome = 'outcome.1', mediator = lm),
      regexp = "mediator is not a character"
    )
    expect_error(
      visual_htmed(mediation.form = format_surv, outcome = 'outcome.1', treatment = lm),
      regexp = "treatment is not a character"
    )
    expect_error(
      visual_htmed(mediation.form = format_surv, outcome = 'outcome.1', pval = '0.05', pval.column = 'p-value_Prop._Mediated_(average)'),
      regexp = "pval is not a number"
    )
    expect_error(
      visual_htmed(mediation.form = format_surv, outcome = 'outcome.1', pval = '0.05', pval.column = lm),
      regexp = "pval.column is not a character"
    )
    expect_error(
      visual_htmed(mediation.form = format_surv, outcome = 'outcome.1', prop.med = lm),
      regexp = "prop.med is not a character"
    )
    expect_error(
      visual_htmed(mediation.form = format_surv, outcome = 'outcome.1', acme = lm),
      regexp = "acme is not a character"
    )
    expect_error(
      visual_htmed(mediation.form = format_surv, outcome = 'outcome.1', split = lm),
      regexp = "split is not a character"
    )
  }
)


## Information not in the input -----------------------------------------------
test_that(
  desc = "Catch errors related to wrong arguments passed to visualization()",
  code = {
    expect_error(
      visual_htmed(mediation.form = format_surv, outcome = 'outcome'),
      regexp = "The outcome is not in mediation.form"
    )
    expect_error(
      visual_htmed(mediation.form = format_surv, outcome = 'outcome.1', treatment = 'outcome.2'),
      regexp = "treatment and/or mediator columns do not exist"
    )
    expect_error(
      visual_htmed(mediation.form = format_surv, outcome = 'outcome.1', mediator = 'outcome.2'),
      regexp = "treatment and/or mediator columns do not exist"
    )
    expect_error(
      visual_htmed(mediation.form = format_surv, outcome = 'outcome.1', pval.column = 'outcome.1'),
      regexp = "pval.column is not in the dataset"
    )
    expect_error(
      visual_htmed(mediation.form = format_surv, outcome = 'outcome.1', pval = -3, pval.column = 'adj.p-value.by_outcome'),
      regexp = "Negative values not supported by pval"
    )
    expect_error(
      visual_htmed(mediation.form = format_surv, outcome = 'outcome.1', prop.med = 'outcome.1'),
      regexp = "Wrong columns for proportion of mediation and/or ACME in the outcome chosen"
    )
    expect_error(
      visual_htmed(mediation.form = format_surv, outcome = 'outcome.1', acme = 'outcome.1'),
      regexp = "Wrong columns for proportion of mediation and/or ACME in the outcome chosen"
    )
    expect_error(
      visual_htmed(mediation.form = format_surv, outcome = 'outcome.1', split = 'outcome.1'),
      regexp = "split is not in the dataset"
    )
  }
)


## None of the models passed the filters ----------------------------------------------
test_that(
  desc = "Catch errors related to wrong arguments passed to visualization()",
  code = {
    format_surv$outcome.1 <- format_surv$outcome.1 %>% dplyr::filter(`adj.p-value.by_outcome` != 0)
    expect_error(
      visual_htmed(mediation.form = format_surv, outcome = 'outcome.1', pval.column = 'adj.p-value.by_outcome', pval = 0),
      regexp = "None of the mediation models for outcome.1 presented statistically significant values, for the p-value chosen"
    )
  }
)


## ----------------------------------------------------------------------------
## Errors in visualizing results just for graph_htmed()
## Format errors --------------------------------------------------------------
test_that(
  desc = "Catch errors related to wrong arguments passed to visualization()",
  code = {
    expect_error(
      graph_htmed(mediation.form = format_surv, outcome = 'outcome.1', size_node = -2),
      regexp = "size_node must be numeric and positive"
    )
    expect_error(
      graph_htmed(mediation.form = format_surv, outcome = 'outcome.1', size_name = 'two'),
      regexp = "size_name must be numeric"
    )
    expect_error(
      graph_htmed(mediation.form = format_surv, outcome = 'outcome.1', end_arrow = '-1'),
      regexp = "end_arrow must be numeric"
    )
  }
)




