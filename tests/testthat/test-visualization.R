
## ----------------------------------------------------------------------------
## Loading data
## ----------------------------------------------------------------------------
#### Data
file.tests <- "../testdata"
load(file.path(file.tests, 'format_1out.RData'))


## ----------------------------------------------------------------------------
## Tests for visualizing results
## ----------------------------------------------------------------------------
test_that(
  desc = "checking if scatter_htmed() produces the expected outcome",
  code = {

    # reading expected results
    file.tests <- "../testdata"
    load(file.path(file.tests, 'visual_1out_outcome1_noadj.RData'))

    visual_outcome1_noadj <- scatter_htmed(mediation.form = format_1out, outcome = 'MVO')

    expect_equal(visual_1out_outcome1_noadj, visual_outcome1_noadj)
  }
)


## ----------------------------------------------------------------------------
## Errors in visualizing results for scatter_htmed() OR graph_htmed()
## Format errors --------------------------------------------------------------
test_that(
  desc = "Catch errors related to wrong arguments passed to visualization()",
  code = {
    expect_error(
      scatter_htmed(mediation.form = format_1out$MVO, outcome = 'MVO'),
      regexp = "mediation.form is not a list"
    )
    format_1out_list <- list()
    format_1out_list[['surv']] <- format_1out
    expect_error(
      scatter_htmed(mediation.form = format_1out_list, outcome = 'surv'),
      regexp = "mediation.form is not a list of DataFrames"
    )
    expect_error(
      scatter_htmed(mediation.form = format_1out, outcome = lm),
      regexp = "outcome is not a character"
    )
    expect_error(
      scatter_htmed(mediation.form = format_1out, outcome = 'MVO', mediator = lm),
      regexp = "mediator is not a character"
    )
    expect_error(
      scatter_htmed(mediation.form = format_1out, outcome = 'MVO', treatment = lm),
      regexp = "treatment is not a character"
    )
    expect_error(
      scatter_htmed(mediation.form = format_1out, outcome = 'MVO', pval = '0.05', pval.column = 'pval.PropMed'),
      regexp = "pval is not a number"
    )
    expect_error(
      scatter_htmed(mediation.form = format_1out, outcome = 'MVO', pval = '0.05', pval.column = lm),
      regexp = "pval.column is not a character"
    )
    expect_error(
      scatter_htmed(mediation.form = format_1out, outcome = 'MVO', prop.med = lm),
      regexp = "prop.med is not a character"
    )
    expect_error(
      scatter_htmed(mediation.form = format_1out, outcome = 'MVO', acme = lm),
      regexp = "acme is not a character"
    )
    expect_error(
      scatter_htmed(mediation.form = format_1out, outcome = 'MVO', data.split = lm),
      regexp = "split is not a character"
    )
  }
)


## Information not in the input -----------------------------------------------
test_that(
  desc = "Catch errors related to wrong arguments passed to visualization()",
  code = {
    expect_error(
      scatter_htmed(mediation.form = format_1out, outcome = 'outcome'),
      regexp = "The outcome is not in mediation.form"
    )
    expect_error(
      scatter_htmed(mediation.form = format_1out, outcome = 'MVO', treatment = 'outcome.2'),
      regexp = "treatment and/or mediator columns do not exist"
    )
    expect_error(
      scatter_htmed(mediation.form = format_1out, outcome = 'MVO', mediator = 'outcome.2'),
      regexp = "treatment and/or mediator columns do not exist"
    )
    expect_error(
      scatter_htmed(mediation.form = format_1out, outcome = 'MVO', pval.column = 'MVO'),
      regexp = "pval.column is not in the dataset"
    )
    expect_error(
      scatter_htmed(mediation.form = format_1out, outcome = 'MVO', pval = -3, pval.column = 'adj.pval.PropMed.by_outcome'),
      regexp = "Negative values not supported by pval"
    )
    expect_error(
      scatter_htmed(mediation.form = format_1out, outcome = 'MVO', prop.med = 'MVO'),
      regexp = "Wrong columns for proportion of mediation and/or ACME in the outcome chosen"
    )
    expect_error(
      scatter_htmed(mediation.form = format_1out, outcome = 'MVO', acme = 'MVO'),
      regexp = "Wrong columns for proportion of mediation and/or ACME in the outcome chosen"
    )
    expect_error(
      scatter_htmed(mediation.form = format_1out, outcome = 'MVO', data.split = 'MVO'),
      regexp = "split is not in the dataset"
    )
  }
)


## None of the models passed the filters ----------------------------------------------
test_that(
  desc = "Catch errors related to wrong arguments passed to visualization()",
  code = {
    format_1out$MVO <- format_1out$MVO %>% dplyr::filter(`adj.pval.PropMed.by_outcome` != 0)
    expect_error(
      scatter_htmed(mediation.form = format_1out, outcome = 'MVO', pval.column = 'adj.pval.PropMed.by_outcome', pval = 0),
      regexp = "None of the mediation models for MVO presented statistically significant values, for the p-value chosen"
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
      graph_htmed(mediation.form = format_1out, outcome = 'MVO', size_node = -2),
      regexp = "size_node must be numeric and positive"
    )
    expect_error(
      graph_htmed(mediation.form = format_1out, outcome = 'MVO', size_name = 'two'),
      regexp = "size_name must be numeric"
    )
    expect_error(
      graph_htmed(mediation.form = format_1out, outcome = 'MVO', end_arrow = '-1'),
      regexp = "end_arrow must be numeric"
    )
  }
)




