
## ----------------------------------------------------------------------------
## Loading data
## ----------------------------------------------------------------------------
#### Data
file.tests <- "../testdata"
load(file.path(file.tests, 'df.RData'))

load(file.path(file.tests, 'mediation_1out.RData'))
load(file.path(file.tests, 'mediation_2out.RData'))

## ----------------------------------------------------------------------------
## Tests for formatting the mediation models produced by htmed()
## ----------------------------------------------------------------------------
test_that(
  desc = "checking if format_med() produces the expected outcome",
  code = {
    # reading expected results
    file.tests <- "../testdata"
    load(file.path(file.tests, 'format_1out.RData'))
    format_results <- format_med(mediation_1out)

    expect_equal(format_results, format_1out)
  }
)


test_that(
  desc = "Catch errors related to wrong arguments passed to format_med()",
  code = {

    expect_error(
      format_med('mediation_1out'),
      regexp = "mediation.list is not a list"
    )
    expect_error(
      format_med(mediation_1out$MVO),
      regexp = "mediation.list is not a list of lists"
    )
    load(file.path(file.tests, 'med_1out.RData'))
    results <- list()
    results[['outcome']] <- lapply(med_1out$model.m.formula, FUN = function(x) {lm(as.formula(x), data=df)})
    expect_error(
      format_med(results),
      regexp = "Some of the models introduced are not mediation models"
    )
    one_list <- list()
    results <- unlist(mediation_2out, recursive=FALSE)
    out <- 'results'
    one_list[[out]] <- results
    expect_error(
      format_med(one_list),
      regexp = "Are you introducing the same model more than one time?"
    )
    expect_error(
      format_med(mediation_1out, split=TRUE),
      regexp = "Using split=TRUE, the first level in the mediation.list should be the outcomes, the second, the conditions used to split and the third, the models"
    )
    expect_error(
      format_med(mediation_1out, split='TRUE'),
      regexp = "split argument only admits logical"
    )
  }
)


## ----------------------------------------------------------------------------
## Tests for format_med() with ordinal (MASS::polr) outcome models
## ----------------------------------------------------------------------------
test_that(
  desc = "checking if format_med() produces one row per effect x category for ordinal outcomes, and unchanged output for scalar outcomes",
  code = {

    # scalar outcome: format_med() should still produce the original
    # one-row-per-effect output (ACME, ADE, Total Effect, Prop. Mediated)
    scalar_results <- format_med(mediation_1out)
    expect_equal(
      rownames(scalar_results$MVO[['LV.EDV ~ Chol']]),
      c("ACME", "ADE", "Total Effect", "Prop. Mediated")
    )

    # ordinal outcome: fit a mediate() object on a MASS::polr outcome model
    set.seed(1)
    n <- 300
    treat    <- rnorm(n)
    mediator <- 0.5 * treat + rnorm(n)
    latent   <- 0.4 * mediator + 0.3 * treat + rnorm(n)
    outcome  <- cut(latent,
                    breaks = quantile(latent, probs = seq(0, 1, 0.25)),
                    include.lowest = TRUE, labels = c("Q0", "Q1", "Q2", "Q3"))
    df_ord <- data.frame(treat = treat, mediator = mediator, outcome = outcome)

    model.m <- lm(mediator ~ treat, data = df_ord)
    model.y <- MASS::polr(outcome ~ mediator + treat, data = df_ord, Hess = TRUE)

    mediate_ord <- mediation::mediate(model.m, model.y,
                                      treat = "treat", mediator = "mediator",
                                      sims = 20)

    mediation.list <- list(outcome = list(model = mediate_ord))

    ordinal_results <- format_med(mediation.list)
    ordinal_summary <- ordinal_results$outcome[['mediator ~ treat']]

    expect_equal(nrow(ordinal_summary), 20)
    expect_equal(colnames(ordinal_summary),
                c("Estimate", "95% CI Lower", "95% CI Upper", "p-value"))
    expect_equal(
      rownames(ordinal_summary),
      c(paste0("ACME (control) (Pr(Y=", c("Q0", "Q1", "Q2", "Q3"), "))"),
        paste0("ACME (treated) (Pr(Y=", c("Q0", "Q1", "Q2", "Q3"), "))"),
        paste0("ADE (control) (Pr(Y=",  c("Q0", "Q1", "Q2", "Q3"), "))"),
        paste0("ADE (treated) (Pr(Y=",  c("Q0", "Q1", "Q2", "Q3"), "))"),
        paste0("Total Effect (Pr(Y=",   c("Q0", "Q1", "Q2", "Q3"), "))"))
    )
    expect_true(!any(grepl("Prop. Mediated|average", rownames(ordinal_summary))))
  }
)

