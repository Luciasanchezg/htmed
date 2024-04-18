
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hightmed

``` r
library(hightmed)
library(survival)
library(dplyr) #
library(ggraph) #
```

In this tutorial, we will use all the functions available in `hightmed`
package to illustrate an example of how high-throughput mediation
analysis could be performed.

## Data

To do so, we will make use of a dataframe that contains information of
98 mice that have died between week 26 and week 138 (this information is
stored in a column called `age_death`). The dataframe also contains
several columns indicating the sex of the mice (`sex`), and the measures
of different treatments and mediators collected in a continuous form.
There is an additional column (`HF`), that differentiates between mice
that have died as a result of heart failure or not, 1 or 0,
respectively.

``` r
data("df", package = "hightmed")
```

With this data, we hypothesize that some treatments could be responsible
of the heart failure observed in the mice, and that some mediators could
explain the underlying mechanism of the relationship between the
treatment and heart failure.

Therefore, we are interested in testing the association between each
treatment and heart failure, through each mediator. In order to perform
this mediation analyses, we first need to compute the fitted models for
mediators and outcomes, respectively, something that can be done with
`generatting_models()`. However, before applying this function, we need
to generate a new dataframe in which we will specify what combination of
treatment, mediator and outcome we want to test. The dataframe (called
`models` in this example), will be composed of as many rows as different
analyses we want to perform and five different columns: \* outcome
(characters): it contains the different outcomes that we are interested
in predict. In this example we will work with just one outcome: heart
failure. \* treatment (characters): column with the different treatments
to test. \* mediator (characters): column with the different mediators
to test. \* model.m.formula (characters): this column will inform about
the formula that needs to be apply to compute the fitted models for
mediator. \* model.y.formula (characters): similar to model.m.formula
but with the formulas for the fitted models for outcome.

``` r
# data("models_surv", package = "hightmed")
outcome <- 'outcome.1'
treatment <- paste('treatment', c(1, 2, 3, 4), sep='.')
mediator <- paste('mediator', c(1, 2, 3, 4), sep='.')
    
models <- expand.grid(outcome = outcome, treatment = treatment, mediator = mediator) %>%
  mutate(model.m.formula = paste(mediator, '~', treatment)) %>%
  mutate(model.y.formula = paste(outcome, '~', mediator, '+', treatment))
```

## Generatting models for mediators and outcomes

After generating `models`, we have all the necessary to apply the
`generating_models()` function. This function will allow us to generate
the fitted models for mediators and outcomes, for each combination of
mediator, treatment and outcome, in an iterative manner.
`generating_models()` contains 6 arguments, being mandatory 4 of them:
the dataframe with the values (`data`), the dataframe with the models to
perform (`data.models`), the name of the column that contains the model
formulas in `data.models` (`column.models`) and the statistical analysis
we are interested in applying over the data (`model.type`). There are
other 3 optional arguments: `model.m` indicates if we are computing the
fitted models for mediators (TRUE) or for outcomes (FALSE), and
`outcome` will refer to the column from the `data.models` object that
contains the information of the outcomes. Although this last argument is
optional and does not need to be specified if we have just one outcome,
it is highly recommended if you are dealing with more than one dependent
variables. The `data.split()` argument will be explained later.

``` r
# fitted models for the mediator
medANDtreat <- generating_models(
    column.models='model.m.formula'
  , model.type=lm
  , data=df
  , data.models=models
  , model.m = TRUE
  ) 

# fitted models for the outcome
medANDtreat <- generating_models(
    column.models='model.y.formula'
  , model.type=survival::survreg
  , data=df
  , data.models=medANDtreat
  , model.m = FALSE
  ) 
```

The new dataframe will contain the same information than `models`, but
with two additional columns:

-   model.M: fitted models for mediators
-   model.Y: fitted models for outcomes

## High-throughput mediation analysis

To apply high-throughput mediation, we execute the `hightmed()` function
over this data. From this point, we will just work with the dataframe
generated in the previous step `medANDtreat`. hightmed() requires the
following arguments: \* data.models (dataframe): object with the fitted
models for mediators and outcomes. \* column.modelm and column.modely
(characters): these arguments refer to the columns from `data.models`
that contain the fitted models for mediator and outcomes, respectively.
\* treat, mediator and outcome (characters): these three arguments refer
to the columns from `data.models` with the treatment, mediator and
outcome information, respectively.

There is an additional argument `seed` that can be introduced to ensure
reproducibility of results.

``` r
med_results <- hightmed(
    data.models=medANDtreat
  , column.modelm = 'model.M'
  , column.modely = 'model.Y'
  , treat='treatment'
  , mediator='mediator'
  , outcome='outcome'
  )
```

``` r
# med_results %>% View(.)

paste('Class of the mediation analysis results:', unlist(unique(lapply(med_results$outcome.1, function(x) {class(x)}))))
#> [1] "Class of the mediation analysis results: mediate"
```

`hightmed()` will generate a list of lists in which the first level will
be the different outcomes tested (one in this example), and the second
level, the mediation analyses performed.

``` r
paste('outcomes tested:', names(med_results))
#> [1] "outcomes tested: outcome.1"

paste('Analyses performed for', names(med_results), ':')
#> [1] "Analyses performed for outcome.1 :"
names(med_results$outcome.1)
#>  [1] "mediator.1 ~ treatment.1" "mediator.1 ~ treatment.2"
#>  [3] "mediator.1 ~ treatment.3" "mediator.1 ~ treatment.4"
#>  [5] "mediator.2 ~ treatment.1" "mediator.2 ~ treatment.2"
#>  [7] "mediator.2 ~ treatment.3" "mediator.2 ~ treatment.4"
#>  [9] "mediator.3 ~ treatment.1" "mediator.3 ~ treatment.2"
#> [11] "mediator.3 ~ treatment.3" "mediator.3 ~ treatment.4"
#> [13] "mediator.4 ~ treatment.1" "mediator.4 ~ treatment.2"
#> [15] "mediator.4 ~ treatment.3" "mediator.4 ~ treatment.4"
```

## Formatting the mediation results

If we explore the results, we will see that the output is in some way
difficult to understand.

``` r
#  med_results$outcome.1$`mediator.1 ~ treatment.1` %>% View(.)
```

We need to transform this data to simplify it and make it more
user-friendly for the visualizations that will be performed later. Using
`formatting_med()` with just `med_results` as input, we will generate a
dataframe with the essential columns needed for the visualizations.

This function also computes two different kinds of adjusted p-value:
adj.p-value.by_outcome and adj.p-value.all. The different between both
is that while the first one adjust by each of the outcomes, the second
adjusted p-value will take into account all the analyses from all the
outcomes. The usage of one or the other will depend on the question that
the investigator wants to answer.

``` r
# formatting data
format_results <- formatting_med(med_results)
```

## Visualization

Finally, we will be interested in visualise our results. The package has
two available functions for this purposse.

We can create a scatterplot for each outcome with the `visual_htmed()`
function. Run in the default model, just with the mandatory arguments,
we will need to specify: \* mediation_form (lists of lists): the object
that contain the results. \* outcome (characters): the outcome the user
is interested in visualising.

``` r
visual_outcome1_nosig <- visual_htmed(mediation.form = format_results, outcome = 'outcome.1')
#> pval.column argument not provided. Results without filtering data will be displayed
visual_outcome1_nosig
```

<img src="man/figures/README-visualizing-1-1.png" width="100%" /> This
scatterplot will represents, for outcome.1, the relationship between the
treatments and mediators, being the size of the dot proportional to the
proportion of mediation, and the color, the estimation of mediation.

Another visualization can be done with `graph_htmed()` function. In the
default mode of this function, the graph will display the treatments as
the internal nodes, and the mediator as the external ones. Similar to
what `visual_htmed()` does, the width of the edges is proportional to
the proportion of mediation, and the color, to the estimation of
mediation.

``` r
graph_outcome1_nosig <- graph_htmed(mediation.form = format_results, outcome = 'outcome.1')
#> pval.column argument not provided. Results without filtering data will be displayed
graph_outcome1_nosig
```

<img src="man/figures/README-visualizing-2-1.png" width="100%" />

You might have notice that we have not filtered out any mediation
analyses in the representation, which means that all results,
independently of the level of significance, are visualized. This is what
occurs by default in both `visual_htmed()` and `graph_htmed()`
functions.

Nevertheless, you can filter the data by specifying the level of
significance (`pval`) and the column to apply it (`pval.column`). In the
following chunk, we will restrict our representation to mediation
analyses with an adjusted p-value \<= 0.05.

``` r
visual_outcome1_adj0.05 <- visual_htmed(
  mediation.form = format_results
  , outcome = 'outcome.1'
  , pval.column = 'adj.p-value.by_outcome'
  , pval = 0.05)
#> Results with adj.p-value.by_outcome <= 0.05 will be filtered out
visual_outcome1_adj0.05
#> Warning: Removed 11 rows containing missing values or values outside the scale range
#> (`geom_point()`).
```

<img src="man/figures/README-visualizing-3-1.png" width="100%" />

``` r
graph_outcome1_adj0.05 <- graph_htmed(
  mediation.form = format_results
  , outcome = 'outcome.1'
  , pval.column = 'adj.p-value.by_outcome'
  , pval = 0.05)
#> Results with adj.p-value.by_outcome <= 0.05 will be filtered out
graph_outcome1_adj0.05
```

<img src="man/figures/README-visualizing-4-1.png" width="100%" />
