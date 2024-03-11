
#' @import ggplot2
#' @importFrom igraph plot.igraph layout_in_circle graph_from_data_frame ecount V vcount
#' @importFrom stats na.omit
#' @importFrom rlang sym
#' @importFrom dplyr mutate
NULL

################################################################################
# Functions to visualize the mediation model results
################################################################################

#' Visualization of the high-throughput causal mediation analysis
#'
#' @description `visual_htmed()` enables to visualize the results of the causal
#'   mediation analyses performed in a single plot
#'
#' @param mediation.form lists of lists with the results of mediation
#' @param outcome a character indicating the name of the outcome the user wants
#'   to visualice
#' @param pval.column a character with the name of the column that contains the
#'   p-values
#' @param pval a number establishing the threshold for the `pval.column`
#' @param prop.med a character indicating the column with the Prop.Mediated
#'   information (Default: Estimate_Prop._Mediated_(average))
#' @param acme a character indicating the column with the ACME information
#'   (Default: Estimate_ACME_(average))
#' @param treatment a character with the name of the column containing the
#'   treatment information
#' @param mediator a character with the name of the column containing the
#'   mediator information
#' @param split a character indicating the name of the column used for the split
#'
#' @return returns a ggplot object
#' @export
#'
visual_htmed <- function(
    mediation.form,
    outcome,
    pval.column = NULL,
    pval = 0.05,
    prop.med = 'Estimate_Prop._Mediated_(average)',
    acme = 'Estimate_ACME_(average)',
    treatment = 'treatment',
    mediator = 'mediator',
    split = NULL
    ) {
  checks <- .checks_visual(mediation.form=mediation.form, outcome=outcome)
  if (checks) {

    if ( !is.null(pval.column) ) {

      # filtering results giving a p-value (default: 0.05)
      mediation.form[[outcome]]  <- mediation.form[[outcome]] %>%
        dplyr::mutate(!!rlang::sym(pval.column) := as.numeric(!!rlang::sym(pval.column)),
                      !!rlang::sym(pval.column) := replace(!!rlang::sym(pval.column), !!rlang::sym(pval.column) >= pval, NA )) %>%

        dplyr::mutate(!!rlang::sym(prop.med) := as.numeric(!!rlang::sym(prop.med)),
                      !!rlang::sym(prop.med) := case_when(is.na(!!rlang::sym(pval.column))~NA, TRUE ~ !!rlang::sym(prop.med) )) %>%

        dplyr::mutate(!!rlang::sym(acme) := as.numeric(!!rlang::sym(acme)),
                      !!rlang::sym(acme) := case_when(is.na(!!rlang::sym(pval.column))~NA, TRUE ~ !!rlang::sym(acme) ))

    }
    else {
      pval <- NULL
      mediation.form[[outcome]] <- mediation.form[[outcome]]
    }

    if ( !is.null(split) ) {
      # generating a new column called split
      mediation.form[[outcome]]  <- mediation.form[[outcome]] %>%
        dplyr::mutate(split := !!rlang::sym(split))

      p <- ggplot(data=mediation.form[[outcome]]) +
        geom_point(aes(x=!!rlang::sym(treatment), y = factor(!!rlang::sym(mediator)), size=!!rlang::sym(prop.med), color=!!rlang::sym(acme))) +
        facet_wrap(~factor(split), nrow=1) +
        scale_color_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, na.value = "transparent") +
        ggtitle(paste("Results for outcome:", outcome)) +
        labs(x = "Treatment", y = "Mediator", size="Prop.med", col="Est.med") +
        scale_x_discrete(guide = guide_axis(angle = 45))
    }
    else {
      p <- ggplot(data=mediation.form[[outcome]]) +
        geom_point(aes(x=!!rlang::sym(treatment), y = factor(!!rlang::sym(mediator)), size=!!rlang::sym(prop.med), color=!!rlang::sym(acme))) +
        scale_color_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, na.value = "transparent") +
        ggtitle(paste("Results for outcome:", outcome)) +
        labs(x = "Treatment", y = "Mediator", size="Prop.med", col="Est.med") +
        scale_x_discrete(guide = guide_axis(angle = 45))
    }

    return(p)
  }
}


#' Graph summary of the high-throughput causal mediation analysis
#'
#' @param mediation.form lists of lists with the results of mediation
#' @param outcome name of the outcome the user want to visualice
#' @param vertex.label.cex number indicating the font size of the node
#' @param vertex.size number indicating the size of the node
#' @param edge.width number indicating the edge width
#' @param edge.arrow.size number indicating the arrow size
#' @param ... rest of arguments passed to igraph
#'
#' @return prints out a graph from igraph package
#' @export
#'
graph_htmed <- function(
    mediation.form,
    outcome,
    vertex.label.cex=.75,
    vertex.size=16,
    edge.width = 2,
    edge.arrow.size = .5,
    ...
    ) {
  checks <- .checks_visual(mediation.form=mediation.form, outcome=outcome)
  if (checks) {
    tabl <- mediation.form[[outcome]]

    # stablising the nodes
    nodes <- data.frame(
      name = c(unique(tabl[['mediator']]), unique(tabl[['treatment']]))
    )

    # stablising the edges
    relations <- stats::na.omit(tabl) %>%
      rename("from" = "treatment", 'to' = 'mediator') %>% dplyr::select(c('from', 'to'))

    g <- igraph::graph_from_data_frame(relations, directed=TRUE, vertices=nodes)

    # colors for the nodes and the edges
    igraph::V(g)[as.character(c(unique(relations[['from']])))]$color <- '#FFA500'
    igraph::V(g)[as.character(c(unique(relations[['to']])))]$color <- '#F0FFF0'

    '%!in%' <- function(x,y)!('%in%'(x,y))
    igraph::plot.igraph(
      g,
      layout=.layout_in_circles(g, group=igraph::V(g)$name %!in% tabl[['treatment']]),
      vertex.label.cex=vertex.label.cex,
      vertex.size=vertex.size,
      edge.width=edge.width,
      edge.arrow.size=edge.arrow.size,
      edge.curved=seq(-0.25, 0.25, length = igraph::ecount(g)),
      ...
    )
  }
}


.checks_visual <- function(mediation.form, outcome) {
  if (!"list" %in% class(mediation.form)) {
    stop("mediation.form is not a list")
  }

  if ( any(unlist(lapply(mediation.form, FUN=function(sublist) { !'data.frame' %in% class(sublist) }))) ) {
    stop("mediation.form is not a list of DataFrames")
  }

  if (!"character" %in% class(outcome)) {
    stop("outcome is not a character")
  }

  if (!outcome %in% names(mediation.form)) {
    stop("The outcome is not in mediation.form")
  }

  if ( !all(c('p-value_Prop._Mediated_(average)', 'Estimate_Prop._Mediated_(average)', 'Estimate_ACME_(average)') %in% colnames(mediation.form[[outcome]])) ) {
    stop("Wrong columns in the outcome chosen")
  }

  if ( !all(c('mediator', 'treatment') %in% colnames(mediation.form[[outcome]])) ) {
    stop("treatment and/or mediator columns do not exist")
  }

  x <- mediation.form[[outcome]] %>%
    filter(!is.na(as.character('p-value_Prop._Mediated_(average)')), !is.na(as.character('Estimate_Prop._Mediated_(average)')), !is.na(as.character('Estimate_ACME_(average)')))
  if (dim(x)[1] == 0) {
    stop(paste('None of the mediation models for', outcome, 'presented statistically significant values'))
  }

  return(TRUE)
}


.layout_in_circles <- function(graph, group=1) {
  layout <- lapply(split(igraph::V(graph), group), function(x) {
    igraph::layout_in_circle(igraph::induced_subgraph(graph,x))
  })
  layout <- Map(`*`, layout, seq_along(layout))
  x <- matrix(0, nrow=igraph::vcount(graph), ncol=2)
  split(x, group) <- layout
  x
}

