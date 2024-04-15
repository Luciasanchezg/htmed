
#' @importFrom ggplot2 ggplot geom_point facet_wrap scale_color_gradient2 ggtitle labs scale_x_discrete theme_light theme arrow unit guide_axis element_rect aes
#' @importFrom ggraph ggraph geom_edge_arc circle scale_edge_width scale_edge_colour_gradient2 geom_node_point geom_node_text
#' @importFrom igraph layout_in_circle graph_from_data_frame V vcount
#' @importFrom stats na.omit
#' @importFrom rlang sym
#' @importFrom dplyr %>% mutate case_when rename
#' @importFrom rlang := .data
NULL

################################################################################
# Functions to visualize the mediation model results
################################################################################

#' Scatterplot with high-throughput causal mediation analyses
#'
#' @description `visual_htmed()` enables to visualize the results of the causal
#'   mediation analyses for a specific outcome in a single scatterplot.
#'
#'   This function also allows to filter results by p-value, giving the column
#'   that contains this information and the threshold of significance. If not,
#'   the scatterplot will display all results, without taking into account the
#'   significance reported.
#'
#'   In case split argument is provided, as many scatterplots as different
#'   levels in the split column will be generated.
#'
#' @param mediation.form lists of lists with the summary of the mediation
#'   analyses
#' @param outcome a character indicating the name of the outcome the user wants
#'   to visualice
#' @param pval.column a character with the name of the column that contains the
#'   p-values. Default: NULL
#' @param pval a number establishing the threshold for the `pval.column`
#' @param prop.med a character indicating the column with the Prop.Mediated
#'   information. Default: Estimate_Prop._Mediated_(average).
#' @param acme a character indicating the column with the ACME information.
#'   Default: Estimate_ACME_(average).
#' @param treatment a character with the name of the column containing the
#'   treatment information. Default: treatment
#' @param mediator a character with the name of the column containing the
#'   mediator information. Default: mediator.
#' @param split a character indicating the name of the column used for the
#'   split. Default: NULL
#'
#' @return returns a scatterplot with the treatments in the X axis and the mediators in the Y axis.
#' The size of the dots will indicate the proportion of mediation. The color of the dots refers to the estimator of mediation.
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
  checks <- .checks_visual(mediation.form=mediation.form, outcome=outcome,
                           pval.column = pval.column, pval = pval,
                           prop.med = prop.med, acme = acme,
                           treatment = treatment, mediator = mediator,
                           split = split)

  if (checks) {
    mediation.form[[outcome]] <- .filtering_pval(mediation.form = mediation.form, outcome = outcome,
                            pval.column = pval.column, pval = pval,
                            prop.med = prop.med, acme = acme)

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
        scale_x_discrete(guide = guide_axis(angle = 45)) +
        theme_light()
    }
    else {
      p <- ggplot(data=mediation.form[[outcome]]) +
        geom_point(aes(x=!!rlang::sym(treatment), y = factor(!!rlang::sym(mediator)), size=!!rlang::sym(prop.med), color=!!rlang::sym(acme))) +
        scale_color_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, na.value = "transparent") +
        ggtitle(paste("Results for outcome:", outcome)) +
        labs(x = "Treatment", y = "Mediator", size="Prop.med", col="Est.med") +
        scale_x_discrete(guide = guide_axis(angle = 45)) +
        theme_light()
    }

    return(p)
  }
}


#' Graph summary of the high-throughput causal mediation analysis
#'
#' @description `graph_htmed()` enables to visualize the results of the causal
#'   mediation analyses for a specific outcome in a single graph
#'
#'   This function also allows to filter results by p-value, giving the column
#'   that contains this information and the threshold of significance. If not,
#'   the scatterplot will display all results, without taking into account the
#'   significance reported.
#'
#'   In case split argument is provided, as many graphs as different
#'   levels in the split column will be generated.
#'
#' @param mediation.form lists of lists with the summary of the mediation
#'   analyses
#' @param outcome a character indicating the name of the outcome the user wants
#'   to visualice
#' @param pval.column a character with the name of the column that contains the
#'   p-values. Default: NULL
#' @param pval a number establishing the threshold for the `pval.column`
#' @param prop.med a character indicating the column with the Prop.Mediated
#'   information. Default: Estimate_Prop._Mediated_(average).
#' @param acme a character indicating the column with the ACME information.
#'   Default: Estimate_ACME_(average).
#' @param treatment a character with the name of the column containing the
#'   treatment information. Default: treatment
#' @param mediator a character with the name of the column containing the
#'   mediator information. Default: mediator.
#' @param split a character indicating the name of the column used for the
#'   split. Default: NULL
#'
#' @return returns a graph with an inner circle (treatments) and an outer circle (mediators).
#' The width of the edges will indicate the proportion of mediation. The color of the edges refers to the estimator of mediation.
#' @export
#'
graph_htmed <- function(
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
  checks <- .checks_visual(mediation.form = mediation.form, outcome = outcome,
                           pval.column = pval.column, pval = pval,
                           prop.med = prop.med, acme = acme,
                           treatment = treatment, mediator = mediator,
                           split = split)

  if (checks) {
    tabl <- .filtering_pval(mediation.form = mediation.form, outcome = outcome,
                            pval.column = pval.column, pval = pval,
                            prop.med = prop.med, acme = acme)

    ########
    '%!in%' <- function(x,y)!('%in%'(x,y))
    ########

    # nodes
    nodes <- data.frame(
      name = c(unique(tabl[[mediator]]), unique(tabl[[treatment]]))
      )
    n.nodes <- length(unique(c(tabl[[mediator]], tabl[[treatment]])))

    # edges
    relations <- stats::na.omit(tabl) %>%
      rename("from" = treatment, 'to' = mediator, 'Prop.med' = prop.med, 'Est.med' = acme) %>%
      dplyr::select(c('from', 'to', 'Prop.med', 'Est.med', split))

    if ( !is.null(split) ) {
      pList <- list()

      for (i in levels(factor(relations[[split]]))) {
        relations.i <- relations %>% dplyr::filter(!!rlang::sym(split) == i)

        nodes.i <- nodes %>%
          dplyr::mutate(vertex.color = case_when(name %in% relations.i$from ~ '#FDA855',
                                                 name %in% relations.i$to ~ '#0180AB',
                                                 name %in% tabl[[treatment]] ~ '#FBC495',
                                                 name %in% tabl[[mediator]] ~ '#BDD6D0')) %>%
          dplyr::mutate(vertex.color.label = case_when(name %in% unique(c(t(relations.i))) ~ '#000000',
                                                       name %!in% unique(c(t(relations.i))) ~ '#666666'))

        g <- igraph::graph_from_data_frame(relations.i, directed=TRUE, vertices=nodes.i)
        coords <-.layout_in_circles(g, group=igraph::V(g)$name %!in% tabl[[treatment]]) %>% as.data.frame()
        lay <- ggraph::create_layout(graph = g, layout = 'manual', x = coords$V1, y = coords$V2)

        pList[[i]] <- .graph_ggraph(layout_graph=lay, n.nodes=n.nodes)
      }
      return(pList)
    }
    else {
      # node color
      nodes <- nodes %>%
        #morado_y_verde
        # dplyr::mutate(vertex.color = case_when(name %in% relations$from ~ '#7AAF60',
        #                                        name %in% relations$to ~ '#b272ba',
        #                                        name %in% tabl[[treatment]] ~ '#d7edcc',
        #                                        name %in% tabl[[mediator]] ~ '#f7e1fa')) %>%
        # dplyr::mutate(vertex.color.label = case_when(name %in% unique(c(t(relations))) ~ '#424242',
        #                                              name %!in% unique(c(t(relations))) ~ '#666666'))
        #azul_y_naranja
        dplyr::mutate(vertex.color = case_when(name %in% relations$from ~ '#FDA855',
                                               name %in% relations$to ~ '#0180AB',
                                               name %in% tabl[[treatment]] ~ '#FBC495',
                                               name %in% tabl[[mediator]] ~ '#BDD6D0')) %>%
        dplyr::mutate(vertex.color.label = case_when(name %in% unique(c(t(relations))) ~ '#000000',
                                                     name %!in% unique(c(t(relations))) ~ '#666666'))
      # graph
      g <- igraph::graph_from_data_frame(relations, directed=TRUE, vertices=nodes)
      coords <-.layout_in_circles(g, group=igraph::V(g)$name %!in% tabl[[treatment]]) %>% as.data.frame()
      lay <- ggraph::create_layout(graph = g, layout = 'manual', x = coords$V1, y = coords$V2)

      return(.graph_ggraph(layout_graph=lay, n.nodes=n.nodes))
    }
  }
}


.checks_visual <- function(
    mediation.form,
    outcome,
    pval.column,
    pval,
    prop.med,
    acme,
    treatment,
    mediator,
    split
    ) {
  if (!"list" %in% class(mediation.form)) {
    stop("mediation.form is not a list")
  }

  if ( any(unlist(lapply(mediation.form, FUN=function(sublist) { !'data.frame' %in% class(sublist) }))) ) {
    stop("mediation.form is not a list of DataFrames")
  }

  if (!"character" %in% class(outcome)) {
    stop("outcome is not a character")
  }

  if (!"character" %in% class(treatment)) {
    stop("treatment is not a character")
  }

  if (!"character" %in% class(mediator)) {
    stop("mediator is not a character")
  }

  if (!"character" %in% class(prop.med)) {
    stop("prop.med is not a character")
  }

  if (!"character" %in% class(acme)) {
    stop("acme is not a character")
  }

  if (!outcome %in% names(mediation.form)) {
    stop("The outcome is not in mediation.form")
  }

  if ( !all(c(prop.med, acme) %in% colnames(mediation.form[[outcome]])) ) {
    stop("Wrong columns for proportion of mediation and/or ACME in the outcome chosen")
  }

  if ( !all(c(mediator, treatment) %in% colnames(mediation.form[[outcome]])) ) {
    stop("treatment and/or mediator columns do not exist")
  }

  if (!is.null(pval.column)) {

    if (!"character" %in% class(pval.column)) {
      stop("pval.column is not a character")
    }

    if ( !pval.column %in% colnames(mediation.form[[outcome]]) ) {
      stop("pval.column is not in the dataset")
    }

    if (!"numeric" %in% class(pval)) {
      stop("pval is not a number")
    }

    if (sign(pval) == '-1') {
      stop("Negative values not supported by pval")
    }
  }

  if (!is.null(split)) {

    if (!"character" %in% class(split)) {
      stop("split is not a character")
    }

    if ( !split %in% colnames(mediation.form[[outcome]]) ) {
      stop("split is not in the dataset")
    }
  }

  return(TRUE)
}

.filtering_pval <- function(
    mediation.form,
    outcome,
    pval.column,
    pval,
    prop.med,
    acme
    ) {
  if ( !is.null(pval.column) ) {

    message(paste('Results with', pval.column, '<=', pval, 'will be filtered out'))

    # filtering results giving a p-value (default: 0.05)
    mediation.form[[outcome]]  <- mediation.form[[outcome]] %>%
      dplyr::mutate(!!rlang::sym(pval.column) := as.numeric(!!rlang::sym(pval.column)),
                    !!rlang::sym(pval.column) := replace(!!rlang::sym(pval.column), !!rlang::sym(pval.column) > pval, NA )) %>%

      dplyr::mutate(!!rlang::sym(prop.med) := as.numeric(!!rlang::sym(prop.med)),
                    !!rlang::sym(prop.med) := case_when(is.na(!!rlang::sym(pval.column))~NA, TRUE ~ !!rlang::sym(prop.med) )) %>%

      dplyr::mutate(!!rlang::sym(acme) := as.numeric(!!rlang::sym(acme)),
                    !!rlang::sym(acme) := case_when(is.na(!!rlang::sym(pval.column))~NA, TRUE ~ !!rlang::sym(acme) ))

    x <- mediation.form[[outcome]] %>%
      filter(!is.na(!!rlang::sym(pval.column)))
    if (dim(x)[1] == 0) {
      stop(paste('None of the mediation models for', outcome, 'presented statistically significant values, for the p-value chosen'))
    }
  }
  else {
    message("pval.column argument not provided. Results without filtering data will be displayed")
    # pval <- NULL
    mediation.form[[outcome]] <- mediation.form[[outcome]]
  }
  return(mediation.form[[outcome]])
}


.layout_in_circles <- function(
    graph,
    group=1
    ) {
  layout <- lapply(split(igraph::V(graph), group), function(x) {
    igraph::layout_in_circle(igraph::induced_subgraph(graph,x))
  })
  layout <- Map(`*`, layout, seq_along(layout))
  x <- matrix(0, nrow=igraph::vcount(graph), ncol=2)
  split(x, group) <- layout
  x
}


.graph_ggraph <- function(
    layout_graph,
    n.nodes
    ) {
  ggraph::ggraph(layout_graph) +
    # edges
    ggraph::geom_edge_arc(
      aes(
        col = .data$Est.med,
        width = .data$Prop.med
      ),
      # curvature=0.1,
      strength=0.1,
      arrow = arrow(length = unit(4, 'mm')),
      start_cap = ggraph::circle(1, 'mm'),
      end_cap = ggraph::circle(ifelse(n.nodes < 30, 5+n.nodes/10, 6+n.nodes/50), 'mm')
    ) +
    ggraph::scale_edge_width(range = c(.5, 2)) +
    ggraph::scale_edge_colour_gradient2(
      low = "blue",
      high = "red",
      mid = "white",
      midpoint = 0,
      na.value = "transparent"
    ) +
    #nodes
    ggraph::geom_node_point(
      # size = ifelse(n.nodes < 30, 30, 20+300/n.nodes),
      size = ifelse(n.nodes < 30, 12, 10+200/n.nodes),
      col = layout_graph$vertex.color
    ) +
    ggraph::geom_node_text(
      aes(
        label = .data$name
      ),
      col = layout_graph$vertex.color.label,
      repel = FALSE,
      size = ifelse(n.nodes < 30, 2, 1.5+30/n.nodes)
    ) +
    ggplot2::theme(panel.background = element_rect(fill = 'white', colour = 'transparent'))
}
