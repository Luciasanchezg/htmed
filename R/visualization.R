
#' @import ggplot2
#' @import igraph
NULL

#' Visualization of the high-throughput causal mediation analysis
#'
#' @description `visual_htmed()` enables to visualize the results of the causal
#'   mediation analyses performed in a single plot
#'
#' @param mediation.list lists of lists with the results of mediation
#' @param outcome name of the outcome the user want to visualice
#' @param ... rest of arguments passed to ggplot
#'
#' @return returns a ggplot object
#' @export
#'
visual_htmed <- function(mediation.list, outcome, ...) {
  p <- ggplot(data=mediation.list[[outcome]]) +
    geom_point(aes(x=treatment, y = factor(mediator), size=`Estimate_Prop._Mediated_(average)`, color=`Estimate_ACME_(average)`)) +
    scale_color_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, na.value = "transparent") +
    labs(x = "Treatment", y = "Mediator", size="Prop.med", col="Est.med") +
    scale_x_discrete(guide = guide_axis(angle = 45))
  return(p)
}
#p <- visual_htmed(mediation.list = filt_mediation.list, outcome = 'tsurvHF')


################################################################################
#' Graph summary of the high-throughput causal mediation analysis
#'
#' @param mediation.list lists of lists with the results of mediation
#' @param outcome name of the outcome the user want to visualice
#' @param vertex.label.cex number indicating the font size of the node
#' @param vertex.size number indicating the size of the node
#' @param edge.width number indicating the edge width
#' @param edge.arrow.size number indicating the arrow size
#' @param ... rest of arguments passed to igraph
#'
#' @return returns a ggplot object
#' @export
#'
graph_htmed <- function(
    mediation.list,
    outcome,
    vertex.label.cex=.75,
    vertex.size=16,
    edge.width = 2,
    edge.arrow.size = .5,
    ...
    ) {

  tabl <- mediation.list[[outcome]]

  # stablising the nodes
  nodes <- data.frame(
    name = c(unique(tabl$mediator), unique(tabl$treatment))
  )

  # stablising the edges
  relations <- na.omit(tabl) %>%
    rename("from" = "treatment", 'to' = 'mediator') %>% dplyr::select(c(from, to))

  g <- graph_from_data_frame(relations, directed=TRUE, vertices=nodes)

  # colors for the nodes and the edges
  V(g)[as.character(c(unique(relations$from)))]$color <- '#FFA500'
  V(g)[as.character(c(unique(relations$to)))]$color <- '#F0FFF0'

  '%!in%' <- function(x,y)!('%in%'(x,y))
  plot(
    g,
    layout=.layout_in_circles(g, group=V(g)$name %!in% tabl$treatment),
    vertex.label.cex=vertex.label.cex,
    vertex.size=vertex.size,
    edge.width=edge.width,
    edge.arrow.size=edge.arrow.size,
    edge.curved=seq(-0.25, 0.25, length = ecount(g)),
    ...
    )
}


.layout_in_circles <- function(graph, group=1) {
  layout <- lapply(split(V(graph), group), function(x) {
    layout_in_circle(induced_subgraph(graph,x))
  })
  layout <- Map(`*`, layout, seq_along(layout))
  x <- matrix(0, nrow=vcount(graph), ncol=2)
  split(x, group) <- layout
  x
}

