
#' @import ggplot2
NULL

#' Visualization of high-throughput causal mediation analysis
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
