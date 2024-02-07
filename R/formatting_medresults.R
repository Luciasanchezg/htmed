

extract_mediation_summary <- function (x) {

  ## TODO: controlar cuando no hay control y treated, sino solo un ACME y un Prop.Mediated
  clp <- 100 * x$conf.level

  stats_model <- c(x$d0, x$d0.ci, x$d0.p)
  stats_model <- rbind(stats_model, c(x$d1, x$d1.ci, x$d1.p))
  stats_model <- rbind(stats_model, c(x$n0, x$n0.ci, x$n0.p))
  stats_model <- rbind(stats_model, c(x$n1, x$n1.ci, x$n1.p))
  stats_model <- rbind(stats_model, c(x$d.avg, x$d.avg.ci, x$d.avg.p))
  stats_model <- rbind(stats_model, c(x$n.avg, x$n.avg.ci, x$n.avg.p))

  rownames(stats_model) <- c("ACME (control)", "ACME (treated)",
                             "Prop. Mediated (control)", "Prop. Mediated (treated)",
                             "ACME (average)",
                             "Prop. Mediated (average)")

  colnames(stats_model) <- c("Estimate", paste(clp, "% CI Lower", sep = ""),
                             paste(clp, "% CI Upper", sep = ""), "p-value")

  return(as.data.frame(stats_model))
}
