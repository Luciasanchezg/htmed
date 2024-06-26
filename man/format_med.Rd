% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formatting_medresults.R
\name{format_med}
\alias{format_med}
\title{Formatting results from causal mediation analysis}
\usage{
format_med(mediation.list, split = FALSE)
}
\arguments{
\item{mediation.list}{list of lists with as many sublists as different
outcomes has been tested in the causal mediation analysis. The first level
of this list will contain the different outcomes that have been analised.
The second level, will contain the mediate objects with the analyses
information.}

\item{split}{a boolean. This argument indicate if we are dealing with
splitted data. Default: FALSE.}
}
\value{
lists of lists with the summary of the mediate analyses and the
adjusted p-values.
}
\description{
\code{format_med()} extracts some useful information from the
causal mediation analyses. This function also computes:
\itemize{
\item Adjusted p-value computed for the models performed for one outcome.
\item Adjusted p-value by all the analyses present object. In case there is
just one outcome, both adjusted p-values will be the same.
}
}
