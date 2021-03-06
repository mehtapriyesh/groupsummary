% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/groupagg.R
\name{groupagg}
\alias{groupagg}
\title{Grouping and aggregating using multiple variables}
\usage{
groupagg(data, grouping, agg, ...)
}
\arguments{
\item{data}{Data to be used}

\item{grouping}{The grouping variable/s to be used. In case multiple, you can pass a vector containing name of variables as strings}

\item{agg}{The variable/s to be aggregated. In case multiple, you can pass a vector containing name of variables as strings}

\item{...}{The function you'll be using to create aggregation. Ex: sum, mean, median, etc...}
}
\value{
Returning the aggregation
}
\description{
Grouping and aggregating using multiple variables
}
\examples{
grouping = "vs"
agg = "wt"
mtcars \%>\% groupagg(grouping, agg, median)
  vs    wt
<dbl> <dbl>
  1     0  3.57
  2     1  2.62

  grouping = c("cyl", "vs")
  agg = c("wt", "qsec")
  mtcars \%>\% groupagg(grouping, agg, mean)

}
