% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combine_stats.R
\name{add_stats}
\alias{add_stats}
\title{Combine stats and compare players}
\usage{
add_stats(award, stats, top = NULL, bot = NULL, start = NULL, end = NULL)
}
\arguments{
\item{award}{The award to compare players for (auoted)}

\item{stats}{Vector of stats to combine (quoted)}

\item{top}{Enter a number (x) to show the top x players in terms of the stat}

\item{bot}{Enter a number (y) to show the bottom y players in terms of the stat}

\item{start}{Season to start at}

\item{end}{Season to end at}
}
\value{
A data frame with rankings of players
}
\description{
Combine stats and compare players
}
\examples{
add_stats("MIP", c("PTS", "TRB", "AST"), start = 1990)
}
