% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hotelling_game.r
\name{the.undercutter}
\alias{the.undercutter}
\title{Example strategy (Hotelling Game): the.undercutter}
\usage{
the.undercutter(obs, i, t, cut = 0.01, ...)
}
\arguments{
\item{obs}{Observations of position and price of both players in the last round as specified in the function hotelling.profits}

\item{i}{Number of current player}

\item{t}{Current period}

\item{cut}{How much lower than the opponent should the price be set?}
}
\description{
This strategy copies the location of the opponent and sets the price as "price of opponent - cut"
}
