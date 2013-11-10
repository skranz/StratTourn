\name{study.actions.and.states}
\alias{study.actions.and.states}
\title{Study distribution of actions and states in each period in a match of the given strategies}
\usage{
  study.actions.and.states(strats, game, delta, T = 100,
    R = 1, sim = NULL, strat.par = NULL,
    verbose = interactive())
}
\arguments{
  \item{strats}{a list of the n strategies required for a
  repeated game}

  \item{game}{the game object}

  \item{delta}{the discount factor}

  \item{T}{the number of periods that shall be studied}

  \item{R}{the number the game is repeated to get the
  distribution}

  \item{sim}{NULL or the results of a previous call,
  results of the current call with be simply added to sim.
  This allows to add repetitions for getting better
  estimates.}
}
\description{
  Study distribution of actions and states in each period
  in a match of the given strategies
}

