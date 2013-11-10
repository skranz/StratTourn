\name{study.strats.and.answers}
\alias{study.strats.and.answers}
\title{A helper function to find best answers to a strategy}
\usage{
  study.strats.and.answers(strats, answers,
    strat.par = NULL, answer.par = NULL, game = NULL,
    delta = 0.9, R = 5, extra.strat.par = NULL,
    extra.answer.par = NULL, ci = 0.9, sim,
    score.fun = "efficiency-2*instability-20*instability^2",
    game.fun = NULL, game.par = NULL)
}
\arguments{
  \item{strats}{a named list of strategies to study}

  \item{answers}{a named list of answer strategies to
  study}

  \item{strat.par}{either NULL or a list with different
  parameters of strat that shall be studied. (May not run
  if strats contains more than one strategy)}

  \item{answer.par}{either NULL or a list with different
  parameters of the answer strategy strat that shall be
  studied.}

  \item{sim}{results from a previous call to
  study.strat.and.answer. New simulations will just be
  added.}

  \item{R}{number of repetions of the simulated matches}

  \item{delta}{a discount factor or a vector of different
  discount factors that shall be studied}

  \item{ci}{a number between 0 and 1 describing the
  confidence niveau of the expected payoffs that will be
  shown in the plots}

  \item{score.fun}{a string containing the formula for the
  score function}

  \item{game}{a game object, if NULL then game.fun must be
  provided}

  \item{game.fun}{a function that generates a game object,
  like make.pd.game. If NULL game must be provided}

  \item{game.par}{either NULL or a list with values for
  game parameters that shall be studied (a game parameter
  is an argument of game.fun)}
}
\description{
  The function is based on simulation.study in sktools
}

