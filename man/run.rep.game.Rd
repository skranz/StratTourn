\name{run.rep.game}
\alias{run.rep.game}
\title{Runs a repeated game}
\usage{
  run.rep.game(delta = game$param$delta, game, strat,
    T.min = 1, T.max = round(runif(1, 10000, 12000)),
    detailed.return = TRUE, strat.seed = NULL,
    game.seed = NULL, do.store = TRUE, strat.par = NULL,
    cap.numbers = FALSE)
}
\arguments{
  \item{delta}{discount factor}

  \item{game}{the game object}

  \item{strat}{a list of strategies}

  \item{T.min}{the minium number of periods that will be
  played. Payoffs for periods t=1,...,T.min will be
  disocunted with delta^(t-1). After any period t>=T the
  game stops with probability 1-delta and all payoffs are
  discounted with delta^(T-1).}

  \item{T.max}{optionally a maximum number of rounds}
  
  \item{cap.numbers}{defines whether and to which extend all numerics of the result are returned. You may choose between FALSE (= off), TRUE (= rounding to 2 digits) and a single number indicating the number of digits. Activating cap.numbers does not influence the behavior within the game.}
}
\description{
  Runs a repeated game
}

