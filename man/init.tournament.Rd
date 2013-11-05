\name{init.tournament}
\alias{init.tournament}
\title{Inits a tournament object}
\usage{
  init.tournament(strat, game, strat.dev = NULL,
    delta = game$param$delta,
    type = ifelse(is.null(strat.dev), "stage1", "stage2"),
    score.fun = "efficiency - 2*instability - 20*instability^2")
}
\description{
  Inits a tournament object
}

