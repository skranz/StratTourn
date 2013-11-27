\name{init.tournament}
\alias{init.tournament}
\title{Inits a tournament object}
\usage{
  init.tournament(strat, game, answers = NULL,
    delta = game$param$delta,
    type = ifelse(is.null(answers), "stage1", "stage2"),
    score.fun = "efficiency - 2*instability - 20*instability^2",
    team = NULL)
}
\description{
  Inits a tournament object
}

