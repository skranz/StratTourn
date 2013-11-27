\name{run.stage2.tournament}
\alias{run.stage2.tournament}
\title{Runs a tournament with R repetitions of each matching and add these rounds to the tournament objects}
\usage{
  run.stage2.tournament(tourn, strat = tourn$strat,
    answers = tourn$answers, game = tourn$game,
    delta = tourn$delta, R = 1, LAPPLY = lapply,
    T.min = ceiling(log(0.01)/log(delta)),
    verbose = interactive(), do.store = FALSE)
}
\description{
  By setting backup.each.R to a number, say 10, a backup of
  the tournament will be created after each 10 repetitions
}

