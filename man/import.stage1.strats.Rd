\name{import.stage1.strats}
\alias{import.stage1.strats}
\title{Import all strategies from a directory with all team's solutions (as Rmd files) from stage 1 of a tournament}
\usage{
  import.stage1.strats(dir, num.scen)
}
\arguments{
  \item{dir}{the directory in which the Rmd files are
  stored}

  \item{num.scen}{the number of scenarios}
}
\value{
  a list that contains a list for each scenario. The list
  for each scenario has a list strat that contains the
  strategies of all teams.
}
\description{
  Import all strategies from a directory with all team's
  solutions (as Rmd files) from stage 1 of a tournament
}

