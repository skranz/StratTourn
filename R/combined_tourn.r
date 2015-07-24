
#' Matching with active a and passive strategies
#' 
#' Each active strat plays agains each active strat and each passive strat.
#' Passive strats do not play against each other.
#' 
#' Can be useful if we already have tournament data in which the passive strats
#' play against each other and we want it to add the active strats to this
#' existing tournament data.
#' 
#' @param active.strat named list of active strategies. Each strategy is a function
#' @param passive.strat named list of passive strategies
#' @param game game object
active.passive.matchings = function(active.strat,passive.strat,game) {
  #browser()
  restore.point("active.passive.matchings")
  n = game$n
  stopifnot(n==2)
  aid = seq_along(active.strat)
  pid = seq_along(passive.strat) + length(active.strat)
  
  left  = c(rep(aid,each=length(aid)) ,rep(aid, each=length(pid)), rep(pid, each=length(aid)))
  right = c(rep(aid,times=length(aid)),rep(pid, times=length(aid)), rep(aid, times=length(pid)))

  
  matchings = cbind(strat1=left, strat2=right)
  
  #m1 = expand.grid(aid, aid,KEEP.OUT.ATTRS = FALSE)
  #m2 = expand.grid(aid, pid,KEEP.OUT.ATTRS = FALSE)
  #m3 = expand.grid(pid, aid,KEEP.OUT.ATTRS = FALSE)
  #rownames(m1) = rownames(m2) = rownames(m3) = NULL
  #matchings = as.matrix(rbind(m1,m2,m3))
  
  matchings
}


active.passive.tourn = function(astrat, ptourn, game=ptourn$game,...) {
  restore.point("active.passive.tourn")
  
  pstrat = ptourn$strat
  
  strat = c(astrat, pstrat)
  matchings = active.passive.matchings(names(astrat), names(pstrat), game=game)
  atourn = init.tournament(strat, game, matchings=matchings,...)
  
  combined.tourn(list(atourn=atourn, ptourn=ptourn))
}


combined.tourn = function(tourns,...) {
  restore.point("combined.tourn")
  strat.li = lapply(tourns, function(tourn) tourn$strat)
  names(strat.li)=NULL
  strats = do.call("c", strat.li)

  dupl = duplicated(strats) & duplicated(names(strats))
  strats = strats[!dupl]
  
  if (any(duplicated(names(strats)))) {
    dupl.names = names(strats)[duplicated(names(strats))]
    stop(paste0("Strategies ", paste0(dupl.names, collapse=", "),
         " exist multiple times with different specifications."))
  }
  tourn = tourns[[1]]
  rs.file = sapply(tourns, function(tou) tou$rs.file)
  ctourn = list(tourns=tourns, game=tourn$game, strat=strats)
  class(ctourn) = c("CombinedTournament","list")
  ctourn
}

print.CombinedTournament = function(ctourn) {
  cat("\nCombined Tournament:\n")
  for (tourn in ctourn$tourns)
    print(tourn)
}
