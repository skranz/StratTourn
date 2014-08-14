
example.random.group.matchings = function() {
  n = 4
  n.strat = 10
}

random.group.matchings = function(strat, game, n = game$n,n.strat = length(strat)) {
  order = sample.int(n.strat)
  mat = suppressWarnings(matrix(order, ncol=n, byrow=TRUE))
}

#' Returns default matchings
all.vs.all.matchings = function(strat,game) {
  n = game$n
  stopifnot(n==2)
  strat.id = seq_along(strat)
  matchings = as.matrix(expand.grid(strat.id, strat.id)[,2:1])
  matchings
}

first.vs.all.matchings = function(strat,game) {
  n = game$n
  stopifnot(n==2)
  strat.id = seq_along(strat)
  as.matrix(rbind(expand.grid(1, strat.id),expand.grid(strat.id[-1],1)))
}

#' Inits a tournament object
init.tournament = function(strat, game, matchings=NULL, score.fun = "u", team=NULL, rs.file=NULL, dir=getwd(), tourn.id=NULL) {
  
  restore.point("init.tournament")
  
  strat.id = seq_along(strat)
  if (is.null(matchings) & game$n==2) {
    matchings = all.vs.all.matchings(game=game, strat=strat)
  }
  
  if (is.null(names(strat)))
    names(strat) = paste0("strat", strat.id)
    
  time.str = gsub("[-:]","",as.character(now()))
  
  if (is.null(tourn.id)) {
    id = paste0("Tourn_",game$name,"_",time.str)
    tourn.id = gsub(" ","_",id)
  }
  if (is.null(team))
    team = rep("", length(strat))
  
  for (s in seq_along(strat)) {
    attr(strat[[s]],"team.name")=team[s]
  }
  if (is.null(rs.file)) {
    rs.file = paste0(dir,"/", tourn.id, "_rs.csv")
  }
  
  tourn = list(tourn.id=tourn.id,strat = strat, game = game, team=team, matchings = matchings, dt=NULL, score.fun = score.fun, rs.file=rs.file)
  class(tourn) = c("Tournament","list")
  return(tourn)
}

#' Runs a tournament with R repetitions of each matching and add these rounds to the tournament objects
#' 
#' By setting backup.each.R to a number, say 10, a backup of the tournament will be created after each 10 repetitions
run.tournament = function(tourn, strat=tourn$strat, matchings=tourn$matchings, game=tourn$game, delta=game$delta, R = 5, LAPPLY=lapply, verbose=interactive()*1, do.store=FALSE,matchings.fun=random.group.matchings,  fixed.matchings = !is.null(matchings),...) {
  restore.point("run.tournament")
  
  dt.li = LAPPLY(1:R, function(r) {
    set.random.state(".GLOBAL")
    game.seed = draw.seed()
    if (verbose >=1)
      cat(paste0("\n",r," game.seed = ", game.seed, " "))
    if (!fixed.matchings) {
      matchings = matchings.fun(game=game,strat=strat)
    }
    res.li = lapply(1:NROW(matchings), function(i) {
      restore.point("one.match")
       
      ind = as.numeric(matchings[i,])
      strat.pair = strat[ind]
      res = run.rep.game(delta=delta,strat=strat.pair,game=game,detailed.return = FALSE, game.seed = game.seed, do.store=do.store,...)
      names = names(strat)[ind]
      if (verbose>=1) {
        cat(".")
      }
      # Save round data
      if (r==1 & i==1 & !isTRUE(file.exists(tourn$rs.file))) {
          write.table(res$rs, file=tourn$rs.file, row.names=FALSE, sep=",") 
      } else {
        write.table(res$rs, file=tourn$rs.file, append=TRUE, col.names=FALSE,row.names=FALSE, sep=",")         
      }
      return(res$res)
    })
    rbindlist(res.li)
  })
  dt = rbindlist(dt.li)
  if (!is.null(tourn$dt)) {
    tourn$dt = rbind(tourn$dt,dt, use.names=FALSE)
  } else {
    tourn$dt = dt
  }
  return(tourn)
}

tournament.stats = function(dt=tourn$dt, tourn) {
  get.tourn.rank(dt)
}

get.tourn.rank = function(dt=tourn$dt, tourn) {
  select(get.var.tourn.stats(var="u",dt=dt),-var)
}

print.Tournament = function(tourn) {
  print(as.list(tourn))
  #print(tourn$dt)
  #print(tournament.stats(dt=tourn$dt))
}

#' Saves a tournament to a file
save.tournament = function(tourn,path=getwd(),file=NULL, add.stats = FALSE) {
  if (is.null(file)) {
    #tourn$prev.backup.num = (tourn$prev.backup.num) %% 2 +1
    #file = paste0(tourn$tourn.id,"_V",tourn$prev.backup.num,".Rdata")
    file = paste0(tourn$tourn.id,".Rdata")

  }
  fn = paste0(path,"/",file)
  
  save(tourn, file=fn)
  message("Tournament saved under ", fn)
  invisible(tourn)
}

#' Loads a tournament from a file
load.tournament = function(file) {
  load(file,verbose=TRUE)
  return(tourn)
}
