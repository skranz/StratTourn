

examples.rep.game = function() {
  #assignInNamespace("cedta.override", c(data.table:::cedta.override,"StratTourn","dplyrExtrs","dplyr"), "data.table")
  
  # Generate a game object
  game = make.pd.game(err.D.prob=0.15)
  
  
  strat = forgiving.grim.trigger
  get.strat.info(game=game, strat=strat)
  # Pick a pair of strategies
  strats = nlist(net.nice,tit.for.tat)

  # Let the strategies play against each other
  set.storing(TRUE)
  run.rep.game(delta=0.9, game=game, strat = strats, detailed.return=!FALSE)
    
  # Init and run a tournament of several strategies against each other  
  set.storing(FALSE)
  game$delta = 0.9; game$sample.delta = 0.9
  strat = nlist(tit.for.tat,always.defect, net.nice0, net.nice1, forgiving.grim.trigger)
  
  setwd("D:/libraries/StratTourn/")
  library(compiler)
  enableJIT(3)
  tourn = init.tournament(game=game, strat=strat)
  Rprof(tmp <- tempfile())
  tourn = run.tournament(tourn=tourn, R = 100)
  Rprof()
  summaryRprof(tmp)
  unlink(tmp)
  
  tourn = run.tournament(tourn=tourn, R = 2)
  tourn
  save.tournament(tourn)
  # Second stage of tournament  
  strat = nlist(tit.for.tat)
  strat.dev = list(grim.trigger= nlist(always.defect, always.coop),
                   tit.for.tat = nlist(always.defect, always.coop))
  tourn = init.tournament(game=game, strat=strat,strat.dev=strat.dev, delta=0.9)
  tourn = run.tournament(tourn=tourn, R = 10)
  tourn
  
}

net.nice0 = function(obs,i,t, net.nice=0,k=0, ...) {
  net.nice(obs,i,t, net.nice,k, ...)
}
net.nice1 = function(obs,i,t, net.nice=0,k=1, ...) {
  net.nice(obs,i,t, net.nice,k, ...)
}
  

net.nice = function(obs,i,t, net.nice=0,k=1, ...) {  
  restore.point("net.nice")
  if (t==1) {
    return(nlist(a="C",net.nice))
  }
  a = obs$a
  j = 3-i  
  a.num = ifelse(a=="C",1,0)
  net.nice = net.nice + a.num[i]-a.num[j] 
  if (net.nice <= k) {
    return(nlist(a="C",net.nice))
  }
  return(nlist(a="D",net.nice))
}

forgiving.grim.trigger = function(obs,i,t,game,coop=TRUE,forgive.prob=0.2,...) {
  coop = coop & all(obs$a == "C")
  if (runif(1)<forgive.prob)
    coop = TRUE
  a = ifelse(coop,"C","D")
  list(a=a,coop=coop)
}


get.strat.info = function(i=1,strat, game) {
  restore.point("get.strat.info")
  
  ex.obs = game$example.obs(i=i)
  ex.action = game$example.action(i=i)
  
  # Extras arguments of the strategy
  args = setdiff(names(formals(strat)),c("obs", "i","t","game","...",names(game$show.par)))
  
  # Returned values of the strategy
  res = do.call(strat, c(list(obs=ex.obs,i=i,t=1), game$show.par))
  
  actions = names(ex.action)
  strat.states = setdiff(intersect(names(res), args), actions)
  strat.par = setdiff(args, c(actions,strat.states))
  
  nlist(actions, strat.states, strat.par)
  
}

get.obs.i = function(i,obs, game) {
  if (is.null(obs))
    return(NULL)
  if (game$private.signals) {
    return(obs[[i]])
    if (is.null(game$private.obs)) {
      obs.i = obs[[i]]  
    } else {
      obs.i = obs
      for (po in game$private.obs) {
        obs.i[[po]] = obs[[po]][[i]]
      }
    }
  } else {
    obs.i = obs
  }
  return(obs.i)  
  
}

examples.sample.T = function() {
  delta = 0.985
  n = 100000
  T = rnbinom(n,size=1,prob=1-delta)+1
  hist(T,breaks=50)
  sum(T>=50)/n
  T.bar = 80
  sum((T>=T.bar)*(T-T.bar))/n
  sum((T<T.bar)*(T))/n
  mean(T)
  median(T)
  tabulate(T[T<=400])
  plot(ecdf(T[T<=400]))
}

sample.T = function(delta, sample.delta = delta) {
  T = rnbinom(1,size=1,prob=1-sample.delta)+1
  if (sample.delta != delta) {
    weight = dnbinom(T-1, size=1, prob=1-delta) / dnbinom(T-1, size=1, prob=1-sample.delta)
  } else {
    weight = 1
  }
  list(T=T, T.weight=weight)
}

#' Runs a repeated game
#' @param delta discount factor
#' @param game the game object
#' @param strat a list of strategies
#' @param T.max optionally a maximum number of rounds
run.rep.game = function(delta=game$delta, game, strat, T.max = NULL,detailed.return = TRUE, strat.seed=NULL, game.seed = NULL, do.store = TRUE,strat.par=NULL, sample.delta = 1 - (1-delta)/2, match.id = sample.int(2147483647,1)) {
  restore.point("run.rep.game")
  
  gbos$do.store = do.store
   
  if (is.null(game.seed))
    game.seed = draw.seed()
  if (is.null(strat.seed))
    strat.seed = draw.seed()
  
  c(game.seed,strat.seed)
  
  #Store seeds to replicate for debugging purposes
  gbos$game.seed = game.seed
  gbos$strat.seed = strat.seed
  
  init.random.state("game",game.seed)
  init.random.state("strat",strat.seed)
  set.random.state("game")
  
  # Draw number of periods from a negative binominal distribution
  ret = sample.T(delta=delta, sample.delta=sample.delta)
  T = ret$T
  T.weight = ret$T.weight
  if (!is.null(T.max))
    T = pmin(T, T.max)

  
  strat.id = seq_along(strat)
  n.strat = length(strat.id)
  
  if (n.strat != game$n) {
    stop(paste("The game requires ",game$n, " players but you submitted ", n.strat, " strategies."))
  }
  
  if (detailed.return) {
    strat.state.str.li = vector("list",T)
  }
  
  if (game$private.signals) {
    obs = lapply(1:game$n, game$example.obs)
  } else {
    obs = game$example.obs(i=1)
  }
  
  denv = new.env()
  a = vector("list",n.strat)
  game.states = game$init.states
  
  # Strategy infos
  si = lapply(strat.id, function(i) {
    get.strat.info(i=i, strat=strat[[i]], game=game)
  })
  strat.states = lapply(strat.id, function(i) {
    list()
  })
  
  round.stats.li = vector("list",T)
  U = rep(0,game$n)
  t = 1
  for (t in 1:T) {
    if (detailed.return) {
      start.strat.states = strat.states
      start.game.states = game.states
    }
    
    # Set the random state for the strategy
    set.random.state("strat")
    
    # 1. Evaluate strategies of each player
    i = 2
    for (i in strat.id) {
      obs.i = get.obs.i(obs = obs, i = i, game = game)    
      # Use only those strat.par that are not returned as a strat.state
      act.strat.par = strat.par[[i]][setdiff(names(strat.par[[i]]),names(strat.states[[i]]))]
      args = c(list(obs = obs.i,i=i,t=t),game$show.par,game.states,strat.states[[i]], act.strat.par)
      tryCatch(
        strat.res <- do.call(strat[[i]],args),
        error = function(e) {
          message("Error in evaluating strategy ", names(strat)[i], " in period t=", t, " for player i=",i,"\nERROR.HIST:")
          hist = denv$df[1:t,]
          
          print(tail(hist))
          assign(".BOS.ERROR.HIST",hist,.GlobalEnv)
          assign(".BOS.ERROR.t",t,.GlobalEnv)
          assign(".BOS.ERROR.i",i,.GlobalEnv)
          
          stop(as.character(e), call.=FALSE)
        }
      )      
      a[[i]] = strat.res[si[[i]]$actions]  
      strat.states[[i]] = strat.res[si[[i]]$strat.states]
      game$check.action(ai=a[[i]],i=i,t=t)
    }
    
    # 2. Evaluate game results
    set.random.state("game")  
    results = game$run.stage.game(t=t,a=a,t.obs=obs,game.states=game.states,game=game)
    
    if (detailed.return) {
      strat.state.str.li[[t]] = sapply(strat.states,list.to.str, use.names=TRUE) 
    }
    round.stats.li[[t]] = results$round.stats

    old.obs = obs
    obs = results$obs
    game.states = results$game.states
    
    # 3. Update total payoffs
    U = U + results$payoff
    
  }
  
  set.random.state(".GLOBAL")
  u = U/T

  # Round stats
  round.stats = rbindlist(round.stats.li)
  game$adapt.round.stats.dt(round.stats, match.id=match.id)
  rs.dt = data.table(match.id=match.id, strat=rep(names(strat), times=T), round.stats)
  
  if (detailed.return) {
    strat.state.str = unlist(strat.state.str.li)
    rs.dt$strat.state = strat.state.str
  }
  
  
  # action statistics
  #as.df = do.call("rbind",lapply(action.stats, unlist)) / T
  # returned data frame
  res.dt = data.table(match.id = match.id,i=1:game$n, T=T,u.weight=T*T.weight, strat=names(strat), u=u)

  if (detailed.return) {
    return(list(rs=rs.dt,res=res.dt))
  }
  return(list(rs=rs.dt,res=res.dt))
}  

# Helper function to add indices to vector obs
obs.names.to.cols = function(obs.names, obs.len) {
  return(obs.names)
  obs.cols = unlist(lapply(seq_along(obs.names), function(j) {
    len = obs.len[j]
    if (len > 1) {
      str.combine(obs.names[j],1:len)
    } else {
      obs.names[j]
    }
  }))
  obs.cols
}

list.to.str = function(li, collapse=",", li.collapse=";", use.names=0) {
  if (!is.list(li))
    return(paste0(li, collapse=collapse))
  str = lapply(li,list.to.str, collapse=collapse, use.names=use.names-1)
  if (use.names>0 & !is.null(names(li)) & length(li)>0) {
    str = paste0(names(li),"=",str, collapse=li.collapse)
  } else {
    str = paste0(str, collapse=li.collapse)    
  }
  str
}

examples.list.to.str = function() {
  list.to.str(list(a=1:2,b=list(c=3,d="Hi")), use.name=1)
}
