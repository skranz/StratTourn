split.action.and.strat.states = function(t,i,game,strat.res, sts.names=NULL, stage=NULL) {
  restore.point("split.action.and.strat.states")
  ai.names = names(game$example.action(i=i,t=t, stage = stage))
  list(action=strat.res[ai.names], strat.states=strat.res[setdiff(names(strat.res), ai.names)])
}



#' Runs a repeated game
#' @param delta discount factor
#' @param game the game object
#' @param strat a list of strategies
#' @param T.min the minium number of periods that will be played. Payoffs for periods t=1,...,T.min will be disocunted with delta^(t-1). After any period t>=T the game stops with probability 1-delta and all payoffs are discounted with delta^(T-1).
#' @param T.max optionally a maximum number of rounds
run.rep.multistage.game = function(delta=game$param$delta, game, strat, T.min=1,T.max = round(runif(1,10000,12000)),detailed.return = TRUE, strat.seed=NULL, game.seed = NULL) {
  restore.point("run.rep.game")
  
  set.random.state(".GLOBAL")
  
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
  T.rand = rnbinom(1,size=1,prob=1-delta)
  T = pmin(T.min + T.rand,T.max)
  
  
  
  strat.id = seq_along(strat)
  n.strat = length(strat.id)
  
  if (n.strat != game$n) {
    stop(paste("The game requires ",game$n, " players but you submitted ", n.strat, " strategies."))
  }
  
  if (game$private.signals) {
    obs = vector("list",game$n)
  } else {
    obs = NULL
  }
  
  denv = new.env()
  a = vector("list",n.strat)
  game.states = game$init.states
  # Names of strategy states
  sts.names = lapply(strat.id, function(i) {
    setdiff(names(formals(strat[[i]])),c("obs", "i","t","game","..."))
  })
  strat.states = lapply(strat.id, function(i) {
    list()
  })
  
  
  u = rep(0,game$n)
  delta.compound = 1
  t = 1
  for (t in 1:T) {
    
    if (detailed.return) {
      start.strat.states = strat.states
      start.game.states = game.states
    }
    
    # Set the random state for the strategy
    set.random.state("strat")
    
    
    period.payoffs = rep(0,game$n)
    stage.num = 1    
    for (stage.num in seq_along(game$stages)) {
      stage = game$stages[stage.num]  
      # 1. Evaluate strategies of each player
      for (i in strat.id) {
        obs.i = get.obs.i(obs = obs, i = i, game = game)
        args = c(list(obs=obs.i,i=i,t=t, game=game, stage = stage),game.states,strat.states[[i]])
        
        tryCatch(
          strat.res <- do.call(strat[[i]],args),
          error = function(e) {
            message("Error in evaluating strategy ", names(strat)[i], " in period t=", t," stage=",stage, " for player i=",i,"\nERROR.HIST:")
            hist = denv$df[1:t,]
            
            print(tail(hist))
            assign(".BOS.ERROR.HIST",hist,.GlobalEnv)
            assign(".BOS.ERROR.t",t,.GlobalEnv)
            assign(".BOS.ERROR.i",i,.GlobalEnv)
            
            stop(as.character(e), call.=FALSE)
          }
        )
        as = split.action.and.strat.states(t=t,i=i,game=game,strat.res=strat.res, stage=stage)
        a[[i]] = as$action
        strat.states[[i]] = as$strat.states
        game$check.action(ai=a[[i]],i=i,t=t, stage=stage)
      
      } # end player
      
      # 2. Evaluate game results
      set.random.state("game")      
      results = game$results.fun(a=a,game.states=game.states,game=game, stage=stage)
      old.obs = obs
      obs = results$obs
      game.states = results$game.states
      
      if (!is.null(results$payoff))
        period.payoffs = period.payoffs + results$payoff

      # 4. Update statistics if desired
      if (detailed.return) {
        rep.multistage.game.store.detailed.return(stage=stage,t,old.obs,obs,a, payoffs=results$payoff, start.strat.states,next.strat.states=strat.states, game.states = start.game.states,next.game.states=game.states, denv,T, game, strat=strat)
      }
      
      
    } # end stage
    
    # 3. Update total payoffs
    if (delta==1) {
      u = u + period.payoffs
    } else {      
      u = u + (delta.compound) + period.payoffs 
    }
    
    if (t<T.min) {
      delta.compound = delta.compound*delta
    }
    
  }
  
  u = sum.to.average.payoffs(u.sum=u,T=T,T.min=T.min,delta=delta, game=game)
  if (detailed.return) {
    return(list(hist=denv$li,u=u))
  }
  return(u)
}  


# Transforms a discounted sum of payoffs to average payoffs per period
sum.to.average.payoffs = function(u.sum,T, T.min, delta, game) {
  # Compute expexted number of rounds
  if (T.min>1) {
    tau = T.min-1
    t = 1:tau
    sum(delta^(t-1)*(1-delta) ) + delta^tau
    ET = sum(delta^(t-1)*(1-delta)*t) + delta^tau*T
    u.sum/ET
  } else {
    ET = T
  }
  u = u.sum/ET
}


rep.multistage.game.store.detailed.return = function(stage,t,obs,next.obs,a, payoffs, strat.states,next.strat.states, game.states,next.game.states,denv,T, game, max.state.vector.size = 4, strat) {
  restore.point("rep.game.store.detailed.return")
  n = game$n
  if (is.null(denv$li))
    denv$li = list()
  
  game.states = game.states[setdiff(names(game.states),"all.obs")]
  denv$li[[length(denv$li)+1]] = unlist(list(t=t,stage=stage,game.states = game.states,obs = obs, a=a, payoffs = payoffs, strat.states = next.strat.states))
  
}       
