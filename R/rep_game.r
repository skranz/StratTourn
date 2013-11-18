get.obs.i = function(i,obs, game) {
  if (is.null(obs))
    return(NULL)
  if (game$private.signals) {
    if (is.null(game$private.obs)) {
      obs.i = obs[[i]]  
    } else {
      obs.i = obs
      for (po in game$private.obs) {
        obs.i[[po]] = obs[[po]][[1]]
      }
    }
  } else {
    obs.i = obs
  }
  return(obs.i)  
  
}

#' Runs a repeated game
#' @param delta discount factor
#' @param game the game object
#' @param strat a list of strategies
#' @param T.min the minium number of periods that will be played. Payoffs for periods t=1,...,T.min will be disocunted with delta^(t-1). After any period t>=T the game stops with probability 1-delta and all payoffs are discounted with delta^(T-1).
#' @param T.max optionally a maximum number of rounds
run.rep.game = function(delta=game$param$delta, game, strat, T.min=1,T.max = round(runif(1,10000,12000)),detailed.return = TRUE, strat.seed=NULL, game.seed = NULL, do.store = TRUE,strat.par=NULL) {
  restore.point("run.rep.game")
  
  gbos$do.store = do.store
  
  # We have a separate function for multistage games
  if (length(game$stages)>0) {
    return(run.rep.multistage.game(delta=delta, game=game, strat=strat, T.min=T.min,T.max = T.max,detailed.return = detailed.return, strat.seed=strat.seed, game.seed = game.seed))
  }
  
  
  
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
    
    # 1. Evaluate strategies of each player
    for (i in strat.id) {
      obs.i = get.obs.i(obs = obs, i = i, game = game)
    
      # Use only those strat.par that are not returned as a strat.state
      act.strat.par = strat.par[[i]][setdiff(names(strat.par),names(strat.states[[i]]))]
      args = c(list(obs = obs.i,i=i,t=t, game=game),game.states,strat.states[[i]], act.strat.par)
      
      
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
      strat.res = format.strat.res(t=t,i=i,game=game,strat.res=strat.res)
      
      a[[i]] = strat.res$a
      strat.states[[i]] = strat.res$strat.states
      game$check.action(ai=a[[i]],i=i,t=t)
    }
    names(a) = game$a.names
    
    # 2. Evaluate game results
    set.random.state("game")  
    results = game$results.fun(a=a,game.states=game.states,game=game)
    old.obs = obs
    obs = results$obs
    game.states = results$game.states
    
    # 3. Update total payoffs
    if (delta==1) {
      u = u + results$payoff
    } else {      
      u = u + (delta.compound) * results$payoff 
    }
    
    if (t<T.min) {
      delta.compound = delta.compound*delta
    }
    
    # 4. Update statistics if desired
    if (detailed.return) {
      rep.game.store.detailed.return(t,old.obs,obs,a, payoffs=results$payoff, start.strat.states,next.strat.states=strat.states, game.states = start.game.states,next.game.states=game.states, denv,T, game, strat=strat)
    }
  }
  
  set.random.state(".GLOBAL")
  
  # Compute expexted number of rounds
  if (T.min>1) {
    tau = T.min-1
    t = 1:tau
    sum(delta^(t-1)*(1-delta) ) + delta^tau
    ET = sum(delta^(t-1)*(1-delta)*t) + delta^tau*T
    u/ET
  } else {
    ET = T
  }
  
  u = u/ET
  if (detailed.return) {
    return(list(hist=denv$df,u=u))
  }
  return(u)
}  


format.strat.res = function(t,i,game,strat.res, sts.names=NULL) {
  ai.names = names(game$example.action(i=i,t=t))
  if (length(ai.names)==1) {
    return(list(a=strat.res[[1]], strat.states=strat.res[-1]))
  } else {
    return(list(a = strat.res[1:length(ai.names)], strat.states = strat.res[-(1:length(ai.names))]))
  }  
}


rep.game.store.detailed.return = function(t,obs,next.obs,a, payoffs, strat.states,next.strat.states, game.states,next.game.states,denv,T, game, max.state.vector.size = 4, strat) {
  restore.point("rep.game.store.detailed.return")
  n = game$n
  if (t == 1) {
    restore.point("rep.game.store.detailed.return_t1")
    
    # Get a list of all names
    a.names = lapply(1:n, function(i) {
      ai = a[[i]]
      if (is.list(ai))
        return(paste0(names(ai),i))
      return(paste0("a",i))
    })
    a.names = do.call("c",a.names)
    payoff.names = paste0("pi",1:n)
    
    ex.obs = game$example.obs()
    
    # Get names of observations
    if (!game$private.signals) {
      obs.names = str.combine("obs_",names(ex.obs))
      obs.len = sapply(ex.obs,length)
      obs.cols = unlist(lapply(seq_along(obs.names), function(j) {
        len = obs.len[j]
        if (len > 1) {
          str.combine(obs.names[j],1:len)
        } else {
          obs.names[j]
        }
      }))
      
    } else {
      obs.names = lapply(1:n, function(i) {
        str.combine("obs",i,"_",names(ex.obs[[i]]))
      })
      obs.len = lapply(1:n, function(i) {
        sapply(ex.obs[[i]], length)
      })
      obs.cols = unlist(lapply(1:n, function(i) {
        sapply(seq_along(obs.names[[i]]), function(j) {
          len = obs.len[[i]][j]
          if (len > 1) {
            str.combine(obs.names[[i]][j],"_",1:len)
          } else {
            obs.names[[i]][j]
          }
        })
      }))
    }
    
    # Get a list of all strategy state names
    strat.states.names = lapply(1:n, function(i) {
      si = next.strat.states[[i]]
      str.combine(names(si),"_",i)
    })
    
    #strat.states.names = lapply(1:n, function(i) {
    #  si = setdiff(names(formals(strat[[i]])),c("obs", "i","t","game","..."))
    #  str.combine(si,"_",i)
    #})
    
    
    strat.states.len = lapply(1:n, function(i) {
      sapply(next.strat.states[[i]], length)
    })
    strat.states.cols = unlist(lapply(1:n, function(i) {
      sapply(seq_along(strat.states.names[[i]]), function(j) {
        len = min(strat.states.len[[i]][j],max.state.vector.size)
        if (len > 1) {
          str.combine(strat.states.names[[i]][j],"_",
                      1:len)
        } else {
          strat.states.names[[i]][j]
        }
      })
    }))
    next.strat.states.cols = str.combine("next_",strat.states.cols)
    
    game.states.names = names(game.states)
    game.states.len = sapply(game.states,length)
    game.states.cols = unlist(lapply(seq_along(game.states.names), function(j) {
      len = min(game.states.len[j],max.state.vector.size)
      if (len > 1) {
        str.combine(game.states.names[j],"_",
                    1:len)
      } else {
        game.states.names[j]
      }
    }))
    next.game.states.cols = str.combine("next_",game.states.cols)
    
    col.names = c("t",unlist(c(game.states.cols,obs.cols,a.names,payoff.names,strat.states.cols)))
    df = as.data.frame(matrix(NA,T,length(col.names)))
    names(df)=col.names
    denv$df = df
    denv$a.names = a.names
    denv$obs.len = obs.len
    denv$obs.cols = obs.cols
    denv$payoff.names = payoff.names
    denv$strat.states.len = strat.states.len
    denv$strat.states.cols = strat.states.cols
    denv$next.strat.states.cols = next.strat.states.cols
    
    denv$game.states.len = game.states.len
    denv$game.states.cols = game.states.cols
    denv$next.game.states.cols = next.game.states.cols
    
  }
  denv$df[t,"t"] = t
  denv$df[t,denv$a.names] = unlist(a)
  denv$df[t,denv$payoff.names] = payoffs
  
  if (game$private.signals) {
    denv$df[t,denv$obs.cols] = unlist(lapply(1:n, function(i) {
      sapply(seq_along(denv$obs.len[[i]]), function(j) {
        if (j > length(obs[[i]][[j]]))
          return(rep(NA,denv$obs.len[[i]][j]))
        to.length(obs[[i]][[j]],denv$obs.len[[i]][j])
      })
    }))  
  } else {
    denv$df[t,denv$obs.cols] = 
      unlist(lapply(seq_along(denv$obs.len), function(j) {
        if (j > length(obs[[j]]))
          return(rep(NA,denv$obs.len[j]))  
        to.length(obs[[j]],denv$obs.len[j])
      }))
  }
  
  denv$df[t,denv$strat.states.cols] = unlist(lapply(1:n, function(i) {
    sapply(seq_along(denv$strat.states.len[[i]]), function(j) {
      if (j > length(next.strat.states[[i]]))
        return(rep(NA,denv$strat.states.len[[i]][j]))
      
      to.length(next.strat.states[[i]][[j]],denv$strat.states.len[[i]][j])
    })
  }))  
  
  denv$df[t,denv$game.states.cols] = 
    sapply(seq_along(denv$game.states.len), function(j) {
      if (j > length(game.states[[j]]))
        return(rep(NA,denv$game.states.len[j]))
      
      to.length(game.states[[j]],denv$game.states.len[j])
    })
  
}       

