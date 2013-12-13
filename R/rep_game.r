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

#' Runs a repeated game
#' @param delta discount factor
#' @param game the game object
#' @param strat a list of strategies
#' @param T.min the minium number of periods that will be played. Payoffs for periods t=1,...,T.min will be disocunted with delta^(t-1). After any period t>=T the game stops with probability 1-delta and all payoffs are discounted with delta^(T-1).
#' @param T.max optionally a maximum number of rounds
#' @param cap.numbers defines to which extend all numerics of the result are returned. As a default case it is off, meaning that the numerics are not to be transformed. Note that this does not influence the behavior within the game. 
run.rep.game = function(delta=game$param$delta, game, strat, T.min=1,T.max = round(runif(1,10000,12000)),detailed.return = TRUE, strat.seed=NULL, game.seed = NULL, do.store = TRUE,strat.par=NULL, cap.numbers = FALSE) {
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
    #if (t==2)
    #  stop()
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
      act.strat.par = strat.par[[i]][setdiff(names(strat.par[[i]]),names(strat.states[[i]]))]
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
      strat.res = format.strat.res(t=t,i=i,game=game,strat.res=strat.res,sts.names=sts.names,formals=formals(strat[[i]]))
      
      
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
  
  #Optional transformation to shorten result based on cap.numbers
  denv$df <- run.rep.game.assist.cap.numbers(detailed.return=detailed.return, output=denv$df, cap.numbers=cap.numbers)
    
  if (detailed.return) {
    return(list(hist=denv$df,u=u))
  }
  return(u)
}  


format.strat.res = function(t,i,game,strat.res,sts.names,formals) {
  restore.point("format.strat.res")
  
  #Note: This function relies fully on the example action. Here we can ensure a certain order of the arguments.
  # It is not obvious though, that the player accepts this order, so we have to match via names
  # This does not allow for unnamed arguments
  # Arguments which are not provided, but should be provided are seen as their default value
  ai.names = names(game$example.action(i=i,t=t))
  pl.names <- sts.names[[i]]
  
  missing.names <- setdiff(c(ai.names,pl.names),names(strat.res))
  
  # Don't add missing states. The user must return all strat.states 
  # other arguments are strat.par and shall not be modified
  
  #missing.elements <- as.list(rep(NA,length(missing.names)))
  #names(missing.elements) <- missing.names
  #missing.elements[intersect(missing.names,names(formals))] <- formals[intersect(missing.names,names(formals))]
  #strat.res <- append(strat.res,missing.elements)
  
  strat.res <- strat.res[setdiff(c(ai.names,pl.names), missing.names)]
  if (length(ai.names)==1) {
    return(list(a=strat.res[[ai.names]], strat.states=strat.res[-match(ai.names,names(strat.res))]))
  } else {
    return(list(a = strat.res[match(ai.names,names(strat.res))], strat.states = strat.res[-match(ai.names,names(strat.res))]))
  }  
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
    } else {
      public.obs.names = str.combine("obs_",game$public.obs)
      private.obs.names = unlist(lapply(1:n, function(i) {
        str.combine("obs",i,"_",game$private.obs[[i]])
      }))
    }
    
    # Get a list of all strategy state names
    strat.states.names = lapply(1:n, function(i) {
      si = next.strat.states[[i]]
      str.combine(names(si),"_",i)
    })
    strat.states.cols = unlist(strat.states.names)
    
    game.states.names = names(game.states)
    game.states.cols = unlist(game.states.names)
    next.game.states.cols = str.combine("next_",game.states.cols)
    if (!game$private.signals) {
      denv$obs.names = obs.names
      col.names = c("t",unlist(c(game.states.cols,obs.names,a.names,payoff.names,strat.states.cols)))  
    } else {
      denv$public.obs.names = public.obs.names
      denv$private.obs.names = private.obs.names
      col.names = c("t",unlist(c(game.states.cols,public.obs.names,private.obs.names,a.names,payoff.names,strat.states.cols)))  
    }
    
    df = as.data.frame(matrix(NA,T,length(col.names)))
    names(df)=col.names
    denv$df = df
    denv$a.names = a.names
    denv$payoff.names = payoff.names
    denv$strat.states.names = strat.states.names
    denv$strat.states.cols = strat.states.cols
    
    denv$game.states.cols = game.states.cols
   }
  denv$df[t,"t"] = t
  denv$df[t,denv$a.names] = unlist(lapply(a, function(ai) lapply(ai,list.to.str)))
  denv$df[t,denv$payoff.names] = payoffs
  
  if (game$private.signals) {
    denv$df[t,denv$public.obs.names] = 
      unlist(lapply(game$public.obs, function(j) {
        if (!(j %in% names(obs[[1]])))
          return(NA)  
        list.to.str(obs[[1]][[j]])
      }))
    
    denv$df[t,denv$private.obs.names] = unlist(lapply(1:n, function(i) {
      sapply(game$private.obs[[i]], function(j) {
        if (!(j %in% names(obs[[1]])))
          return(NA)
        list.to.str(obs[[i]][[j]])
      })
    }))  
  } else {
    denv$df[t,denv$obs.names] = 
      unlist(lapply(seq_along(denv$obs.names), function(j) {
        if (j > length(obs))
          return(NA)  
        list.to.str(obs[[j]])
      }))
  }
  
  denv$df[t,denv$strat.states.cols] = unlist(lapply(1:n, function(i) {
    sapply(seq_along(denv$strat.states.names[[i]]), function(j) {
      if (j > length(next.strat.states[[i]]))
        return(NA)
      list.to.str(next.strat.states[[i]][[j]])
    })
  }))  
  
  denv$df[t,denv$game.states.cols] = 
    sapply(seq_along(denv$game.states.cols), function(j) {
      if (j > length(game.states[[j]]))
        return(NA)
      list.to.str(game.states[[j]])
    })
  
}       

list.to.str = function(li, collapse=",", li.collapse=";") {
  if (!is.list(li))
    return(paste0(li, collapse=collapse))
  return(paste0(lapply(li,list.to.str, collapse=collapse),collapse=li.collapse))
}

examples.list.to.str = function() {
  list.to.str(list(a=1:2,b=list(c=3,d="Hi")))
}