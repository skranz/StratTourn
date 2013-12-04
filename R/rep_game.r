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
  
  #< Basic parameters not to be changed live
  default.cap.numbers = 2 #Number of digits to be rounded to in case of cap.number==TRUE
  #> Stop Basic parameters
  
  #< Minifunctions
  
  # Used in context with cap.numbers for better output
  transform.to.numeric <- function(df){
    as.data.frame(apply(df,MARGIN=2,FUN=function(x){
      pos.nas <- suppressWarnings(as.numeric(x))
      if(sum(is.na(pos.nas))==sum(is.na(x))){
        x <- round(as.numeric(x),cap.numbers)
      } else {
        #ignore 
      }
      x
    }))
  }
  #>
  
  
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
  
  #Optional transformation to shorten result
  
  #Basic checks of cap.number
  if(length(cap.numbers)>1){
    message("cap.numbers has to be single number. Please read the documentation.")
    stop()    
  }
  if(is.logical(cap.numbers)){ #So either the default value or it should be set to a default number
    if(cap.numbers==TRUE){
      cap.numbers <- as.integer(default.cap.numbers)
    } else {
      cap.numbers <- -1
    }
  } else if (is.numeric(cap.numbers)) { # number of digits explicitely given
    if(cap.numbers<0){
      message("cap.numbers has to be of positive value. Please read the documentation.")
      stop()
    }
    cap.numbers <- as.integer(cap.numbers) # to ensure that doubles do not interfere
  } else { # unexpected class of cap.numbers
    message("cap.numbers has to be either numerical or logical. Please read the documentation.")
    stop()
  }
  ## Now we have ensured, that cap.numbers is an integer, which is "greater or equal 0" or -1 (in the case of no cap.numbers)
  if (cap.numbers>=0 && detailed.return){ # If not detailed.return, we do not need this transformation
    denv$df <- transform.to.numeric(denv$df)  #Transform.to.numeric is a minifunction of this function an as such cap.numbers is known  
  } else { # cap.numbers==FALSE
    # do nothing
  }
  if (cap.numbers>=0){
    u <- round(u,cap.numbers)
  }
  
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

# Helper function to add indices to vector obs
obs.names.to.cols = function(obs.names, obs.len) {
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
      obs.len = sapply(ex.obs,length)
      obs.cols = obs.names.to.cols(obs.names,obs.len)
      
    } else {
      public.obs.names = str.combine("obs_",game$public.obs)
      private.obs.names = lapply(1:n, function(i) {
        str.combine("obs",i,"_",game$private.obs[[i]])
      })
      public.obs.len = sapply(ex.obs[[1]][game$public.obs],length)
      names(public.obs.len) = game$public.obs
      private.obs.len = lapply(1:n, function(i) {
        ret = sapply(ex.obs[[i]][game$private.obs[[i]]], length)
        names(ret) = game$private.obs[[i]]
        ret
      })
      public.obs.cols = obs.names.to.cols(public.obs.names,public.obs.len)
      private.obs.cols = unlist(lapply(1:n, function(i) {
        obs.names.to.cols(private.obs.names[[i]],private.obs.len[[i]])
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
    game.states.cols = obs.names.to.cols(game.states.names, pmin(game.states.len,max.state.vector.size))
    next.game.states.cols = str.combine("next_",game.states.cols)
    if (!game$private.signals) {
      denv$obs.len = obs.len
      denv$obs.cols = obs.cols
      col.names = c("t",unlist(c(game.states.cols,obs.cols,a.names,payoff.names,strat.states.cols)))  
    } else {
      denv$public.obs.len = public.obs.len
      denv$public.obs.cols = public.obs.cols
      denv$private.obs.len = private.obs.len
      denv$private.obs.cols = private.obs.cols
      col.names = c("t",unlist(c(game.states.cols,public.obs.cols,private.obs.cols,a.names,payoff.names,strat.states.cols)))  
    }
    
    df = as.data.frame(matrix(NA,T,length(col.names)))
    names(df)=col.names
    denv$df = df
    denv$a.names = a.names
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
    denv$df[t,denv$public.obs.cols] = 
      unlist(lapply(game$public.obs, function(j) {
        if (!(j %in% names(obs[[1]])))
          return(rep(NA,denv$public.obs.len[j]))  
        to.length(obs[[1]][[j]],denv$public.obs.len[j])
      }))
    
    denv$df[t,denv$private.obs.cols] = unlist(lapply(1:n, function(i) {
      sapply(game$private.obs[[i]], function(j) {
        if (!(j %in% names(obs[[1]])))
          return(rep(NA,denv$private.obs.len[[i]][j]))
        to.length(obs[[i]][[j]],denv$private.obs.len[[i]][j])
      })
    }))  
  } else {
    denv$df[t,denv$obs.cols] = 
      unlist(lapply(seq_along(denv$obs.len), function(j) {
        if (j > length(obs))
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

