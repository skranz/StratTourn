

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
  run.rep.game(delta=0.9, game=game, strat = strats, detailed.return=FALSE)
    
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
  ex.obs = game$example.obs(i=i)
  ex.action = game$example.action(i=i)
  
  # Extras arguments of the strategy
  args = setdiff(names(formals(strat)),c("obs", "i","t","game","..."))
  
  # Returned values of the strategy
  res  = strat(obs=ex.obs,i=i,t=1, game=game) 
  
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
run.rep.game = function(delta=game$param$delta, game, strat, T.max = NULL,detailed.return = TRUE, strat.seed=NULL, game.seed = NULL, do.store = TRUE,strat.par=NULL, sample.delta = 1 - (1-delta)/2, match.id = sample.int(2147483647,1)) {
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
  #action.stats = lapply(1:game$n, get.empty.action.stats, game=game)
  
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
      a[[i]] = strat.res[si[[i]]$actions]  
      strat.states[[i]] = strat.res[si[[i]]$strat.states]
      game$check.action(ai=a[[i]],i=i,t=t)
      #action.stats[[i]] = add.to.action.stats(a[[i]], action.stats[[i]])
    }    
    # 2. Evaluate game results
    set.random.state("game")  
    results = game$run.stage.game(t=t,a=a,t.obs=obs,game.states=game.states,game=game)
    round.stats.li[[t]] = results$round.stats
    old.obs = obs
    obs = results$obs
    game.states = results$game.states
    
    # 3. Update total payoffs
    U = U + results$payoff
    
    # 4. Update statistics if desired
    if (detailed.return) {
      rep.game.store.detailed.return(t,old.obs,obs,a, payoffs=results$payoff, start.strat.states,next.strat.states=strat.states, game.states = start.game.states,next.game.states=game.states, denv,T, game, strat=strat)
    }
  }
  
  set.random.state(".GLOBAL")
  u = U/T

  # Round stats
  round.stats = rbindlist(round.stats.li)
  game$adapt.round.stats.dt(round.stats, match.id=match.id)
  rs.dt = data.table(match.id=match.id, strat=rep(names(strat), times=T), round.stats)
  
  # action statistics
  #as.df = do.call("rbind",lapply(action.stats, unlist)) / T
  # returned data frame
  res.dt = data.table(match.id = match.id,i=1:game$n, T=T,u.weight=T*T.weight, strat=names(strat), u=u)

  if (detailed.return) {
    return(list(hist=denv$df,res=res.dt, rs=rs.dt))
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


get.empty.action.stats = function(i, game) {
  sets = game$get.action.set(i)
  na = names(sets)
  as = lapply(sets, function(set) {
    if (is.null(set))
      return(0)
    li = vector("numeric",length(set))
    names(li) = set
    li
  })
  as  
}

add.to.action.stats = function(a,as) {
  li = lapply(seq_along(as), function(j) {
    if (is.numeric(a[[j]]))
      return(as[[j]]+a[[j]])
    ret = as[[j]]
    ret[a[[j]]] = ret[a[[j]]]+1
    ret
  })
  names(li) = names(as)
  return(li)
}
