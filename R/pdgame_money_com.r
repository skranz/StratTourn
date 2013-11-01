
# The code inside can be used to explore the behavior of strategies for the PD game
examples.pd = function() {
  #Load package
  library(StratTourn)
  
  # Generate a game object
  game = make.message.transfer.pd.game(err.D.prob=0.6)
  # Pick a pair of strategies
  strat = nlist(always.be.even,random.action)

  strat = nlist(always.be.even,always.be.even)
  
  # Let the strategies play against each other
  run.rep.game(delta=0.95, game=game, strat = strat, T.min = 1)
  
  
  # Init and run a tournament of several strategies against each other  
  set.storing(FALSE)
  strat = nlist(tit.for.tat,always.defect, always.coop, random.action)  
  tourn = init.tournament(game=game, strat=strat, delta=0.95, score.fun = "efficiency-2*instability-20*instability^2")
  tourn = run.tournament(tourn=tourn, R = 4)
  tourn
  
  # Second stage of tournament  
  strat = nlist(tit.for.tat)
  strat.dev = list(grim.trigger= nlist(always.defect, always.coop),
                   tit.for.tat = nlist(always.defect, always.coop))
  tourn = init.tournament(game=game, strat=strat,strat.dev=strat.dev, delta=0.9)
  tourn = run.tournament(tourn=tourn, R = 10)
  tourn
  
}


# A strategy that always cooperates
always.coop = function(obs,i,t,game, stage,...) {
  if (stage == "action")
    return(list(a="C"))
  
  if (stage == "message")
    return(list(message="No message"))
  
  if (stage == "transfer")
    return(list(transfer=0))
}

# A strategy that always defects
always.defect = function(obs,i,t,game, stage,...) {
  if (stage == "action")
    return(list(a="D"))
  
  if (stage == "message")
    return(list(message=""))
  
  if (stage == "transfer")
    return(list(transfer=0))
}

# A strategy that randomly chooses an action
random.action = function(obs,i,t,game,stage,...) {
  a = sample( c("C","D"),  1)
  if (stage == "action")
    return(list(a=a))
  
  if (stage == "message")
    return(list(message=""))
  
  if (stage == "transfer")
    return(list(transfer=0))
}


# Always try to equalize payoff difference 
always.be.even = function(obs,i,t,game, stage, net.payoff=0, ...) {
  debug.store("always.be.even",i = i, t = t, stage=stage)
  debug.restore("always.be.even",i = 1, t = 2, stage="action")
  
  restore.point("always.be.even")
  
  
  
  j = 3-i
  
  if (stage == "action") {
    if (t>1) {
      net.payoff= net.payoff + obs$transfers[j] - obs$transfers[i]
    }
    if (net.payoff >= 0) {
      a = "C"
    } else {
      a = "D"
    }
    return(list(a=a, net.payoff=net.payoff))
  }  
  
  if (stage == "message") {
    # Compute stage game payoffs corresponding to observed a
    pi=game$results.fun(a=list(list(a=obs$a[1]),list(a=obs$a[2])), stage="action")$payoff
    net.payoff= net.payoff + pi[i]-pi[j] 
  
    return(list(message="", net.payoff = net.payoff))
  }
  # If net.payoff > 0 make transfers to settle payoff differences
  if (stage == "transfer") {
    return(list(transfer=max(net.payoff,0),net.payoff = net.payoff))
  }
}


#' Generate a (noisy) Prisoners' Dilemma game in which players can conduct messages and transfers
make.message.transfer.pd.game = function(uCC=1,uCD=-1,uDC=2,uDD=0,err.D.prob = 0, err.C.prob=0, private.signals=FALSE) {
  results.fun = function(a,stage,game.states=NULL,...) {
    restore.point("transfer.pd.results.fun")
    
    if (stage == "action") { 
      restore.point("transfer.pd.results.fun_action")
      
      a = c(a1=a[[1]]$a,a2=a[[2]]$a)
      if (err.D.prob + err.C.prob >1)
        stop("We need err.D.prob + err.C.prob <=1")
      
      err.prob = err.D.prob + err.C.prob
      
      # Payoffs
      mat = rbind(c(uCC,uCD),
                  c(uDC,uDD))
      colnames(mat) = rownames(mat)=c("C","D")
      payoff = c(mat[a[1],a[2]],mat[a[2],a[1]])
      
      
      # Observation with noise
      if (private.signals) {
        obs1 = obs2 = a
        rand = runif(1)      
        if (rand<err.prob)
          obs1[2] = ifelse(rand<err.D.prob,"D","C")      
        rand = runif(1)      
        if (rand<err.prob)
          obs2[1] = ifelse(rand<=err.D.prob,"D","C")
        obs = list(a=list(obs1,obs2))
        
      } else {
        obs = a
        rand = runif(1)      
        if (rand<err.prob)
          obs[1] = ifelse(rand<=err.D.prob,"D","C")      
        rand = runif(1)      
        if (rand<err.prob)
          obs[2] = ifelse(rand<=err.D.prob,"D","C")      
        obs = list(a=obs)
      }
      obs = c(list(public.rand.num = runif(1)), obs)
      
      all.obs = c(game.states$all.obs,obs)
      return(list(payoff=payoff,obs=all.obs, game.states = list(all.obs = obs), next.stage="message", next.period = FALSE))
    }
    
    if (stage == "message") {
      obs = c(game.states$all.obs,
              list(messages = c(a[[1]]$message,a[[2]]$message)))
      return(list(payoff=c(0,0),obs=obs, game.states = list(all.obs = obs), next.stage="transfer", next.period = FALSE))
    }
    
    if (stage == "transfer") {
      obs = c(game.states$all.obs,
              list(transfers = c(a[[1]]$transfer,a[[2]]$transfer)))
      payoff = c(a[[2]]$transfer-a[[1]]$transfer, a[[1]]$transfer-a[[2]]$transfer)
      return(list(payoff=payoff,obs=obs, game.states = list(all.obs = obs), next.stage="action", next.period = TRUE))
    }
  }  
  check.action = function(ai,i,t,stage,...) {
    return()
  }
  example.action = function(i,t,stage,...) {
    if (stage == "action") {
      return(list(a="C"))
    }
    if (stage == "message") {
      return(list(message='myobs = c("C","D")'))
    }
    if (stage == "transfer") {
      return(list(transfer=2))
    }
  }
  example.obs = function(i,t,stage,...) {
    if (stage == "action") {
      return(list(a=c("D","C"), messages =c("Hi","you"), transfers=c(20,0)))
    }
    if (stage == "message") {
      return(list(message='myobs = c("C","D")'))
    }
    if (stage == "transfer") {
      return(list(transfer=2))
    }
    
    
    
  }
  
  if (private.signals) {
    private.obs = "a"
  } else {
    private.obs = NULL
  }
  
  nlist(stages = c("action","message","transfer"),results.fun, check.action,example.action,example.obs, n=2, private.signals, private.obs, params = nlist(uCC,uCD,uDC,uDD,err.D.prob, err.C.prob), sym=TRUE, name="Noisy PD with messages and transfers", score.fun = "efficiency-2*instability- 20*instability^2")
}

  

