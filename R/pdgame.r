
# The code inside can be used to explore the behavior of strategies for the PD game
examples.pd = function() {
  #Load package
  library(StratTourn)
  
  # Generate a game object
  game = make.pd.game(err.D.prob=0.15)
  # Pick a pair of strategies
  strat = nlist(tit.for.tat,random.action)
  strat = nlist(always.coop,always.coop)
  
  # Let the strategies play against each other
  run.rep.game(delta=0.9, game=game, strat = strat)
  run.rep.game(delta=0.9, game=game, strat = strat, T.min = 10)
  
  
  # Init and run a tournament of several strategies against each other  
  set.storing(FALSE)
  #strat = nlist(grim.trigger,tit.for.tat,always.defect, always.coop,net.nice0)
  strat = nlist(grim.trigger,tit.for.tat,always.defect,net.nice0,net.nice2)
  
  tourn = init.tournament(game=game, strat=strat, delta=0.95, score.fun = "efficiency-2*instability-20*instability^2")
  tourn = run.tournament(tourn=tourn, R = 4)
  tourn
  
  x = seq(0,0.5,by=0.05)
  score = 2*x+20*x^2
  plot(x,x+10*x^2)
  round(cbind(x,score),3)
  
  strat = nlist(tit.for.tat)
  strat.dev = list(grim.trigger= nlist(always.defect, always.coop),
                   tit.for.tat = nlist(always.defect, always.coop))
  tourn = init.tournament(game=game, strat=strat,strat.dev=strat.dev, delta=0.9)
  tourn = run.tournament(tourn=tourn, R = 10)
  tourn
  
}


# A strategy that always cooperates
always.coop = function(obs,i,t,game) {
  return(list(a="C"))
}

# A strategy that always defects
always.defect = function(obs,i,t,game) {
  return(list(a="D"))
}

# A strategy that randomly chooses an action
random.action = function(obs,i,t,game) {
  a = sample( c("C","D"),  1)
  return(list(a=a))
}


# The famous tit.for.tat strategy: winner of Axelrod's original tournament
tit.for.tat = function(obs,i,t,game) {
  debug.store("tit.for.tat",i,t) # Store each call for each player
  debug.restore("tit.for.tat",i=1,t=2) # Restore call for player i in period t
  
  # Cooperate in the first period
  if (t==1)
    return(list(a="C"))
  
  # In later periods Return the other players previous (observed) action
  j = 3-i
  list(a=obs$a[j])
}

grim.trigger = function(obs,i,t,game,coop) {
  debug.store("grim.trigger",i,t) # Store each call for each player
  debug.restore("grim.trigger",i=1,t=2) # Restore call for player i in period t
  
  if (t==1)
    return(list(a="C",coop=TRUE))
  
  # Cooperate if and only if everybody so far has cooperated
  if (coop & obs$a[1]=="C" & obs$a[2]=="C") {
    return(list(a="C",coop=TRUE))
  } else {
    return(list(a="D",coop=FALSE))
  }
}



# A function to generate a PD game
make.pd.game = function(uCC=1,uCD=-1,uDC=2,uDD=0,err.D.prob = 0, err.C.prob=0, private.signals=FALSE) {
  results.fun = function(a,...) {
    restore.point("pd.results.fun")
    a = unlist(a)
    names(a) = paste0("a",1:2)
    
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
      obs = list(list(a=obs1),list(a=obs2))
      
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
    #print(paste("Obs:"))
    #print(obs)
    return(list(payoff=payoff,obs=obs))
  }  
  check.action = function(ai,i,t,...) {
    if (is.character(ai) & length(ai)==1) {
      if (ai %in% c("C","D")) {
        return()
      }
    }
    #restore.point("check.action.pd")
    stop(paste0("player ",i, "'s strategy in period ",t, " returned an infeasible action: ", ai))
  }
  example.action = function(i,t,...) {
    list(a="C")
  }
  example.obs = function(i,t,...) {
    list(a=c("D","C"))
  }
  
  nlist(results.fun, check.action,example.action,example.obs, n=2, private.signals, a.names = c("a1","a2"), params = nlist(uCC,uCD,uDC,uDD,err.D.prob                                                                                                                                    ), sym=TRUE, name="Noisy PD")
}

  

