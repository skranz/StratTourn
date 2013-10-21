## Only necessary if not done already. 
## Delete the "#" to uncomment
#install.packages("devtools", "data.table", "ggplot2", "reshape2")

library(devtools)

#Should be installed in case there was an update
## Can be commented while actively working on strategies

#install_github(repo = "restorepoint", username = "skranz")
#install_github(repo = "sktools", username = "skranz")
#install_github(repo = "StratTourn", username = "skranz")


# The code inside can be used to explore the behavior of strategies for the PD game
examples.pd = function() {
  #Load package
  library(StratTourn)
  
  # Generate a game object (Baseline Szenario)
  game <- make.pd.game(err.D.prob=0.15,err.C.prob=0)
  delta <- 0.95
  
  # Pick a pair of strategies
  ## To pick a pair of strategies, comment (#) the other one
  strat <- nlist(ashamed.defector,random.action)
  strat <- nlist(always.coop,always.coop)
  
  # Let the strategies play against each other (Baseline Szenario but with T.min set for less random results)
  set.storing(TRUE) # Allow storing, which is necessary for debugging
  run.rep.game(delta=delta, game=game, strat = strat, T.min=15)
  
  
  # Init and run a tournament of several strategies against each other  
  set.storing(FALSE) # Debugging off, which saves time
  strat = nlist(tit.for.tat,always.defect,always.coop,ashamed.defector) # Strategies in the tournament
  tourn = init.tournament(game=game, strat=strat, delta=delta, score.fun = "efficiency-2*instability-20*instability^2")
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

#Does not admit to sending the wrong signal (even though he can not influence that) by defecting on purpose
#to make it seem as if the defection was on purpose all along
ashamed.defector <- function(obs, i, t, game, shame.counter){
  debug.store("ashamed.defector",i,t) # Store each call for each player
  debug.restore("ashamed.defector",i=1,t=2) # Restore call for player i in period t
  
  #First round be nice
  if(t==1){
    return(list(a="C",shame.counter=0))
  }
  
  #"Normal" case, where player is not ashamed and has no reason to be so
  if(shame.counter==0 && obs$a[i]=="C"){
    return(list(a="C",shame.counter=0))
  }
  
  #If ashamed, defect to cover up "mistake"
  if(shame.counter>1){
    shame.counter = shame.counter-1
    return(list(a="D",shame.counter=shame.counter))
  }
  
  #Ready to be nice again
  if(shame.counter==1){
    shame.counter = 0
    return(list(a="C",shame.counter=shame.counter))
  }  
  
  ##This point is reached iff the shame.counter is zero, but other player observed a defection
  return(list(a="D",shame.counter=5))
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

  

