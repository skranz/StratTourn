
# The code inside can be used to explore the behavior of strategies for the PD game
examples.pd = function() {
  #Load package
  library(StratTourn)
  
  # Generate a game object
  game = make.pd.game(err.D.prob=0.1, delta=0.9)

  # Pick a pair of strategies
  strat = nlist(tit.for.tat,random.action)
  # Let the strategies play against each other
  run.rep.game(game=game, strat = strat)
  
  
  getwd()
  # Set working directory in which data is stored
  setwd("D:/libraries/StratTourn/studies")

  # Init and run a tournament of several strategies against each other  
  strat = nlist(tit.for.tat,always.defect, always.coop, random.action)  
  tourn = init.tournament(game=game, strat=strat)
  
  #set.storing(FALSE)  # uncoment to make code run faster
  tourn = run.tournament(tourn=tourn, R = 4)
  set.storing(TRUE)
  
  tourn
  save.tournament(tourn)
  # Analyse tournament in web browser
  show.tournament(tourn)
}



# A strategy that always cooperates
always.coop = function(obs,i,t,...) {
  return(list(a="C"))
}

# A strategy that always defects
always.defect = function(obs,i,t,...) {
  return(list(a="D"))
}

# A strategy that randomly chooses an action
random.action = function(obs,i,t,...) {
  a = sample( c("C","D"),  1)
  return(list(a=a))
}


# The famous tit.for.tat strategy: winner of Axelrod's original tournament
tit.for.tat = function(obs,i,t,...) {
  debug.store("tit.for.tat",i,t) # Store each call for each player
  debug.restore("tit.for.tat",i=1,t=2) # Restore call for player i in period t
  
  # Cooperate in the first period
  if (t==1)
    return(list(a="C"))
  
  # In later periods Return the other players previous (observed) action
  j = 3-i
  list(a=obs$a[j])
}

# Strategy from the tutorial without much meaning
strange.defector <- function(obs, i, t, still.defect=0,...){
  debug.store("strange.defector",i,t) # Store each call for each player
  debug.restore("strange.defector",i=1,t=2) # Restore call for player i in period t
  
  # Randomize between C and D
  if (still.defect==0) {
    do.cooperate = (runif(1)<0.7) 
    # With 60% probability choose C
    if (do.cooperate){
      return(list(a="C", still.defect=0))
    } else {
      return(list(a="D", still.defect=4))
    }
  }
  
  # still.defect is bigger 0: play D and reduce still.defect by 1
  still.defect = still.defect -1
  return(list(a="D",still.defect=still.defect))
}

#' Generate a (noisy) Prisoners' Dilemma game
make.pd.game = function(uCC=1,uCD=-1,uDC=2,uDD=0,err.D.prob = 0, err.C.prob=0, private.signals=FALSE,delta=0.9,...) {
  
  run.stage.game = function(a,t,t.obs,...) {
    restore.point("pd.stage.game.fun")
    a = unlist(a, recursive=TRUE, use.name=FALSE)
    names(a) = paste0("a",1:2)
    
    if (err.D.prob + err.C.prob >1)
      stop("We need err.D.prob + err.C.prob <=1")
    
    err.prob = err.D.prob + err.C.prob
    
    # Payoffs
    mat = rbind(c(uCC,uCD),
                c(uDC,uDD))
    colnames(mat) = rownames(mat)=c("C","D")
    payoff = c(mat[a[1],a[2]],mat[a[2],a[1]])

    rand = runif(1)      
    err.D.1 = rand<err.prob & rand<err.D.prob
    err.C.1 = rand<err.prob & rand>=err.D.prob
    
    rand = runif(1)      
    err.D.2 = rand<err.prob & rand<err.D.prob
    err.C.2 = rand<err.prob & rand>=err.D.prob
    
    
    # Observation with noise
    if (private.signals) {
      obs1 = obs2 = a
      if (err.D.1) obs2[1] = "D"
      if (err.C.1) obs2[1] = "C"
      if (err.D.2) obs1[2] = "D"
      if (err.C.2) obs1[2] = "C"
      obs = list(list(a=obs1),list(a=obs2))
      obs.i = c(t.obs[[1]]$a[1],t.obs[[2]]$a[2])
      obs.j = c(t.obs[[1]]$a[2],t.obs[[2]]$a[1])
      
    } else {
      obs = a
      if (err.D.1) obs[1] = "D"
      if (err.C.1) obs[1] = "C"
      if (err.D.2) obs[2] = "D"
      if (err.C.2) obs[2] = "C"
      obs.i = t.obs$a
      obs.j = rev(t.obs$a)
      obs = list(a=obs)
    }
    round.stats = quick.df(t=c(t,t),i=1:2,u=payoff,a=a,
                             obs.i=obs.i,obs.j=obs.j,
                             err.D.i=c(err.D.1,err.D.2),err.D.j=c(err.D.2,err.D.1)) 
    
    return(list(payoff=payoff,obs=obs, round.stats=round.stats))
  } 
  adapt.round.stats.dt = function(rs.dt, ...) {
    rs.dt
  }
  
  check.action = function(ai,i,t,...) {
    ai = ai$a
    if (is.character(ai) & length(ai)==1) {
      if (ai %in% c("C","D")) {
        return()
      }
    }
    #restore.point("check.action.pd")
    stop(paste0("player ",i, "'s strategy in period ",t, " returned an infeasible action: ", ai))
  }
  example.action = function(i=1,t=1,...) {
    list(a="C")
  }
  example.obs = function(i=1,t=1,...) {
    list(a=c("C","C"))
  }
  
  nlist(run.stage.game, adapt.round.stats.dt,check.action,example.action,example.obs, n=2, private.signals, params = nlist(uCC,uCD,uDC,uDD,err.D.prob, err.C.prob), sym=TRUE, delta=delta, name="Noisy PD")
}


