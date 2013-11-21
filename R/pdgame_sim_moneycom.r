
# The code inside can be used to explore the behavior of strategies for the PD game
examples.pd = function() {
  #Load package
  library(StratTourn)
  
  # Generate a game object
  game = make.payment.message.pd.game(err.D.prob=0.25, private.signals=TRUE)
  # Pick a pair of strategies
  strat = nlist(repay,repay)
  
  # Let the strategies play against each other
  run.rep.game(delta=0.9, game=game, strat = strat, T.min = 2)
  
  
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
pm.always.coop = function(obs,i,t,game, ...) {
  return(list(a="C",pay=0,m=""))
}

# A strategy that always defects
pm.always.defect = function(obs,i,t,game,...) {
  return(list(a="D",pay=0,m=""))
}

# A strategy that randomly chooses an action
pm.random.action = function(obs,i,t,game,...) {
  return(list(a="D",pay=0,m=""))
}


# Always try to equalize payoff difference 
repay = function(obs,i,t,game, net.payoff=0, interest = 0.05, ...) {
  debug.store("repay",i = i, t = t)
  debug.restore("repay",i = 1, t = 2)
  
  if (t==1) {
    return(nlist(a="C",pay=0, m="",net.payoff))
  }
  j=3-i
  if (obs$a[i]==obs$a[j]) {
    net.stage.payoff = 0
  } else if (obs$a[i]=="C") {
    net.stage.payoff = game$param$uCD-game$param$uDC    
  } else {
    net.stage.payoff = game$param$uDC-game$param$uCD        
  }
  
  net.payoff= net.payoff + obs$pay[j] - obs$pay[i]
  balanced = net.payoff>=0 # Did payments balance out previous inequalities?
  net.payoff = (net.payoff + net.stage.payoff) *(1+interest)
  
  
  if (!balanced) {
    return(nlist(a="D",pay=0,m="",net.payoff))
  } else {
    return(nlist(a="C",pay=max(0,net.payoff), m="", net.payoff))
  }
}


#' Generate a (noisy) Prisoners' Dilemma game in which players can conduct messages and payments
make.payment.message.pd.game = function(uCC=1,uCD=-1,uDC=2,uDD=0,err.D.prob = 0,err.C.prob=0, private.signals=FALSE) {
  results.fun = function(a,game.states=NULL,...) {
    restore.point("payment.pd.results.fun")
    
    pay = c(a[[1]]$pay,a[[2]]$pay)
    m = list(a[[1]]$m,a[[2]]$m)
    a = c(a1=a[[1]]$a,a2=a[[2]]$a)
    
    if (err.D.prob+err.C.prob >1)
      stop("We need err.D.prob+err.C.prob <=1")
 
    err.prob=err.D.prob+err.C.prob
    # Payoffs
    mat = rbind(c(uCC,uCD),
                c(uDC,uDD))
    colnames(mat) = rownames(mat)=c("C","D")
    payoff = c(mat[a[1],a[2]],mat[a[2],a[1]])

    payoff = payoff + c(pay[2], pay[1]) - pay  
    prand = round(runif(1),3)
    
    # Observation with noise
    if (private.signals) {
      obsa1 = obsa2 = a
      rand = runif(1)      
      if (rand<err.prob)
        obsa1[2] = ifelse(rand<err.D.prob,"D","C")      
      rand = runif(1)      
      if (rand<err.prob)
        obsa2[1] = ifelse(rand<=err.D.prob,"D","C")
  
      obs1 = nlist(a=obsa1,pay,m,prand)
      obs2 = nlist(a=obsa2,pay,m,prand)
      
      obs = list(obs1,obs2)
    } else {
      obs = a
      rand = runif(1)      
      if (rand<err.prob)
        obs[1] = ifelse(rand<=err.D.prob,"D","C")      
      rand = runif(1)      
      if (rand<err.prob)
        obs[2] = ifelse(rand<=err.D.prob,"D","C")      
      obs = nlist(a=obs,pay,m,prand)
    }
      
      return(list(payoff=payoff,obs=obs))
  }  
  check.action = function(ai,i,t,...) {
    return()
  }
  example.action = function(i,t,...) {
    return(list(a="C",pay=5,m="Hi"))
  }
  example.obs = function(i,t,...) {
    if (!private.signals) 
      return(list(a=c("D","C"),pay=c(20,0), m =c("Hi","you"), prand=0.3))  
    return(list(list(a=c("D","C"),pay=c(20,0), m =c("Hi","you"),prand=0.3),
                list(a=c("D","C"),pay=c(20,0), m =c("Hi","you"), prand=0.3 )))  
    
  }

  # Observations that are always public
  public.obs = c("pay","m","prand")
  private.obs = list("a","a")
  
  nlist(results.fun, check.action,example.action,example.obs, n=2, private.signals, public.obs, private.obs, params = nlist(uCC,uCD,uDC,uDD,err.D.prob, err.C.prob), sym=TRUE, name="Noisy PD with messages and transfers", score.fun = "efficiency-2*instability- 20*instability^2")
}


#' Generate a (noisy) Prisoners' Dilemma game in which players can conduct messages and payments, as well as perform costly sabotage that makes the other player look like a defector
make.payment.message.sabotage.pd.game = function(uCC=1,uCD=-1,uDC=2,uDD=0,err.D.prob = 0,err.C.prob=0, sabotage.cost=0.3) {
  private.signals=FALSE
  results.fun = function(a,game.states=NULL,...) {
    restore.point("payment.pd.results.fun")
    
    pay = c(a[[1]]$pay,a[[2]]$pay)
    m = list(a[[1]]$m,a[[2]]$m)
    sab = c(a[[1]]$sab,a[[2]]$sab)
    a = c(a1=a[[1]]$a,a2=a[[2]]$a)
    
    if (err.D.prob+err.C.prob >1)
      stop("We need err.D.prob+err.C.prob <=1")
    
    err.prob=err.D.prob+err.C.prob
    # Payoffs
    mat = rbind(c(uCC,uCD),
                c(uDC,uDD))
    colnames(mat) = rownames(mat)=c("C","D")
    payoff = c(mat[a[1],a[2]],mat[a[2],a[1]])
    
    payoff = payoff + c(pay[2], pay[1]) - pay - sabotage.cost*sab
    
    prand = round(runif(1),3)
    
    obs = a
    rand = runif(1)      
    if (rand<err.prob)
      obs[1] = ifelse(rand<=err.D.prob,"D","C")      
    rand = runif(1)      
    if (rand<err.prob)
      obs[2] = ifelse(rand<=err.D.prob,"D","C")
    
    # Sabotage to look the other player like a defector
    if (sab[2]) obs[1] = "D"
    if (sab[1]) obs[2] = "D"
    
    
    obs = nlist(a=obs,pay,m,prand)
    
    return(list(payoff=payoff,obs=obs))
  }  
  check.action = function(ai,i,t,...) {
    return()
  }
  example.action = function(i,t,...) {
    return(list(a="C",pay=5,m="Hi", sab=FALSE))
  }
  example.obs = function(i,t,...) {
    return(list(a=c("D","C"),pay=c(20,0), m =c("Hi","you"), prand=0.3))      
  }
  
  # Observations that are always public
  public.obs = c("pay","m","prand")
  private.obs = list("a","a")
  
  nlist(results.fun, check.action,example.action,example.obs, n=2, private.signals, public.obs, private.obs, params = nlist(uCC,uCD,uDC,uDD,err.D.prob, err.C.prob), sym=TRUE, name="Noisy PD with messages and transfers", score.fun = "efficiency-2*instability- 20*instability^2")
}




