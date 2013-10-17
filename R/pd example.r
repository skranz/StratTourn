make.pd.game = function(uCC=1,uCD=-3,uDC=2,uDD=0,obs.D.prob = 0, obs.C.prob=0, private.signals=FALSE) {
  results.fun = function(a,...) {
    restore.point("pd.results.fun")
    a = unlist(a)
    names(a) = paste0("a",1:2)
    
    if (obs.D.prob + obs.C.prob >1)
      stop("We need obs.D.prob + obs.C.prob <=1")
    
    obs.err.prob = obs.D.prob + obs.C.prob
    
    # Payoffs
    mat = rbind(c(uCC,uCD),
                c(uDC,uDD))
    colnames(mat) = rownames(mat)=c("C","D")
    payoff = c(mat[a[1],a[2]],mat[a[2],a[1]])

    
    # Observation with noise
    if (private.signals) {
      obs1 = obs2 = a
      rand = runif(1)      
      if (rand<obs.err.prob)
        obs1[2] = ifelse(rand<obs.D.prob,"D","C")      
      rand = runif(1)      
      if (rand<obs.err.prob)
        obs2[1] = ifelse(rand<=obs.D.prob,"D","C")
      obs = list(list(a=obs1),list(a=obs2))
      
    } else {
      obs = a
      rand = runif(1)      
      if (rand<obs.err.prob)
        obs[1] = ifelse(rand<=obs.D.prob,"D","C")      
      rand = runif(1)      
      if (rand<obs.err.prob)
        obs[2] = ifelse(rand<=obs.D.prob,"D","C")      
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
    restore.point("check.action.pd")
    stop(paste0("player ",i, "'s strategy in period ",t, " returned an infeasible action: ", ai))
  }
  example.action = function(i,t,...) {
    list(a="C")
  }
  example.obs = function(i,t,...) {
    list(a=c("D","C"))
  }
  
  nlist(results.fun, check.action,example.action,example.obs, n=2, private.signals, a.names = c("a1","a2"), params = nlist(uCC,uCD,uDC,uDD,obs.D.prob                                                                                                                                    ), sym=TRUE, name="Noisy PD")
}


always.coop = function(...) {
  return("C")
}

always.defect = function(obs,i,t,game) {
  return("D")
}

count.tit.for.tat = function(obs,i,t, game, net.nice=0L, ...) {  
  restore.point(paste0("count.tit.for.tat_",t,"_",i))
  #restore.point(paste0("count.tit.for.tat_2_1"))
  
  if (t==1) {
    return(nlist("C",net.nice))
  }
  
  j = 3-i  
  a.num = ifelse(obs$a=="C",1L,0L)
  net.nice = net.nice + a.num[i]-a.num[j]
  if (game$private.signals)
    net.nice = net.nice-game$params$obs.D.prob
  
  if (net.nice <= 1 | runif(1)<(1/(net.nice^2)) ) {
    return(list(a="C",net.nice=net.nice))
  }
  return(list(a="D",net.nice=net.nice))
}


tit.for.tat = function(obs,i,t,game) {
  restore.point(paste0("tit.for.tat_",t,"_",i))
  #restore.point(paste0("tit.for.tat_2_1"))
  
  if (t==1)
    return(list(a="C"))
  # Return the other players previous (observed) action
  j = 3-i
  list(a=obs$a[j])
}

random.action = function(obs,i,t,game) {
  sample( c("C","D"),  1)  
}

grim.trigger = function(obs,i,t,game,coop) {
  restore.point(paste0("grim.trigger_",t,"_",i))
  #restore.point(paste0("grim.trigger_5_2"))
  
  if (t==1)
    return(list("C",coop=TRUE))
  
  # Cooperate if and only if everybody so far has cooperated
  if (coop & obs[1]=="C" & obs[2]=="C") {
    return(list("C",coop=coop))
  } else {
    return(list("D",coop=FALSE))
  }
}
  

examples.pd = function() {
  library(restorepoint)
  library(sktools)
  library(reshape2)
  library(ggplot2)
  
  
  library(restorepoint)
  
  setwd("C:/libraries/StratTourn/StratTourn/R")
  source("battle_of_strategies.r")
  source("multi_random_seed.r")
  
  setwd("C:/libraries/StratTourn/")
  
  game = make.pd.game(private.signals=!TRUE, obs.D.prob=0.2)
  game$sym = FALSE
  strat = nlist(grim.trigger,grim.trigger)

  strat = nlist(count.tit.for.tat,count.tit.for.tat)
  
  set.storing(TRUE)
  options(warn=2)
  ret = run.rep.game(delta=0.9, game=game, strat = strat)
  ret
  as.data.table(ret$hist)
  print(ret$hist,digits=3)
  set.storing(TRUE)
  
  strat = nlist(grim.trigger,tit.for.tat,always.defect, always.coop)
  tourn = init.tournament(game=game, strat=strat, delta=0.9)
  tourn = run.tournament(tourn=tourn, R = 5, backup.each.R=2)
  tourn
  
  save.tournament(tourn)
  save.tournament(tourn,file="tourn.Rdata")
  
  t = load.tournament(file="tourn.Rdata")
  
  print.Tournament(tourn)
  tourn = add.tournament.stats(tourn)
  
  strat = load.strategies("pd_strategies.r")
  game.maker = load.game.maker("pd_game.r")
  game = game.maker(private.signals=FALSE, obs.D.prob=0.1)
  names(strat)  
  set.storing(FALSE)
  ret = run.tournament(delta = 0.98, T = 1000, game=game, strat = strat)
  
  
  run.rep.game(delta=0.95,T=50, game=game, strat=list(count.tit.for.tat,count.tit.for.tat))

  ret = run.tournament(delta = 0.98, T = 1000, game=game, strat = nlist(count.tit.for.tat,always.defect,tit.for.tat, grim.trigger, always.coop))
  
  ret
  bos$stability.weight <<- 20
  
  efficiency = diag(ret)
  stability = efficiency-apply(ret,2,max)
  score = efficiency + bos$stability.weight * stability
  
  ord = order(score,stability, efficiency,decreasing=TRUE)
  data.frame(score,efficiency,stability)[ord,]
  plot(stability,efficiency)
  
   
  shares = evolve(mat=ret,alpha=0.1,rounds=1000)
  plot.evolve(shares)
  

  res.prop = evolve(mat=ret[-3,-3],alpha=0.1,rounds=1000)
  plot.evolve(res.prop)

  shares = c(0.3,0.7,0.5,0.3)
  plot.evolve(evolve(initial=shares,mat=ret,alpha=0.1,rounds=1000,min.share=0.05))
  
}

