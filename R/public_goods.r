
# The code inside can be used to explore the behavior of strategies for the PD game
examples.pg = function() {
  #Load package
  library(StratTourn)
  
  # Generate a game object
  game = make.pg.game(n=3)

  # Pick a pair of strategies
  strat = nlist(give.all.pg, random.pg, punish.pg)
  
  # Let the strategies play against each other
  run.rep.game(delta=0.7, game=game, strat = strat)
  
   
  # Init and run a tournament of several strategies against each other  
  set.storing(FALSE)
  # Pick strategies
  strat = nlist(give.all.pg,random.pg, punish.pg, defect.pg)
  tourn = init.tournament(game=game, strat=strat)
  tourn = run.tournament(tourn=tourn, R = 50)
  tourn$dt
  
  dt = tourn$dt
  
  table(dt$strat)
  save.tournament(tourn)
  
}


# A strategy that always cooperates
give.all.pg = function(i,t, obs, max.x, max.pun, n, ...) {
  nlist(x=max.x, pun=rep(0,n))
}


# A strategy that always cooperates
defect.pg = function(n, ...) {
  nlist(x=0, pun=rep(0,n))
}


# A strategy that always cooperates
punish.pg = function(t,i, obs, n, max.x, ...) {
  if (t==1)
    return( nlist(x=max.x, pun=rep(0,n)))
  
  pun = 100-obs$x
  nlist(x=max.x, pun=pun)
}



# A strategy that always defects
random.pg = function(i,t, obs, max.x, max.pun, n, ...) {
  x = sample.int(max.x+1, 1)-1
  pun = sample.int(max.pun+1,n, replace=TRUE)-1
  nlist(x=x, pun=pun)
}

#' Generate a public goods game
make.pg.game = function(n=5,MPCR=2/n, pun.cost = 1/3, max.x=100, max.pun=200,delta=0.9, private.signals=FALSE,...) {
  
  run.stage.game = function(a, t,t.obs,...) {
    restore.point("pg.run.stage.game")
    x = sapply(a, function(ai) ai$x)
    x = round(x)
    x[x<0]=0
    x[x>max.x]=max.x
    
    pun = do.call(rbind,lapply(a, function(ai) ai$pun))
    
    pun.unscaled = pun
    pun[pun<0]=0
    
    pun.got = colSums(pun)
    pun.scale = pmin(1,max.pun / pun.got)
    pun.got = pun.got * pun.scale
    
    pun.scale.mat = matrix(pun.scale, NROW(pun), NCOL(pun), byrow=TRUE)
    pun = pun * pun.scale.mat
    
    pun.made = rowSums(pun)
    
    X = sum(x)
    
    
    payoff = X*MPCR - x - pun.got - pun.made * pun.cost
    obs = nlist(x,pun)  
    round.stats = quick.df(t=rep(t,n),i=1:n,u=payoff,x=x, pun.made=rowSums(pun.unscaled), pun.got = colSums(pun.unscaled)) 
    
    colnames(pun.unscaled) = paste0("pun",1:n)
    round.stats = cbind(round.stats, pun.unscaled)
    
    return(list(payoff=payoff,obs=obs, round.stats=round.stats))
  }
      
  check.action = function(ai,i,t,...) {
    return()
    #restore.point("check.action.pd")
    stop(paste0("player ",i, "'s strategy in period ",t, " returned an infeasible action: ", ai))
  }
  example.action = function(i=1,t=1,...) {
    list(x=0, pun=rep(0,n))
  }
  example.obs = function(i=1,t=1,...) {
    list(x=rep(0,n), pun=matrix(0,n,n))
  }
  li = nlist(run.stage.game, check.action,example.action,example.obs, n, private.signals, params = nlist(MPCR, pun.cost, max.x, max.pun), sym=TRUE, delta=delta, name="Public goods game", show.par = nlist(max.x,max.pun,n))
  li
  
}

adjust.pd.rs.dt = function(rs.dt) {
  #rs.dt[, obs.i :=lag(obs.i)]
  #rs.dt[, obs.j :=lag(obs.j)]
  #rs.dt[, err.D.i :=lag(err.D.i)]
  #rs.dt[, err.D.j :=lag(err.D.j)]
}
  

