
evolve.asymmetric = function(initial=NULL,mat,generations = 100,alpha=0.1, min.share=0.001) {
  restore.point("evolve.asymmetric")
  if (is.null(initial))
    initial = lapply(mat,function(mat) rep(1/NROW(mat),NROW(mat)))
  shares = list()
  for (i in 1:2) {
    shares[[i]] = matrix(NA,generations,length(initial[[i]]))
    shares[[i]][1,] = initial[[i]]
    colnames(shares[[i]]) = rownames(mat[[i]])
  }
  for (r in 2:generations) {
    shares[[1]][r,] = evolve.one.generation(shares[[1]][r-1,],shares[[2]][r-1,],mat[[1]],alpha=alpha,min.share=min.share)
    shares[[2]][r,] = evolve.one.generation(shares[[2]][r-1,],shares[[1]][r-1,],mat[[2]],alpha=alpha, min.share=min.share) 
  }
  return(shares)
}

plot.evolve.asymmetric = function(shares) {
  shares = shares[[1]]
  generations = NROW(shares)
  df = as.data.frame(cbind(1:generations,res.prop))
  colnames(df)[1] = "generation"
  mdf = melt(df,id.vars = "generation")
  colnames(mdf) = c("generation","strategy","share")
  qplot(x=generation,y=share,group=strategy,color=strategy,data=mdf, main = "Evolution of strategies", geom="point", size=I(1.2), shape = strategy)
  
}


get.evolutionary.shares = function(tourn, generations=100, alpha=0.1, min.shares = 1/1000, initial=NULL) {
  mat = get.matches.vs.matrix(tourn = tourn)
  if (!is.null(initial)) {
    if (!is.null(names(initial)))
      initial = initial[rownames(mat)]
   res = evolve(initial=initial,mat=mat, generations=generations, alpha=alpha, min.shares=min.shares) 
  } else {
   res = evolve(mat=mat, generations=generations, alpha=alpha, min.shares=min.shares) 
  }
  return(res[NROW(res),])
}

evolve = function(initial=rep(1/NROW(mat),NROW(mat)),mat,generations = 100,alpha=0.1, min.shares=0, add.matrix=TRUE) {
  # Start with last row of a matrix
  if (is.matrix(initial)) {
    org.mat = initial
    initial = initial[nrow(initial),]
  } else {
    org.mat = NULL
  }
  
  shares = matrix(NA,generations,length(initial))
  shares[1,] = initial
  colnames(shares) = rownames(mat)
  for (r in 2:generations) {
    shares[r,] = evolve.one.generation(shares=shares[r-1,],mat=mat,alpha=alpha, min.shares=min.shares) 
  }
  if (add.matrix & !is.null(org.mat)) {
    shares = rbind(org.mat,shares[-1,])
  }
  return(shares)
}

evolve2 = function(initial=rep(1/NROW(mat),NROW(mat)),vs.mat,generations = 100,alpha=0.1, min.shares=0) {
  s.mat = evolve(initial,mat=vs.mat, generations=generations, alpha = alpha, min.shares = min.shares)
  
  u.mat = t(vs.mat %*% t(s.mat))
  detailed.evolve.return(s.mat, u.mat)
}

#res.prop = ev
plot.evolve = function(res.prop, direct.labels=suppressWarnings(require(directlabels,quietly=TRUE))) {
  generations = NROW(res.prop)
  df = as.data.frame(cbind(1:generations,res.prop))
  colnames(df)[1] = "generation"
  mdf = melt(df,id.vars = "generation")
  colnames(mdf) = c("generation","strategy","share")
  if (!direct.labels) {
    qplot(x=generation,y=share,group=strategy,color=strategy,data=mdf, main = "Evolution of strategies", geom="point", size=I(1.2), shape = strategy)
  } else {
    library(directlabels)
    p=qplot(x=generation,y=share,color=strategy,data=mdf, main = "Evolution of strategies", geom="point", size=I(1.2))
    direct.label(p) 
  }
}

evolve.one.generation = function(shares,shares.j=shares,mat,alpha=0.1, min.shares=0.001) {
  restore.point("evolve.one.generation")
  fit = mat %*% shares.j
  shares = pmax(min.shares,shares + alpha*(fit-sum(shares*fit))*shares)
  shares/sum(shares)
}

# Evolution for n player games 
n.evolve = function(initial=NULL,dt,generations = 100,alpha=0.1, min.shares=0, scale.alpha = max(dt$u)-min(dt$u)) {
  restore.point("n.evolve")

  
  alpha = alpha / scale.alpha
  
  if (is.null(initial)) {
    strats = unique(dt$strat)
    n.strats = length(strats)
    initial = rep(1/n.strats, n.strats)
    names(initial) = strats
  }

  initial = initial / sum(initial)
  shares = u.mat = matrix(NA,generations,length(initial))
  shares[1,] = initial
  colnames(shares) = colnames(u.mat) = names(initial)
  r = 2
  for (r in 2:generations) {
    ret = n.evolve.one.generation(shares=shares[r-1,],dt=dt,alpha=alpha, min.shares=min.shares) 
    shares[r,] = ret$shares
    u.mat[r-1,] = ret$u  
  }
  s.mat = shares

  u.mat[generations,] = n.compute.fitness(shares[generations,], dt)
  
  detailed.evolve.return(s.mat, u.mat)
}

detailed.evolve.return = function(s.mat, u.mat) {
  df = as.data.frame(s.mat)
  df$generation = 1:NROW(df)
  smdf = melt(df,id.vars = "generation")
  colnames(smdf) = c("generation","strategy","share")
  
  df = as.data.frame(u.mat)
  df$generation = 1:NROW(df)
  umdf = melt(df,id.vars = "generation")
  colnames(umdf) = c("generation","strategy","u")
  
  mdf = merge(smdf,umdf, by=c("generation","strategy"))
  mdf$strat = mdf$strategy
  
  mdf$time = as.Date(paste0(mdf$generation+2000,"-01-01"))
  list(grid=mdf, shares.mat=s.mat, u.mat=u.mat)
  
}

n.evolve.one.generation = function(shares,dt,alpha=0.1, min.shares=0.001) {
  restore.point("n.evolve.one.generation")
  
  fit = n.compute.fitness(shares,dt)
  shares = pmax(min.shares,shares + alpha*(fit-sum(shares*fit))*shares)
  
  list(shares=shares/sum(shares), u=fit)
}


n.compute.fitness = function(shares,dt) {
  restore.point("n.compute.fitness")
  # Compute new fitness
  dt$strat.prob = shares[dt$strat] 
  dt = mutate(group_by(dt, match.id ), other.prob = prod(strat.prob) / strat.prob)
  
  plot(dt$other.prob, dt$other.sample.prob, log="xy")
  hist(dt$other.prob)
  
  
  u.mean = summarise(group_by(dt,strat), u = sum(u*other.prob*u.weight / other.sample.prob) / sum(other.prob*u.weight/other.sample.prob))
  fit = u.mean$u
  names(fit) = u.mean$strat
  fit = fit[names(shares)]
  fit
}

examples.evolve = function() {
  mat = tourn$mat
  ev = evolve(mat=mat, generations = 500)
  plot.evolve(ev)
}

add.type = function(shares,add.id,start.share = 0.01) {
  if (is.matrix(shares)) {
    org.mat = shares
    shares = shares[nrow(shares),]
  } else {
    org.mat = NULL
  }
  shares[add.id] = pmax(shares[add.id],start.share)
  shares = shares / sum(shares)
  if (!is.null(org.mat)) {
    shares = rbind(org.mat,shares)
  }
  shares
  
}
 
path.to.defect = function() {
  
  
  # Goal: Try to destabilize a society of tit-for-tats
  # Winning Condition: For at least 100 consequitive periods your society has a share of always.defects of at least 90%
  # What you can do: Besides tit.for.tat and always.defect  you can invent 3 additional strategies
  # The society starts with 100% tit.for.taters
  # In every period, you can introduce a new strategy into your society that will be followed by 1% of inhabitants
  # Populations evolve according to some standard evolutionary dynamic
  
  game = make.pd.game(err.D.prob=0.15)
  
  # First part run tournament for all strategies to determine relative fitness
  delta = 0.95
  strat = nlist(tit.for.tat, always.defect, always.coop)
  tourn = init.tournament(game=game, strat=strat, delta=delta)
  tourn = run.tournament(tourn=tourn, R = 1)
  tourn
  mat = tourn$mat
  names = colnames(mat)
  names
  
  
  
  # Set initial shares  
  init.shares = c(1,0,0); names(init.shares) = names
  init.shares = add.type(init.shares,"always.coop",0.01)
  #init.shares = add.type(init.shares,"nice.tft",0.01)
  
  # Start evolution
  R = 1
  ev = evolve(initial=init.shares,mat=mat, generations = R,min.shares=0, alpha=0.5)
  plot.evolve(ev,!TRUE)
  next.shares = add.type(ev,"always.defect")
  ev = evolve(initial=next.shares,mat=mat, generations = 200,min.shares=0, alpha=0.5)
  plot.evolve(ev)
  
  
  
  library(StratTourn)  
  # First part run tournament for all strategies to determine relative fitness
  delta = 0.95
  strat = nlist(tit.for.tat, always.defect, nice.tft, always.coop)
  tourn = init.tournament(game=game, strat=strat, delta=delta)
  tourn = run.tournament(tourn=tourn, R = 10)
  tourn
  mat = tourn$mat
  names = colnames(mat)
  names
  
  
  
  # Set initial shares  
  init.shares = c(1,0,0,0); names(init.shares) = names
  init.shares = add.type(init.shares,"always.coop",0.01)
  init.shares = add.type(init.shares,"nice.tft",0.01)
  
  # Start evolution
  R = 100
  ev = evolve(initial=init.shares,mat=mat, generations = R,min.shares=min.shares, alpha=1)
  next.shares = add.type(ev,"always.defect")
  ev = evolve(initial=next.shares,mat=mat, generations = 200,min.shares=min.shares, alpha=1)
  plot.evolve(ev)
  
  
  
  
  init.shares = c(0,0,1,0); names(init.shares) = names
  init.shares = add.type(init.shares,"always.coop",0.01)
  
  R = 100
  ev = evolve(initial=init.shares,mat=mat, generations = R,min.shares=min.shares, alpha=10)
  plot.evolve(ev)
  next.shares = add.type(ev,"always.defect")
  ev = evolve(initial=next.shares,mat=mat, generations = R,min.shares=min.shares, alpha=10)
  plot.evolve(ev)
  
  
}