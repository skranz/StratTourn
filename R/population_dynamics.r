
evolve.asymmetric = function(initial=NULL,mat,rounds = 100,alpha=0.1, min.share=0.001) {
  restore.point("evolve.asymmetric")
  if (is.null(initial))
    initial = lapply(mat,function(mat) rep(1/NROW(mat),NROW(mat)))
  shares = list()
  for (i in 1:2) {
    shares[[i]] = matrix(NA,rounds,length(initial[[i]]))
    shares[[i]][1,] = initial[[i]]
    colnames(shares[[i]]) = rownames(mat[[i]])
  }
  for (r in 2:rounds) {
    shares[[1]][r,] = evolve.one.round(shares[[1]][r-1,],shares[[2]][r-1,],mat[[1]],alpha=alpha,min.share=min.share)
    shares[[2]][r,] = evolve.one.round(shares[[2]][r-1,],shares[[1]][r-1,],mat[[2]],alpha=alpha, min.share=min.share) 
  }
  return(shares)
}

plot.evolve.asymmetric = function(shares) {
  shares = shares[[1]]
  rounds = NROW(shares)
  df = as.data.frame(cbind(1:rounds,res.prop))
  colnames(df)[1] = "round"
  mdf = melt(df,id.vars = "round")
  colnames(mdf) = c("round","strategy","share")
  qplot(x=round,y=share,group=strategy,color=strategy,data=mdf, main = "Evolution of strategies", geom="point", size=I(1.2), shape = strategy)
  
}


evolve = function(initial=rep(1/NROW(mat),NROW(mat)),mat,rounds = 100,alpha=0.1, min.shares=0, add.matrix=TRUE) {
  # Start with last row of a matrix
  if (is.matrix(initial)) {
    org.mat = initial
    initial = initial[nrow(initial),]
  } else {
    org.mat = NULL
  }
  
  shares = matrix(NA,rounds,length(initial))
  shares[1,] = initial
  colnames(shares) = rownames(mat)
  for (r in 2:rounds) {
    shares[r,] = evolve.one.round(shares=shares[r-1,],mat=mat,alpha=alpha, min.shares=min.shares) 
  }
  if (add.matrix & !is.null(org.mat)) {
    shares = rbind(org.mat,shares[-1,])
  }
  return(shares)
}

#res.prop = ev
plot.evolve = function(res.prop, direct.labels=suppressWarnings(require(directlabels,quietly=TRUE))) {
  rounds = NROW(res.prop)
  df = as.data.frame(cbind(1:rounds,res.prop))
  colnames(df)[1] = "round"
  mdf = melt(df,id.vars = "round")
  colnames(mdf) = c("round","strategy","share")
  if (!direct.labels) {
    qplot(x=round,y=share,group=strategy,color=strategy,data=mdf, main = "Evolution of strategies", geom="point", size=I(1.2), shape = strategy)
  } else {
    library(directlabels)
    p=qplot(x=round,y=share,color=strategy,data=mdf, main = "Evolution of strategies", geom="point", size=I(1.2))
    direct.label(p) 
  }
}

evolve.one.round = function(shares,shares.j=shares,mat,alpha=0.1, min.shares=0.001) {
  restore.point("evolve.one.round")
  fit = mat %*% shares.j
  shares = pmax(min.shares,shares + alpha*(fit-sum(shares*fit))*shares)
  shares/sum(shares)
}

examples.evolve = function() {
  mat = tourn$mat
  ev = evolve(mat=mat, rounds = 500)
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
  ev = evolve(initial=init.shares,mat=mat, rounds = R,min.shares=0, alpha=0.5)
  plot.evolve(ev,!TRUE)
  next.shares = add.type(ev,"always.defect")
  ev = evolve(initial=next.shares,mat=mat, rounds = 200,min.shares=0, alpha=0.5)
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
  ev = evolve(initial=init.shares,mat=mat, rounds = R,min.shares=min.shares, alpha=1)
  next.shares = add.type(ev,"always.defect")
  ev = evolve(initial=next.shares,mat=mat, rounds = 200,min.shares=min.shares, alpha=1)
  plot.evolve(ev)
  
  
  
  
  init.shares = c(0,0,1,0); names(init.shares) = names
  init.shares = add.type(init.shares,"always.coop",0.01)
  
  R = 100
  ev = evolve(initial=init.shares,mat=mat, rounds = R,min.shares=min.shares, alpha=10)
  plot.evolve(ev)
  next.shares = add.type(ev,"always.defect")
  ev = evolve(initial=next.shares,mat=mat, rounds = R,min.shares=min.shares, alpha=10)
  plot.evolve(ev)
  
  
}