examples.analyse.results = function() {
  rs = fread("D:/libraries/StratTourn/Tourn_Noisy_PD_20140721_095918_rs.csv")
  rg = group_by(rs, match.id,i)
  
  rg = mutate(rg,
    choose.C = (a == "C")*1,
    choose.D = (a == "D")*1,
    other.D1 = obs.j == "D",
    other.D2 = is.true(obs.j == "D" & lag(obs.j) == "D")*1,
    own.D.cum = cumsum(obs.i=="D"),
    other.D.cum = cumsum(obs.j=="D"),
    net.nice = other.D.cum - own.D.cum,
    error.D.i = lag(err.D.i, default=FALSE)*1,
    error.D.j = lag(err.D.j, default=FALSE)*1,
    error.D.i.cum = cumsum(error.D.i),
    error.D.j.cum = cumsum(error.D.j),
    error.D.i.once = (cumsum(error.D.i)>0)*1,
    error.D.j.once = (cumsum(error.D.j)>0)*1,  
    other.strat = ""
  )
  # Set other strategy
  #rg = mutate(group_by(rg,match.id,t), other.strat = rev(strat))
  rg = arrange(group_by(rg),match.id,t,i)
  rows = which(rg$i==1)
  rg[rows]$other.strat = rg$strat[rows+1]
  rg[rows+1]$other.strat = rg$strat[rows]
    
  lm(choose.D ~ own.D+other.D+net.nice+t, data=rg)
  
  reg = function(formula, dat, reg.fun=lm,..., round=4) {
    reg.res = reg.fun(formula, data=dat,...)
    r.sqr = summary(reg.res)$r.squared
    ret = c(coef(reg.res), num.obs=NROW(dat), r.sqr=r.sqr)
    do.call("quick.df",as.list(round(ret,round)))
  }
  reg(choose.D ~ other.D.cum+net.nice+t,rg)

  tab = do(group_by(rg, strat, other.strat), reg(u ~ error.D.i.once+error.D.i.cum+error.D.j.once+error.D.j.cum+I(t>1), .))
  tab  

  
  
  do(group_by(rg, strat), reg(choose.D ~ other.D1+other.D.cum+other.D2+net.nice, .))
  tab = do(group_by(rg, strat), reg(u ~ other.D1+other.D.cum+other.D2+net.nice+other.strat, .))
  
  tab = do(group_by(rg, strat, other.strat), reg(u ~ other.D1+other.D.cum+other.D2+net.nice, .))
  tab 
  
  rg$ind = make.condition.indicator(rg, cond=c(
    "error.D.i.cum > 0","t==1", "error.D.j.cum > 0"    
  ))
  tab = summarise(group_by(rg, strat, other.strat,ind), mean(u), num.obs = length(u))
  tab 

  tab = summarise(group_by(rg, strat), mean(u), num.obs = length(u))
  tab 
  
  tab = summarise(group_by(rg, ind,strat), mean(u), num.obs = length(u))
  tab 

  
  rg.li = split(rg, rg$ind)
  res = lapply(rg.li,get.rs.vs.matrix)
  res
  
  fun = function(dat) {
    as.data.frame(get.rs.vs.matrix(dat, var="u", round=4))
  }
  tab = do(group_by(rg, ind), fun(.))
  tab 

  get.rs.vs.matrix
}

add.other.var = function(dt, var="strat", other.var = paste0("other.",var)) {
  #rg = arrange(group_by(rg),match.id,t,i)
  rows = which(dt$i==1)
  for (i in seq_along(var)) {
    dt[[other.var[i]]] = dt[[var[i]]]
    dt[[other.var[i]]][rows] = dt[[var[i]]][rows+1]
    dt[[other.var[i]]][rows+1] = dt[[var[i]]][rows]
  }
  dt
}

make.condition.indicator = function(dt, cond, other.label="other") {
  #dt = rg
  #cond = c("other.D1 == 1 & t>1", "t==1", "t==2")
  n = NROW(dt)
  ind = rep("",n)
  
  for (i in seq_along(cond)) {
    rows = eval_text(cond[[i]], envir=dt)
    ind[rows] = paste0(ind[rows]," & ",cond[[i]],"", sep="")
    ind
  }
  ind
  ind = substring(ind,3)
  ind[ind==""] = other.label
  ind
}


get.rounds.vs.matrix = function(d, var="u", round=4) {
  #d = rg
  
  d = s_mutate(d, paste0("VAR=",var))

  
  ds = summarise(group_by(d,strat, other.strat), mean = mean(VAR))
  
  
  grid = cbind(expand.grid(unique(d$strat), unique(d$other.strat)),NA)
  colnames(grid) = c("strat", "other.strat", "mean")
  ds = rbind(ds,grid)
  rows = !duplicated(select(ds, strat, other.strat))
  ds = ds[rows,]
  ds = arrange(ds, strat, other.strat)

  strats = unique(ds$strat)
  other.strats = unique(ds$other.strat)

  
  mat = matrix(ds$mean, nrow=length(strats), ncol=length(other.strats), byrow=TRUE)
  rownames(mat) = strats
  colnames(mat) = other.strats

  dto = summarise(group_by(d,strat), mean = mean(VAR))
  total.mean = dto$mean
  names(total.mean) = dto$strat

  total.mean = total.mean[strats]  
  ord = order(-total.mean)
  mat = mat[ord,]
  if (identical(strats, other.strats))
    mat = mat[,ord]
  mat = cbind(total=total.mean[ord],mat)
  
  #mat
  round(mat,round)
}

mean.over.matches = function(dt=tourn$dt, tourn,var) {
  restore.point("mean.over.matches")
  d = s_select(dt, paste0("strat, u.weight,", var))
  dg = group_by(d, strat)
  ds = s_summarise(dg, paste0(var," = sum( ", var," * u.weight)/sum(u.weight)"))
  ds
}

add.strat.shares = function(dt, shares=NULL, add.other.share = "other.strat" %in% colnames(dt), overwrite=FALSE) {
  restore.point("add.strat.shares")
  if (is.null(shares)) {
    strats=unique(dt$strat)
    if (is.null(dt$share) | overwrite)
      dt$share= 1 / length(strats)
    if (is.null(dt$other.share) | overwrite)
      dt$other.share= 1 / length(strats)
  } else {
    if (is.null(dt$share) | overwrite)
      dt$share= shares[dt$strat]
    if (is.null(dt$other.share) | overwrite)
      dt$other.share= shares[dt$other.strat]    
  }
  dt
}

strat.rank.from.matches = function(dt=tourn$dt, tourn,var=NULL, add.var = !is.null(var)) {
  restore.point("strat.rank.from.matches")
  if (is.null(var))
    var = "u"
  
  dt = add.strat.shares(dt)
  
  d = s_select(dt, paste0("strat, u.weight,other.share,share,", var))
  setnames(d, var, "VARIABLE")
  dg = group_by(d, strat)
  ds = summarise(dg, share = round(share[1]*100,1),mean = sum(VARIABLE*u.weight*other.share)/sum(u.weight*other.share), se = wtd.se(VARIABLE,u.weight*other.share), num.obs = length(VARIABLE))
  if (add.var)
    ds$var = var

  ds = arrange(ds, -mean)
  ds = mutate(ds, rank=seq_along(mean), low = mean-se, up=mean+se)
  sigma.rank = rep(0, NROW(ds))
  for (i in 1:NROW(ds)) {
    sigma.rank[i] = sum(ds$low > ds$up[i])+1 
  }
  ds$sigma.rank = sigma.rank
  if (add.var) 
    return(select(ds, strat,share,var, rank, sigma.rank, mean, se, up, low, num.obs))
  return(select(ds, strat,share, rank, sigma.rank, mean, se, up, low, num.obs))
}

eval_text = function(code, subst=NULL, envir=parent.frame()) {
  if (!is.null(subst)) {
    code = gsub(subst[1],subst[2],code)
  }
  expr = parse(text=code,srcfile=NULL)
  eval(expr,envir)
}

example.add.strat.stats = function() {
  dt = add.strat.stats(dt=tourn$dt)
  summarise(group_by(dt, strat), cor(u,other.u), cor(u, delta.u))
  get.var.tourn.stats(var="delta.u",dt=dt)

  lm(u ~ other.u +T , weights=dt$weights.u, data=dt)

  reg = function(dat,formula = u ~ other.u +T, weights=dat$u.weight) {
    restore.point("reg")
    res = lm(formula,data=dat, weights=weights)
    as.data.frame(t(round(coef(res),5)))
  }
  reg(dt)
  reg(dat=dt, formula=u~delta.u+T)

  do(group_by(dt,strat), reg(dat=., formula=u~I(T<=5)))
  do(group_by(dt,strat), reg(dat=.))
  do(group_by(dt,strat), reg(dat=., formula=u~delta.u+T))

  
  summarise(group_by(dt, strat), cor(u,other.u), cor(u, delta.u))
  
  
}

add.strat.stats = function(var="u",dt=tourn$dt, tourn) {
  dg = group_by(dt, match.id)
  code = 
"  
  mutate(dg,
    other.VAR = (sum(VAR)-VAR) / (length(VAR)-1),
    delta.VAR = VAR - other.VAR  
  )
"  
  dt = eval_text(code, subst = c("VAR",var))
  dt
}

examples.get.matches.vs.matrix = function() {
  get.matches.vs.matrix(md, br.sign="*")
}

get.matches.vs.matrix = function(dt=tourn$dt, tourn, var="u", br.sign=NULL, round=3) {
  restore.point("get.matches.vs.matrix")

  d = copy(dt)
  d$VAR = d[[var]]
  d = select(d,match.id,i,strat,VAR,u.weight)
  d1 = filter(d, i==1)
  d2 = filter(d, i==2)
  dw = data.table(match.id=d1$match.id, strat1=d1$strat, strat2=d2$strat, u1=d1$VAR, u2=d2$VAR, u.weight=d1$u.weight)
  dw = arrange(dw, strat1, strat2)
  #importance of matching
  dw = mutate(group_by(dw,strat1, strat2), imp=u.weight/sum(u.weight))
  ds1 = summarise(group_by(dw,strat1, strat2), u1=sum(u1*imp), w=length(u1))
  ds2 = summarise(group_by(dw,strat1, strat2), u1=sum(u2*imp), w=length(u2))
  setnames(ds2,c("strat1","strat2"),c("strat2","strat1"))
  dsl =rbind(ds1, ds2, use.names=TRUE)
  ds = summarise(group_by(dsl, strat1, strat2), u=sum(u1*w)/sum(w))
  
  strats = unique(ds$strat1)
  ns = length(strats)

  # Assume all matchings exist
  if (NROW(ds)==ns*ns) {
    mat = matrix(ds$u,ns,ns,byrow=TRUE)
  } else {
    warning("Not all matchings exist.")
    return(NULL)
  }
  if (!is.null(round)) {
    mat = round(mat,round)
  }
  
  if (!is.null(br.sign)) {
    max.row =max.col(t(mat))
    smat = matrix("",NROW(mat),NCOL(mat))
    smat[cbind(max.row,1:NCOL(mat))] = "*"
    mat = matrix(paste0(mat,smat),NROW(mat),NCOL(mat))
  }
  colnames(mat) = rownames(mat) = strats

  mat
}

get.matches.vs.grid = function(dt=tourn$dt, tourn, var="u") {
  restore.point("get.matches.vs.matrix")
  
  dt = add.strat.shares(dt)

  num.u = NROW(dt)
  weight.factor = num.u / sum(dt$u.weight)
  
  d = copy(dt)
  d$VAR = d[[var]] * d$u.weight * weight.factor
  #d = s_mutate(dt, paste0("VAR=",var,"*u.weight * weight.factor"))
  d = select(d,match.id,i,strat,share, VAR)
  d1 = filter(d, i==1)
  d2 = filter(d, i==2)
  dw = data.table(match.id=d1$match.id, strat1=d1$strat, strat2=d2$strat, u1=d1$VAR, u2=d2$VAR, share1=d1$share, share2=d2$share)
  dw$pair = paste0(dw$strat1," - ",dw$strat2)
  ds = summarise(group_by(dw,pair),strat1=strat1[1], strat2=strat2[1], u1=mean(u1),u2=mean(u2), obs=length(u1), share1=share1[1], share2=share2[1])
  ds$share1x2 = ds$share1* ds$share2
  
  ds = as.data.frame(ds)
  colnames(ds)[4:5] = paste0(var,1:2)
  #ds[,4:5] = round(ds[,4:5],round)
  ds
}