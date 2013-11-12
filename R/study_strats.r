# Utilities for helping to find best answers

examples.study.strats.and.answers.par = function() {
  library(StratTourn)
  # A strategy that cooperates with probability probC
  # The function nests always.coop (probC=1) and always.defect (probC=0)
  mix = function(obs,t,i,game, probC = 0.5, ...) {
    if (runif(1)<=probC) return(nlist(a="C"))
    return(nlist(a="D"))
  }
  
  # A noisy PD game
  set.storing(TRUE)
  
  game = make.pd.game(err.D.prob=0.15)

  sim = NULL
  # Study performance of mix for different parameters
  sim = study.strats.and.answers(
    strats = nlist(mix), answers=nlist(mix),
    strat.par = list(probC = c(0,0.1,0.5,1)),
    answer.par=list(probC = seq(0,1,length=5)),
    R=50, delta=0.95, sim=sim,game=game
  )
  plot(sim)
  
  
  
  sim = NULL
  # Study performance of tit.for.tat against variants of mix
  sim = study.strats.and.answers(
    strats = nlist(tit.for.tat), answers=nlist(mix),
    strat.par = NULL,
    answer.par=list(probC = seq(0,1,length=5)),
    R=5, delta=0.95, sim=sim,game=game
  )
  head(sim)
  plot(sim)

  
  sim = NULL
  # Study performance of tit.for.tat against variants of mix
  sim = study.strats.and.answers(
    strats = nlist(tit.for.tat), answers=nlist(mix),
    strat.par = NULL,
    answer.par=list(probC = seq(0,1,length=5)),
    R=5, delta=0.95, sim=sim,
    game.fun=make.pd.game,
    game.par = list(err.D.prob = c(0,0.05,0.15))
  )
  head(sim)
  plot(sim)
  
  sim  
}


plot.StratsAnswersStudy = function(sim,...) {
  
  if (length(sim$answers)==0)
    return(plot.StratsStudy(sim$s))
  
  library(ggplot2)
    
  answer.par.names = str.combine("answer_",sim$answer.par)
  par = c(answer.par.names,sim$strat.par,sim$game.par, "strat","delta", "answer")
  
  
  par.len = sapply(par, function(p) length(unique(sim$sa$agg[,p])))
  need.facet = par.len > 1
  if (sum(need.facet)>3) {
    message(paste0("Sorry, we can plot so far only variations in 3 dimension, yet you have variations in ", paste0(par[need.facet], collapse=",")))
    return(NULL)
  }
  ord = order(need.facet, decreasing = TRUE)
  par = par[ord]; need.facet = need.facet[ord]
  
  if (par[1]=="answer" | par[1]=="strat") {
    message("Sorry, plotting so far only works if you have a strat.par, answer.par, or game.par with more than one numeric level that can be plotted to the x-Axis. Here is just the data: ")
    return(sim$sa$agg)
    #aes = aes_string(x=par[1],y="u.mean")
    #facet = facet_grid(paste0(par[2],"~",par[1]),labeller = label_both)
        
    #ggplot(data=sim$sa$agg) + aes +
    #  geom_bar(stat="identity", fill="red", colour="red", alpha=0.2)
    #  geom_bar(aes(y=u.mean.strat), stat="identity", fill="blue", colour="blue", alpha=0.2)
  }
  
  
  aes = aes_string(x=par[1], y="u.mean")
  facet = facet_grid(paste0(par[3],"~",par[2]),labeller = label_both)

  min.y = min(c(min(sim$sa$agg$ci.lower),min(sim$s$agg$ci.lower)))
  max.y = max(c(max(sim$sa$agg$ci.upper),max(sim$s$agg$ci.upper)))
  
  diff = (max.y-min.y)
  min.y = max(min(c(sim$s$agg$score, min.y - 0.01*diff)), min.y - 0.2*diff)
  max.y = max.y + 0.05*diff

  if ((par[1] == "answer" & need.facet[1]) |
      (par[2] == "answer" & need.facet[2]) |
      (par[3] == "answer" & need.facet[3])) {
    score.geom = NULL    
  } else {
    score.geom = geom_line(aes(y=score), colour="green")
  }
  
  ggplot(data=sim$sa$agg) + aes +
    geom_smooth(aes(ymin = ci.lower, ymax = ci.upper), stat="identity", fill="red", colour="red", alpha=0.2)+
    geom_smooth(aes(y=u.mean.strat,ymin = ci.lower.strat, ymax = ci.upper.strat), stat="identity", fill="blue", colour="blue", alpha=0.2)+
    score.geom+
    facet +
    coord_cartesian(ylim = c(min.y, max.y))+
    ylab("blue=u.strat red=u.answer green=score")+NULL
  
}


plot.StratsStudy = function(sim) {
  library(ggplot2)

  par = c(sim$strat.par,"delta",sim$game.par,"strat")
  par.len = sapply(par, function(p) length(unique(sim$agg[,p])))
  need.facet = par.len > 1
  if (sum(need.facet)>2) {
    message(paste0("Sorry we can plot so far only variations in 3 dimension, yet you have variations in ", paste0(par[need.facet], collapse=",")))
    return(NULL)
  } 
  ord = order(need.facet, decreasing = TRUE)
  par = par[ord]
  
  aes = aes_string(x=par[1], y="u.mean")
  facet = facet_grid(paste0(par[3],"~",par[2]),labeller = label_both)
  
  min.y = min(c(min(sim$agg$ci.lower),min(sim$agg$ci.lower)))
  max.y = max(c(max(sim$agg$ci.upper),max(sim$agg$ci.upper)))
  
  diff = (max.y-min.y)
  min.y = min.y - 0.03*diff
  max.y = max.y + 0.03*diff
  
  
  ggplot(data=sim$agg) + aes +
   # geom_line(colour="red")+
    geom_smooth(aes(ymin = ci.lower, ymax = ci.upper), stat="identity", fill="blue", alpha=0.2, colour="blue")+
    facet +
    coord_cartesian(ylim = c(min.y, max.y))+
    ylab("Mean payoff") 
  
}
#' A helper function to find best answers to a strategy
#' 
#' The function is based on simulation.study in sktools
#' 
#' @param strats a named list of strategies to study
#' @param answers a named list of answer strategies to study 
#' @param strat.par either NULL or a list with different parameters of strat that shall be studied. (May not run if strats contains more than one strategy)
#' @param answer.par either NULL or a list with different parameters of the answer strategy strat that shall be studied.
#' @param sim results from a previous call to study.strat.and.answer. New simulations will just be added.
#' @param R number of repetions of the simulated matches
#' @param delta a discount factor or a vector of different discount factors that shall be studied
#' @param ci a number between 0 and 1 describing the confidence niveau of the expected payoffs that will be shown in the plots
#' @param score.fun a string containing the formula for the score function 
#' @param game a game object, if NULL then game.fun must be provided
#' @param game.fun a function that generates a game object, like make.pd.game. If NULL game must be provided
#' @param game.par either NULL or a list with values for game parameters that shall be studied (a game parameter is an argument of game.fun)
#' @export

study.strats.and.answers = function(strats,answers=NULL, strat.par=NULL, answer.par=NULL, game=NULL, delta=0.9, R = 5, extra.strat.par = NULL,extra.answer.par=NULL, ci = 0.9, sim, score.fun = "efficiency-2*instability-20*instability^2", game.fun=NULL, game.par=NULL, verbose=interactive(), disable.restore.point=TRUE) {
  restore.point("study.strats.and.answers")
  
  seeds = draw.seed(R)
  
  strat.names = names(strats)
  answer.names = names(answers)  
  
  if (is.null(sim)) {
    sim = list(strats=strat.names, answers = answer.names, answer.par = names(answer.par), strat.par = names(strat.par),game.par = names(game.par),sa = list(), s = list())
    class(sim) = c("StratsAnswersStudy","list")
  } else {
    sim$strats = union(sim$strats,strat.names)
    sim$answers = union(sim$answers, answer.names)
  }

  sim$s = study.strats(strats, R, strat.par=strat.par, extra.strat.par=NULL, sim=sim$s, game, delta, seeds=seeds, game.fun=game.fun, game.par=game.par,ci=ci, disable.restore.point=disable.restore.point)
  
  if (length(answer.names)>0) {
    sim$sa = study.answers(strats,answers, R, strat.par,answer.par,  extra.strat.par, extra.answer.par, sim$sa, game, delta,verbose, seeds=seeds, game.fun=game.fun, game.par=game.par,ci=ci, disable.restore.point=disable.restore.point)
  
  }
  if (length(sim$answers)>0)  {
    sim = add.score.to.study(sim, score.fun=score.fun)
    
    keys = c("strat","delta",sim$game.par,sim$strat.par,sim$strat)
    sim$sa$agg = merge(x=sim$sa$agg,y=sim$s$agg, by=keys,all=TRUE,suffixes=c("",".strat"))
    
  }
  return(sim)  
}

add.score.to.study = function(sim, score.fun) {
  restore.point("add.score.to.study")

#   keys = c("strat","delta",sim$strat.par,sim$game.par)
#   dat = merge(sim$sa$dat, sim$s$dat[,c(keys,"u")],by=keys, suffixes = c("",".strat"))
#   head(dat)
#   head(sim$sa$dat)
#   sa.dat = sim$sa$dat
#   s.dat = sim$s$dat
#   
#   dat$efficiency = dat$u.strat
#   dat$instability = dat$u - dat$efficiency
#   dat$score = eval(base::parse(text=score.fun), dat)
  

  agg = sim$s$agg
  keys = c("strat","delta",sim$strat.par,sim$game.par)
  uba.df = quick.by(sim$sa$agg,by=keys, "u.best.answer = max(u.mean)") 
  agg = merge(agg,uba.df, by=keys)
  
  # Instablity and score
  agg$efficiency = agg$u.mean
  agg$instability = agg$u.best.answer-agg$efficiency
  agg$score = eval(base::parse(text=score.fun), agg)
  
  sim$s$agg = agg
  return(sim)
}

study.answers = function(strats,answers, R=1, strat.par=NULL,answer.par = NULL,  extra.strat.par=NULL, extra.answer.par=NULL, sim=NULL, game=NULL, delta,verbose=interactive(), seeds = draw.seed(R), game.fun=NULL, game.par=NULL,ci=0.9, disable.restore.point=TRUE) {
  restore.point("study.answers")

  strat.names = names(strats)
  answer.names = names(answers)
  
  # rename answer_par so that we can have same original parameter names for strat and answer
  if (length(answer.par)>0) {
    names(answer.par) = paste0("answer_", names(answer.par))
  }
  
  run.one.game = function(strat,answer,delta,...) {
    args = list(...)    
    spar = c(args[intersect(names(strat.par),names(args))],extra.strat.par)
    apar = c(args[intersect(names(answer.par),names(args))])
    names(apar) = substring(names(apar),8) # strip off "answer_"
    apar = c(apar,extra.answer.par)
    
    if (!is.null(game.fun)) {
      gpar = c(args[intersect(names(game.par),names(args))])
      game = do.call(game.fun, gpar)
    }
    
    u = run.rep.game(strat=c(strats[strat],answers[answer]), game=game,delta=delta, strat.par = list(first=spar,second=apar), detailed.return=FALSE)
    return(u[2])    
  }
  
  library(compiler)

  par = c(game.par,strat.par,answer.par)
  par.names = names(par)
  
  if (verbose)
    cat(paste0("Strategies vs answers... \n"))
  
  
  was.storing = is.storing();set.storing(!disable.restore.point);library(compiler);enableJIT(3)
  dat = simulation.study(run.one.game, par = c(list(strat=strat.names, answer=answer.names, delta=delta),par), repl=R, seeds = seeds)
  enableJIT(0); set.storing(was.storing)
  colnames(dat)[NCOL(dat)] = "u"
  
  enableJIT(0)
  
  if (!is.null(sim)) {
    dat = rbind(sim$dat,dat)
  } else {
    sim = list()
    class(sim) = c("AnswersStudy")
    
  }
  
  agg = quick.by(dat, by=c("strat","answer","delta",par.names),"u.mean = mean(u), u.sd = sd(u), R=length(u)" )
  agg
  
  # Add confidence interval based on normal-distribution
  error <- qnorm(1-((1-ci)/2)) * agg$u.sd/sqrt(agg$R) 
  agg$ci.lower = agg$u.mean - error 
  agg$ci.upper = agg$u.mean + error
  
  sim$dat = dat
  sim$agg = agg
  sim
}

study.strats = function(strats, R=1, strat.par=NULL, extra.strat.par=NULL, sim=NULL, game, delta,verbose=interactive(), seeds = draw.seed(R), game.fun=NULL, game.par = NULL, ci=0.9, disable.restore.point=TRUE) {
  restore.point("study.strats")
  
  strat.names = names(strats)
  
  # Compute payoff of strategy against itself
  strats.par = list(strat.par, strat.par)
  run.against.itself = function(strat,delta,...) {
    args = list(...)    
    strat = as.character(strat)
    spar = c(args[intersect(names(strat.par),names(args))],extra.strat.par)
    if (!is.null(game.fun)) {
      gpar = c(args[intersect(names(game.par),names(args))])
      game = do.call(game.fun, gpar)
    }
    
    u = run.rep.game(list(strats[[strat]],strats[[strat]]), game=game,delta=delta, strat.par = list(spar,spar), detailed.return=FALSE)
    return(u[2])    
  }
  
  if (!is.null(sim)) {
    dat = sim$dat
    agg = sim$agg
  } else {
    sim = list()
    class(sim) = c("StratsStudy")
  }
  
  sim$strats = union(sim$strats,strat.names)
  sim$strat.par = names(strat.par)
  sim$game.par = names(game.par)
  
  if (verbose)
    cat(paste0("Strategies play against themselves... \n"))
  
  was.storing = is.storing(); set.storing(!disable.restore.point);library(compiler); enableJIT(3)
  dat = simulation.study(run.against.itself,par=c(list(strat=strat.names, delta=delta),strat.par, game.par), repl=R, seeds = seeds)
  enableJIT(0); set.storing(was.storing)
        
  colnames(dat)[NCOL(dat)] = "u"
  dat = rbind(sim$dat,dat)
  sim$dat = dat

  par.names = c(names(strat.par),names(game.par))
  agg = quick.by(dat, by=c("strat","delta", par.names),"u.mean = mean(u), u.sd = sd(u), R=length(u)" )
  
  error <- qnorm(1-((1-ci)/2)) * agg$u.sd/sqrt(agg$R) 
  agg$ci.lower = agg$u.mean - error 
  agg$ci.upper = agg$u.mean + error
  
  sim$agg = agg
  return(sim)
}

#' Study distribution of actions and states in each period in a match of the given strategies
#' @param strats a list of the n strategies required for a repeated game
#' @param game the game object
#' @param delta the discount factor
#' @param T the number of periods that shall be studied
#' @param R the number the game is repeated to get the distribution
#' @param sim NULL or the results of a previous call, results of the current call with be simply added to sim. This allows to add repetitions for getting better estimates.
#' @export
study.actions.and.states = function(strats, game, delta, T=100, R=1, sim=NULL, strat.par=NULL, verbose=interactive()) {
  restore.point("study.actions.and.states")
  
  was.storing = is.storing(); set.storing(FALSE);library(compiler); enableJIT(3)
  cat("\n")
  li = replicate(n=R,simplify=FALSE, {
    cat(".")
    run.rep.game(strat=strats,game=game, delta=delta, T.min=T, T.max = T, strat.par = strat.par)$hist                   
  }
  )
  enableJIT(0); set.storing(was.storing)
  cat("\n")
  
  res = rbindlist(li)
  if (!is.null(sim))
    res = rbind(sim,res)
  return(res)
}

examples.study.actions.and.states = function() {
  
  game = make.pd.game(err.D.prob=0.15)
  delta = 0.95
  sim = NULL
  
  set.storing(TRUE)
  sim = study.actions.and.states(strats=nlist(tit.for.tat,tit.for.tat),game=game, delta=delta, T=20, R = 10, sim=sim)

  # Show histogram
  ts = c(2,3,4,5,10,20)
  dat = sim[sim$t %in% ts,]
  qplot(a1,data=dat, geom="histogram", color=as.factor(t), fill=as.factor(t), facets = ~t, main="Distribution of actions by period")
  
}
