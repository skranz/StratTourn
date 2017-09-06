
#' The code inside can be used to explore the behavior of strategies for the PD game
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
  strat = nlist(tit.for.tat, always.coop, random.action)  
  tourn = init.tournament(game=game, strat=strat)
  
  #set.storing(FALSE)  # uncoment to make code run faster
  tourn = run.tournament(tourn=tourn, R = 3)
  set.storing(TRUE)
  
  aptourn  = active.passive.tourn(astrat = nlist(always.coop), ptourn = tourn, game = game)

  atourn = aptourn$tourns[[1]]
  atourn = run.tournament(tourn=atourn,R=2)
  
  tourn
  save.tournament(tourn)
  # Analyse tournament in web browser
  show.tournament(tourn)
}



#' A strategy that always cooperates
#' 
#' A strategy for the iterated prisoners dilemma;
#' see make.pd.game
always.coop = function(obs,i,t,...) {
  return(list(a="C"))
}

#' A strategy that always defects
#' 
#' A strategy for the iterated prisoners dilemma;
#' see make.pd.game
always.defect = function(obs,i,t,...) {
  return(list(a="D"))
}

#' A strategy that randomly chooses an action
#' 
#' A strategy for the iterated prisoners dilemma;
#' see make.pd.game
random.action = function(obs,i,t,...) {
  a = sample( c("C","D"),  1)
  return(list(a=a))
}


#' The famous tit.for.tat strategy: winner of Axelrod's original tournament
#' 
#' A strategy for the iterated prisoners dilemma;
#' see make.pd.game
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

#' Strategy from the tutorial without much meaning
#' 
#' A strategy for the iterated prisoners dilemma;
#' see make.pd.game
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

#' Cooperates if cooperation makes sense in a naive way
#' 
#' A strategy for the iterated prisoners dilemma
#' It chooses the action with the highest sum value of the payoff matrix (i.e. always.defect in the case of the standard version);
#' see make.pd.game
coop.high.C = function(obs,i,t,...){
  restore.point("coop.high.C")
  
  #Receive payoff.matrix
  mat <- obs$pm 
  matC <- mat["C","C"] + mat["C","D"]
  matD <- mat["D","C"] + mat["D","D"]
  
  #Coop if Cooperation is better
  if(matC>matD){
    my.action <- "C"
  } else {
    my.action <- "D"
  }
  
  return(list(a=my.action))
}

#' Waits for Input of Human Player in Prisoners Dilemma Game
#' 
#' 
human.player.pd = function(obs,i,t,...){
  restore.point("human.player.pd")
  j=3-i
    
  if(t==1){
    cat("What do you want to do in your first round? Use \"C\", \"D\" or \"Stop\"")
  } else {
    other <- obs$a[j]
    me <- obs$a[i]
    cat(paste0("You: ",me,"\n","Other Strategy: ",other,"\n",collapse=" "))
    cat("What do you want to do?")
  }
  
  ok <- FALSE
  
  while(!ok){
    line <- readline()
    if(line[1]=="D"||line[1]=="C"||line[1]=="d"||line[1]=="c"||line[1]=="Stop"){
      if(line[1]=="D"||line[1]=="d"){
        my.action <- "D"
      } else if (line[1]=="C"||line[1]=="c"){
        my.action <- "C"
      } else {
        stop("Player stopped")
      }
      ok <- TRUE
    } else {
      cat("Only D or C are allowed. Please retry.")
    }
  }
  
  return(list(a=my.action))
}

#' Generate an iterated Prisoners Dilemma game
#' 
#' @param uCC Utility in the case 'both players cooperate'. If specified in form of a vector (e.g. c(0,2)), then a random value is uniformly drawn each round and presented to the strategies.
#' @param uCD Utility in the case 'I cooperate, but the other one doesn't'. If specified in form of a vector (e.g. c(0,2)), then a random value is uniformly drawn each round and presented to the strategies.
#' @param uDC Utility in the case 'I defect, but the other one cooperates'. If specified in form of a vector (e.g. c(0,2)), then a random value is uniformly drawn each round and presented to the strategies.
#' @param uDD Utility in the case 'both players defect'. If specified in form of a vector (e.g. c(0,2)), then a random value is uniformly drawn each round and presented to the strategies.
#' @param digits If the costs are drawn from a uniform distribution: To how many digits should the draw be rounded?
#' @param err.D.prob Probability that a cooperation is seen as a defection, i.e. the propability to erroneous see a D
#' @param err.C.prob Probability that a defection is seen as a cooperation, i.e. the propability to erroneous see a C
#' @param private.signals If TRUE, then every player always sees his own action as originaly intended. He can no longer see whether his action in the previous round was due to an error.
#' @param delta Probability of playing another round

make.pd.game = function(uCC=1,uCD=-1,uDC=2,uDD=0,digits=2,err.D.prob = 0, err.C.prob=0, private.signals=FALSE,delta=0.9,...) {
  
  run.stage.game = function(a,t,t.obs,game.states,...) {
    restore.point("pd.stage.game.fun")
    a = unlist(a, recursive=TRUE, use.name=FALSE)
    names(a) = paste0("a",1:2)
    
    # Payoffs
    mat = game.states$payoff.matrix
    payoff = c(mat[a[1],a[2]],mat[a[2],a[1]])
    
    rand = runif(1)
    if(a[1]=="D"){
      err.D.1 = FALSE
      err.C.1 = rand<err.C.prob
    } else {
      err.D.1 = rand<err.D.prob
      err.C.1 = FALSE
    }
    
    rand = runif(1)      
    if(a[2]=="D"){
      err.D.2 = FALSE
      err.C.2 = rand<err.C.prob
    } else {
      err.D.2 = rand<err.D.prob
      err.C.2 = FALSE
    }
    
    #ReDraw game.states
    game.states = initial.game.states()
    
    # Observation with noise
    if (private.signals) {
      obs1 = obs2 = a
      if (err.D.1) obs2[1] = "D"
      if (err.C.1) obs2[1] = "C"
      if (err.D.2) obs1[2] = "D"
      if (err.C.2) obs1[2] = "C"
      obs = list(list(a=obs1, pm=game.states$payoff.matrix),list(a=obs2,pm=game.states$payoff.matrix))
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
      obs = list(a=obs, pm=game.states$payoff.matrix)
    }
    round.stats = quick.df(t=c(t,t),i=1:2,u=payoff,a=a, obs.i=obs.i,obs.j=obs.j)
    if(err.D.prob>0){
      round.stats = data.frame(round.stats, err.D.i=c(err.D.1,err.D.2),err.D.j=c(err.D.2,err.D.1))
    }
    if(err.C.prob>0){
      round.stats = data.frame(round.stats, err.C.i=c(err.C.1,err.C.2),err.C.j=c(err.C.2,err.C.1))
    }
    if(game.states$rand.payoff){
      round.stats = data.frame(round.stats, uCC=mat[1,1], uCD=mat[1,2], uDC=mat[2,1], uDD=mat[2,2])
    }
    return(list(payoff=payoff,obs=obs, round.stats=round.stats, game.states=game.states))
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
  
  initial.game.states = function() {
    if(length(uCC)>1||length(uCD)>1||length(uDD)>1||length(uDC)>1){
      rand.payoff <- TRUE
    } else {
      rand.payoff <- FALSE
    }
    if(rand.payoff){
      lCC <- length(uCC)
      if(!(lCC %in% c(1,2))) stop("Wrong specification of uCC. Has to be single value or vector of 2")
      lCD <- length(uCD)
      if(!(lCD %in% c(1,2))) stop("Wrong specification of uCD. Has to be single value or vector of 2")
      lDC <- length(uDC)
      if(!(lDC %in% c(1,2))) stop("Wrong specification of uDC. Has to be single value or vector of 2")
      lDD <- length(uDD)
      if(!(lDD %in% c(1,2))) stop("Wrong specification of uDD. Has to be single value or vector of 2")
    }
    if(!rand.payoff){
      mat = rbind(c(uCC,uCD),
                  c(uDC,uDD))
    } else {
      mat = rbind(c(runif(1,uCC[1],uCC[lCC]),runif(1,uCD[1],uCD[lCD])),
                  c(runif(1,uDC[1],uDC[lDC]),runif(1,uDD[1],uDD[lDD])))
      mat = round(mat, digits=digits)
    }
    colnames(mat) = rownames(mat)=c("C","D")
    
    nlist(payoff.matrix=mat, rand.payoff=rand.payoff)  
  }
  
  example.action = function(i=1,t=1,game.states,...) {
    list(a="C")
  }
  example.obs = function(i=1,t=1,game.states,...) {
      list(a=c("C","C"),pm=game.states$payoff.matrix)
  }
  action.set = function(i=1, t=1,...) {
    list(a=c("C","D"))
  }
  
  nlist(run.stage.game, initial.game.states, check.action,example.action,example.obs, n=2, private.signals, params = nlist(uCC,uCD,uDC,uDD,err.D.prob, err.C.prob), sym=TRUE, delta=delta, name="Noisy PD")
}


