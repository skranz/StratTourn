
# The code inside can be used to explore the behavior of strategies for the PD game
examples.rps = function() {
  #Load package
  library(StratTourn)
  
  # Generate a game object
  game = make.rps.game(T=50)

  # Pick a pair of strategies
  strat = nlist(one.third,always.s)
  # Let the strategies play against each other
  run.rep.game(game=game, strat = strat)
  
  
  getwd()
  # Set working directory in which data is stored
  setwd("D:/libraries/StratTourn/studies")

  # Init and run a tournament of several strategies against each other  
  strat = nlist(one.third,always.s)
  tourn = init.tournament(game=game, strat=strat)
  
  set.storing(FALSE)  # uncoment to make code run faster
  tourn = run.tournament(tourn=tourn, R = 4)
  set.storing(TRUE)
  
  tourn
  save.tournament(tourn)
  # Analyse tournament in web browser
  show.tournament(tourn)
}

one.third = function(i,t,...) {
  rand = runif(1)
  if (rand < 1/3) {
    return(list(a="r"))
  } else if (rand < 2/3) {
    return(list(a="p"))
  } else {
    return(list(a="s"))    
  }
}

always.s = function(i,t,...) {
  return(list(a="s"))
}

#' Waits for Input of Human Player in Rock-Papers-Scissors
#' 
#' 
human.player.rps = function(obs,i,t,...){
  restore.point("human.player.rps")
  j=3-i
  
  if(t==1){
    cat("What do you want to do in your first round? Use \"r\", \"p\", \"s\" or \"Stop\"")
  } else {
    other <- obs[j]
    me <- obs[i]
    cat(paste0("You: ",me,"\n","Other Strategy: ",other,"\n",collapse=" "))
    cat("What do you want to do?")
  }
  
  ok <- FALSE
  
  while(!ok){
    line <- readline()
    if(line[1]=="R"||line[1]=="P"||line[1]=="S"||line[1]=="r"||line[1]=="p"||line[1]=="s"||line[1]=="Stop"){
      if(line[1]=="R"||line[1]=="r"){
        my.action <- "r"
      } else if (line[1]=="S"||line[1]=="s"){
        my.action <- "s"
      } else if (line[1]=="P"||line[1]=="p"){
        my.action <- "p"
      } else {
        stop("Player stopped")
      }
      ok <- TRUE
    } else {
      cat("Use \"r\", \"p\", \"s\" or \"Stop\" Please retry.")
    }
  }
  
  return(list(a=my.action))
}

#' Generate a rock papers scissors game
make.rps.game = function(delta=NULL,T=NULL,cost.r=0.75, cost.p=0, cost.s=0,...) {
  
  run.stage.game = function(a,t,t.obs,...) {
    restore.point("rps.stage.game.fun")
    a = unlist(a, recursive=TRUE, use.name=FALSE)
    
    costs = c(cost.r, cost.p, cost.s)
    mat = matrix(c(
        1, 0,  2,
        2,  1, 0,
        0,  2,  1
      ), nrow=3,byrow=TRUE)
    
    colnames(mat) = rownames(mat) =  c("r","p","s")
    mat = mat-costs
    
    u1 = mat[a[1],a[2]]
    u2 = mat[a[2],a[1]]
    payoff = c(u1,u2)
    obs = a
    round.stats = quick.df(t=c(t,t),i=1:2,u=payoff,a=a) 
    
    return(list(payoff=payoff,obs=obs, round.stats=round.stats))
  } 
  
  check.action = function(ai,i,t,...) {
    ai = ai$a
    if (is.character(ai) & length(ai)==1) {
      if (ai %in% c("r","p","s")) {
        return()
      }
    }
    #restore.point("check.action.pd")
    stop(paste0("player ",i, "'s strategy in period ",t, " returned an infeasible action: ", ai))
  }
  example.action = function(i=1,t=1,...) {
    list(a="r")
  }
  example.obs = function(i=1,t=1,...) {
    list(a=c("r","p"))
  }
  
  nlist(run.stage.game, check.action,example.action,example.obs, n=2, private.signals=FALSE, params =list(), sym=TRUE, delta=delta, T=T, name="RockPaperScissors")
}


