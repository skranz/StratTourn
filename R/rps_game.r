
# The code inside can be used to explore the behavior of strategies for the PD game
examples.rps = function() {
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



#' Generate a rock papers scissors game
make.rps.game = function(delta=0.9,...) {
  
  run.stage.game = function(a,t,t.obs,...) {
    restore.point("pd.stage.game.fun")
    a = unlist(a, recursive=TRUE, use.name=FALSE)
    if (a[1]==a[2])
      
    mat = matrix(c(
        0, -1,  1,
        1,  0, -1,
       -1,  1,  0
      ), nrow=3,byrow=TRUE)
    
    colnames(mat) = rownames(mat) =  c("r","p","s")
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
  
  nlist(run.stage.game, check.action,example.action,example.obs, n=2, private.signals, params =list(), sym=TRUE, delta=delta, name="Rock-Paper-Scissors")
}


