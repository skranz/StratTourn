
# The code inside can be used to explore the behavior of strategies for the PD game
examples.trade.game = function() {
  #Load package
  library(StratTourn)
  
  T = 10000
  x1 = runif(T, 0, 0.8)
  x2 = runif(T, 0, 0.8)
  hist(x1+x2)
  mean(x1+x2>1)
  
  mean(x2>0.5)
  
  
  
  # Generate a game object
  game = make.trade.game(cost.low=0, cost.high=80, uniform=TRUE, delta=0.8)
  
  game = make.trade.game(cost.low=20, cost.high=60, uniform=FALSE, delta=0.8)

  # Pick a pair of strategies
  strat = nlist(markup10,markup05)
  # Let the strategies play against each other
  run.rep.game(game=game, strat = strat)
  
  
  getwd()
  # Set working directory in which data is stored
  setwd("D:/libraries/StratTourn/studies")

  # Init and run a tournament of several strategies against each other  
  strat = nlist(markup10, markup05,demand50)  
  tourn = init.tournament(game=game, strat=strat)
  
  set.storing(FALSE)  # uncoment to make code run faster
  tourn = run.tournament(tourn=tourn, R = 100)
  set.storing(TRUE)
  
  tourn
  save.tournament(tourn)
  # Analyse tournament in web browser
  show.tournament(tourn)
}


trade.fixed.markup = function(obs,i,t,markup=0.1,...) {
  cost = obs$cost
  return(list(demand=cost+markup))
}

markup10 = function(obs,i,t,markup=10,...) {
  cost = obs$cost
  return(list(demand=cost+markup))
}

markup5 = function(obs,i,t,markup=5,...) {
  cost = obs$cost
  return(list(demand=cost+markup))
}


demand50 = function(obs,i,t,...) {
  cost = obs$cost
  return(list(demand=pmax(50,cost)))
}

trade.reduce = function(obs,i,t,start.markup=0.5,max.demand=0.7, ...) {
  return(list(demand=0.75))
}


#' Generate a (noisy) Prisoners' Dilemma game
make.trade.game = function(cost.low=20,cost.high=60,prob.low=0.5, prob.high=0.5, uniform=FALSE, digits=0, price=100, delta=0.8,...) {
  
  run.stage.game = function(a,t,t.obs,game.states,...) {
    restore.point("trade.run.stage.game")
    demands = unlist(a)
    costs = game.states$costs
    
    D = sum(demands)
    # Feasible demand 
    if (D<=price) {
      demands = demands + (price-D)/2
      payoff = demands-costs  
    } else {
      payoff = c(0,0)
    }
    
    # private signals: each player sees her cost type
    obs = list(list(cost=costs[1], demands=demands),
               list(cost=costs[2], demands=demands))
    round.stats = quick.df(t=c(t,t),i=1:2,cost=costs,demand=demands,u=payoff) 
    return(list(payoff=payoff,obs=obs, round.stats=round.stats, game.states=list(costs=costs)))
  } 
  
  check.action = function(ai,i,t,...) {
    demand = ai$demand
    if (is.numeric(demand) & length(demand)==1) {
      return()
    }
    stop(paste0("player ",i, "'s strategy in period ",t, " returned an infeasible action: ", ai))
  }
  example.action = function(i=1,t=1,...) {
    list(demand=0.5)
  }
  example.obs = function(i=1,t=1,game.states,...) {
    list(cost=game.states$costs[i], demands=c(0,0))
  }
  
  initial.game.states = function() {
    if (uniform) {
      costs = round(runif(2,cost.low, cost.high),digits)
    } else {
      costs = sample(c(cost.low,cost.high),2, replace=TRUE, prob=c(prob.low, prob.high))
    }
    nlist(costs)
  }
  
  nlist(run.stage.game, initial.game.states, check.action,example.action,example.obs, n=2, private.signals=TRUE, params = nlist(cost.min,cost.max), sym=TRUE, delta=delta, name="trade")
}


