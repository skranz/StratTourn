
# The code inside can be used to explore the behavior of strategies for the PD game
examples.bargaining.game = function() {
  #Load package
  library(StratTourn)
   
  
  # Generate a game object
  
  # 25 vs 10
  game = make.bargaining.game(cost.low=25, cost.high=55, uniform=FALSE, delta=0.9)

  # Pick a pair of strategies
  strat = nlist(waiter2,x50)
  # Let the strategies play against each other
  run.rep.game(game=game, strat = strat)
  
  
  getwd()
  # Set working directory in which data is stored
  setwd("D:/libraries/StratTourn/studies")

  # Init and run a tournament of several strategies against each other  
  strat = nlist(markup10, x50, waiter1, waiter2, waiter3)  

  strat = nlist(x50,waiter1, waiter2, waiter3, markup15_5)  
  tourn = init.tournament(game=game, strat=strat)
  
  set.storing(FALSE)  # uncoment to make code run faster
  tourn = run.tournament(tourn=tourn, R = 30)
  set.storing(TRUE)
  
  tourn
  save.tournament(tourn)
  # Analyse tournament in web browser
  show.tournament(tourn)
}


#' Generate a bargaining game
#' @param cost.low The lower costs in the standard case and the lower bound of costs in the uniform case
#' @param cost.high The higher costs in the standard case and the upper bound of costs in the uniform case
#' @param prob.low The probability to draw the lower costs in the standard case
#' @param prob.high The probability to draw the higher costs in the standard case
#' @param uniform If TRUE, then the costs are drawn from a uniform distribution between cost.low and cost.high instead of the two discrete choices cost.low and cost.high
#' @param digits If the costs are drawn from a uniform distribution: To how many digits should the draw be rounded?
#' @param redraw.costs If TRUE, then the costs are redrawn each round. In the standard case the costs are drawn once and are not changed from round to round
#' @param price Paid utility in case of an agreement
#' @param delta Probability of playing another round
make.bargaining.game = function(cost.low=20,cost.high=60,prob.low=0.5, prob.high=0.5, uniform=FALSE, digits=0, redraw.costs=FALSE, price=100, delta=0.8,...) {
  
  run.stage.game = function(a,t,t.obs,game.states,...) {
    restore.point("bargaining.run.stage.game")
    x = unlist(a, use.names=FALSE)
    costs = game.states$costs
    
    D = sum(x)
    # Feasible x 
    if (D<=price) {
      payoff = x + (price-D)/2 -costs  
      #x = x + (price-D)/2
      #payoff = x  -costs  
    } else {
      payoff = c(0,0)
    }
    
    #Draw new costs if necessary
    if(redraw.costs){
      if(uniform){
        costs = round(runif(2,cost.low, cost.high),digits)
      } else {
        costs = sample(c(cost.low,cost.high),2, replace=TRUE, prob=c(prob.low, prob.high))
      }
    }

    # private signals: each player sees her cost type
    obs = list(list(cost=costs[1], x=x),
               list(cost=costs[2], x=x))
    round.stats = quick.df(t=c(t,t),i=1:2,cost=game.states$costs,x=x,u=payoff) 
    return(list(payoff=payoff,obs=obs, round.stats=round.stats, game.states=list(costs=costs, redraw.costs=redraw.costs)))
  } 
  
  check.action = function(ai,i,t,...) {
    x = ai$x
    if (is.numeric(x) & length(x)==1) {
      return()
    }
    stop(paste0("player ",i, "'s strategy in period ",t, " returned an infeasible action: ", ai))
  }
  example.action = function(i=1,t=1,...) {
    list(x=0.5)
  }
  example.obs = function(i=1,t=1,game.states,...) {
    list(cost=game.states$costs[i], x=c(0,0))
  }
  
  initial.game.states = function() {
    if (uniform) {
      costs = round(runif(2,cost.low, cost.high),digits)
    } else {
      costs = sample(c(cost.low,cost.high),2, replace=TRUE, prob=c(prob.low, prob.high))
    }
    nlist(costs)
  }
  
  nlist(run.stage.game, initial.game.states, check.action,example.action,example.obs, n=2, private.signals=TRUE, params = nlist(cost.low,cost.high,prob.low, prob.high, uniform, redraw.costs), sym=TRUE, delta=delta, name="bargaining")
}


