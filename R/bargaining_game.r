# Some bargaining strategies

#' Markup5
#' 
#' An example strategy that adds a fixed markup of 5 above the own production cost
markup5 = function(obs,i,t,markup=5,...) {
  # Extract variables from obs 
  cost = obs$cost
  j = 3-i
  obs.xj = obs$x[j]
  obs.xi = obs$x[i]
  
  return(list(x=cost+markup, markup=markup))
}

#' Markup20
#' 
#' An example strategy that adds a fixed markup of 20 above the own production cost
markup20 = function(obs,i,t,markup=20,...) {
  # Extract variables from obs 
  cost = obs$cost
  j = 3-i
  obs.xj = obs$x[j]
  obs.xi = obs$x[i]
  
  return(list(x=cost+markup, markup=markup))
}


#' The code inside can be used to explore the behavior of strategies for the PD game
examples.bargaining.game = function() {
  #Load package
  library(StratTourn)
   
  
  # Generate a game object
  
  # 25 vs 10
  game = make.bargaining.game(cost.low=25, cost.high=55, uniform=FALSE, delta=0.9)

  # Pick a pair of strategies
  strat = nlist(markup5,markup20)
  # Let the strategies play against each other
  run.rep.game(game=game, strat = strat)
  
  
  getwd()
  # Set working directory in which data is stored
  setwd("D:/libraries/StratTourn/studies")

  # Init and run a tournament of several strategies against each other  
  strat = list(markup5,markup20)

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
#' 
#' Note: In a previous version there was the parameter "uniform". In this new version the same effect can be achieved by setting the upper boundary of cost.low to the same value as the lower.boundary of cost.high and setting the probability of prob.low and prob.high to 0.5 each.
#' @param cost.low The lower costs in the standard case. May be a single value or a vector specifiying the upper and lower bound of the lower costs (e.g. c(20,30))
#' @param cost.high The higher costs in the standard case. May be a single value or a vector specifiying the upper and lower bound of the higher costs (e.g. c(50,60))
#' @param prob.low The probability to draw the lower costs in the standard case. The probability of the higher case is thus 1-prob.low
#' @param digits To how many digits should the drawn costs be rounded?
#' @param redraw.costs If TRUE, then the costs are redrawn each round. In the standard case the costs are drawn once and are not changed from round to round
#' @param price Paid utility in case of an agreement
#' @param delta Probability of playing another round
make.bargaining.game = function(cost.low=20,cost.high=60,prob.low=0.5, digits=2, redraw.costs=FALSE, price=100, delta=0.8,...) {
  
  run.stage.game = function(a,t,t.obs,game.states,...) {
    restore.point("bargaining.run.stage.game")
    x = unlist(a, use.names=FALSE)
    costs = game.states$costs
    cost.type <- game.states$cost.type
    prob.high <- 1-prob.low
    
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
      cl <- c(min(cost.low),max(cost.low))
      cl.draw <- runif(n=2,min=cl[1],max=cl[2])
      ch <- c(min(cost.high),max(cost.high))
      ch.draw <- runif(n=2,min=ch[1],max=ch[2])
      
      #Draw for each player (reorder)
      c1.draw <- c(cl.draw[1], ch.draw[1])
      c2.draw <- c(cl.draw[2], ch.draw[2])
      
      #Determine who gets high/low costs
      hl.choice <- sample(c(1,2),2,replace=TRUE,prob=c(prob.low, 1-prob.low))
      costs = round(c(c1.draw[hl.choice[1]], c2.draw[hl.choice[2]]), digits=digits)
      cost.type <- c("low","high")[hl.choice]
    }

    # private signals: each player sees her cost type
    obs = list(list(cost=costs[1], x=x),
               list(cost=costs[2], x=x))
    round.stats = quick.df(t=c(t,t),i=1:2,cost=game.states$costs, type=game.states$cost.type,x=x,u=payoff) 
    return(list(payoff=payoff,obs=obs, round.stats=round.stats, game.states=list(costs=costs, redraw.costs=redraw.costs, cost.type=cost.type)))
  } 
  
  check.action = function(ai,i,t,...) {
    restore.point("check.action.bargaining.game")
    x = ai$x
    if (is.finite(x) & length(x)==1) {
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
    if(!is.finite(prob.low)||prob.low>1||prob.low<0){
      stop("Invalid prob.low. Has to be a number in the intervall [0,1]")
    }
    if(!all(is.finite(cost.low))||!(length(cost.low) %in% c(1,2))){
      stop("Invalid specification of cost.low. Has to be single numeric value or vector of two numeric values.")
    }
    if(!all(is.finite(cost.high))||!(length(cost.high) %in% c(1,2))){
      stop("Invalid specification of cost.high. Has to be single numeric value or vector of two numeric values.")
    }
    cl <- c(min(cost.low),max(cost.low))
    cl.draw <- runif(n=2,min=cl[1],max=cl[2])
    ch <- c(min(cost.high),max(cost.high))
    ch.draw <- runif(n=2,min=ch[1],max=ch[2])
    
    #Draw for each player (reorder)
    c1.draw <- c(cl.draw[1], ch.draw[1])
    c2.draw <- c(cl.draw[2], ch.draw[2])
    
    #Determine who gets high/low costs
    hl.choice <- sample(c(1,2),2,replace=TRUE,prob=c(prob.low, 1-prob.low))
    costs = round(c(c1.draw[hl.choice[1]], c2.draw[hl.choice[2]]), digits=digits)
    cost.type <- c("low","high")[hl.choice]
    nlist(costs, cost.type=cost.type)
  }
  
  nlist(run.stage.game, initial.game.states, check.action,example.action,example.obs, n=2, private.signals=TRUE, params = nlist(cost.low,cost.high,prob.low, redraw.costs), sym=TRUE, delta=delta, name="bargaining")
}


