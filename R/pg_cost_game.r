#' Generate a (noisy) Prisoners' Dilemma game
make.pg_cost.game = function(cost.min=0.3,cost.max=1.3,new.cost.prob = 0.5, digits=0, delta=0.95,...) {
  
  run.stage.game = function(a,t,t.obs,game.states,...) {
    restore.point("pg_cost.run.stage.game")

    x = unlist(a, use.names=FALSE)
    x = pmin(x,1)
    x = pmax(x,0)
    x = round(x)

    costs = game.states$costs
    payoff = sum(x) / 2 - costs*x
    old.costs = costs
    
    # draw new costs with probability new.cost.prob
    if (runif(1)<new.cost.prob)
      costs[1] = runif(1,cost.min,cost.max)
    if (runif(1)<new.cost.prob)
      costs[2] = runif(1,cost.min,cost.max)
    costs = round(costs,2)
    
    
    # private signals: each player sees her cost type
    obs = list(list(cost=costs[1], x=x),
               list(cost=costs[2], x=x))
    round.stats = quick.df(t=c(t,t),i=1:2,cost=old.costs,x=x,u=payoff) 
    return(list(payoff=payoff,obs=obs, round.stats=round.stats, game.states=list(costs=costs)))
  } 
  
  check.action = function(ai,i,t,...) {
    x = ai$x
    if (identical(x,0) | identical(x,1)) {
      return()
    }
    stop(paste0("player ",i, "'s strategy in period ",t, " returned an infeasible action: ", ai))
  }
  example.action = function(i=1,t=1,...) {
    list(x=1)
  }
  example.obs = function(i=1,t=1,game.states,...) {
    list(cost=game.states$costs[i], x=c(0,0))
  }
  
  initial.game.states = function() {
    costs = round(runif(2,cost.min,cost.max),2)
    nlist(costs)
  }
  
  nlist(run.stage.game, initial.game.states, check.action,example.action,example.obs, n=2, private.signals=TRUE, params = nlist(cost.min,cost.max,new.cost.prob = 0.5), sym=TRUE, delta=delta, name="pg_cost")
}


