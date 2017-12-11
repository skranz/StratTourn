
# The code inside can be used to explore the behavior of strategies for the research agreement game
examples.hotelling.agreement.game = function() {
  #Load package
  library(StratTourn)
   
  
  # Generate a game object
  game = make.hotelling.game(lower.bound=0, upper.bound=1, s=1, t=1, delta=0.9)
  
  # Pick a pair of strategies
  strat = nlist(traveling.salesman,the.undercutter)
  # Let the strategies play against each other
  run.rep.game(game=game, strat = strat)
  
  
  getwd()
  # Set working directory in which data is stored
  #setwd("D:/libraries/StratTourn/studies")
  setwd("E:/!Data/!Daten/Work/Promotion/L - Lectures/Kooperation Spieltheorie/WS 2015-16/StratTourn/studies")
  
  strat = nlist(traveling.salesman,the.undercutter, traveling.salesman.1) # add your own strategies here  
  tourn = init.tournament(game=game, strat=strat) #initialise tournament
  
  set.storing(FALSE)  # makes code run faster but disables debugging
  tourn = run.tournament(tourn=tourn, R = 100)
  set.storing(TRUE)
  
  tourn
  save.tournament(tourn) # uncomment if you want to save your tournament
  # Analyse tournament in web browser (needs internet connection)
  show.tournament(tourn)
}

#'Example strategy (Hotelling Game): traveling.salesman
#'
#' A strategy which "walks" through over the "beach" with a fixed price
#'
#' @param obs Observations of position and price of both players in the last round as specified in the function hotelling.profits
#' @param i Number of current player
#' @param t Current period
#' @param price Fixed price of the strategy
#' @param speed The strategy starts at 0 and moves each period by speed, afterwards it starts at the beginning. 
traveling.salesman = function(obs,i,t,price=0.5,speed=0.05,...) {
  debug.store("traveling.salesman",i,t)       # Store each call for each player
  debug.restore("traveling.salesman",i=1,t=2) # Restore call for player i in period t
  
  # Extract variables from obs 
  j = 3-i #Whats the number of the other player?
  obs.pj = obs$a[[j]]$p #What was the price of the other firm last round?
  obs.pi = obs$a[[i]]$p #What was the price of my firm last round?
  obs.lj = obs$a[[j]]$l #What was the location of the other firm last round?
  obs.li = obs$a[[i]]$l #What was the location of my firm last round?
  
  #start in the first round
  if(t==1){
    action = list(p=price,l=0)
    return(list(a=action, price=price, speed=speed))
  }
  
  #after the first round
  location = obs.li + speed
  if(location>1){ #we don't want to travel into the wasteland
    location = location - 1
  }
  action=list(p=price,l=location)
  
  return(list(a=action, price=price, speed=speed))
}

#'Example strategy (Hotelling Game): the.undercutter
#'
#' This strategy copies the location of the opponent and sets the price as "price of opponent - cut"
#'
#' @param obs Observations of position and price of both players in the last round as specified in the function hotelling.profits
#' @param i Number of current player
#' @param t Current period
#' @param cut How much lower than the opponent should the price be set?
the.undercutter = function(obs,i,t,cut=0.01,...) {
  debug.store("the.undercutter",i,t)       # Store each call for each player
  debug.restore("the.undercutter",i=1,t=3) # Restore call for player i in period t
  
  # Extract variables from obs 
  j = 3-i #Whats the number of the other player?
  obs.pj = obs$a[[j]]$p #What was the price of the other firm last round?
  obs.pi = obs$a[[i]]$p #What was the price of my firm last round?
  obs.lj = obs$a[[j]]$l #What was the location of the other firm last round?
  obs.li = obs$a[[i]]$l #What was the location of my firm last round?
  
  #start in the first round
  if(t==1){
    action = list(p=0.5,l=0.5)
    return(list(a=action, cut=cut))
  }
  
  #after the first round
  location = obs.lj
  price = max(obs.pj-cut,0)
  
  action=list(p=price,l=location)
  
  return(list(a=action, cut=cut))
}

#' Waits for Input of Human Player in Hotelling Game
#' 
#' 
human.player.ht = function(obs,i,t,...){
  restore.point("human.player.ht")
  
  # Extract variables from obs 
  j = 3-i #Whats the number of the other player?
  obs.pj = obs$a[[j]]$p #What was the price of the other firm last round?
  obs.pi = obs$a[[i]]$p #What was the price of my firm last round?
  obs.lj = obs$a[[j]]$l #What was the location of the other firm last round?
  obs.li = obs$a[[i]]$l #What was the location of my firm last round?
  
  if(t!=1){
    profits <- hotelling.profits(lower.bound=game$params$lower.bound, upper.bound = game$params$upper.bound, s=game$params$s, t=game$params$t.distance, choice1=list(p=obs.pi, l=obs.li), choice2=list(p=obs.pj, l=obs.lj))
    cat(paste0("Your location: ",obs.li," Your price: ",obs.pi," Your profit: ",round(profits$choice1$pi,3), "\n","Other location: ",obs.lj," Other price: ",obs.pj," Other profit: ",round(profits$choice2$pi,3),"\n",collapse=" "))
  } 
  cat("Which location do you choose? Write a number or \"Stop\"")
  
  ok <- FALSE
  while(!ok){
    line <- readline()
    if(line[1]=="Stop"){
      stop("Player stopped")
    } else if(!is.na(as.numeric(line[1]))){
      location = as.numeric(line[1])
      ok <- TRUE
    } else {
      cat("Not a valid number. Please retry.")
    }
  }
  
  cat("Which price do you choose? Write a number or \"Stop\"")
  
  ok <- FALSE
  while(!ok){
    line <- readline()
    if(line[1]=="Stop"){
      stop("Player stopped")
    } else if(!is.na(as.numeric(line[1]))){
      price = as.numeric(line[1])
      ok <- TRUE
    } else {
      cat("Not a valid number. Please retry.")
    }
  }
  
  action=list(p=price,l=location)
  
  return(list(a=action))
}


#' Calculates the profits of a hotelling game
#'
#' We consider a hotelling game where two firms set price and location at the same time. The Utility Function of the uniformly distributed customers is 
#' U = s - t.distance*|x-l|-p.
#' 
#' @param lower.bound Customers are uniformly distributed in the intervall [lower.bound, upper.bound]
#' @param upper.bound Customers are uniformly distributed in the intervall [lower.bound, upper.bound]
#' @param s Prohibitive price of customer
#' @param t Factor determining how much the customer loathes distance
#' @param print.details When TRUE the function prints various details and intermediate calculations.
#' @param choice1 choice of first company. The choice has to have the structure list(p=<numerical price>, l=<numerical location>).
#' @param choice2 choice of first company. The choice has to have the structure list(p=<numerical price>, l=<numerical location>). Note that in contrast to some standard notations of the hotelling game the location choice of firm2 is given in absolute values and neither starting from the lower bound nor from the upper bound.
hotelling.profits <- function(lower.bound=0, upper.bound=1, s=1, t=1, print.details=FALSE, choice1, choice2){
  restore.point("hotelling.profits")
  
  if(lower.bound>=upper.bound){
    stop("lower.bound has to be less than upper.bound")
  }
  
  #Transform variables to better fit formulas
  #Now l1/p1 depict the lower location
  if(choice1$l<=choice2$l){
    l1 <- choice1$l
    p1 <- choice1$p
    l2 <- choice2$l
    p2 <- choice2$p
    is.lower <- 1
  } else {
    l1 <- choice2$l
    p1 <- choice2$p
    l2 <- choice1$l
    p2 <- choice1$p
    is.lower <- 2
  }
  
  #Calculate cut off points
  x.lower1 <- l1 + (p1-s)/t
  x.lower2 <- l2 + (p2-s)/t
  x.upper1 <- l1 + (s-p1)/t
  x.upper2 <- l2 + (s-p2)/t
  x.indifferent <- (p2-p1)/(2*t) + 1/2 * (l1+l2)
  ## x.lower may never be left of lower.bound but should not be higher as upper.bound
  x.lower1 <- min(max(x.lower1,lower.bound), upper.bound)
  x.lower2 <- min(max(x.lower2,lower.bound), upper.bound)
  x.upper1 <- max(min(x.upper1, upper.bound), lower.bound)
  x.upper2 <- max(min(x.upper2, upper.bound), lower.bound)
  x.indifferent <- max(min(x.indifferent, upper.bound), lower.bound)
  
  #calculate part of interval which is relevant for profit
  if(l1<= x.indifferent && x.indifferent<=l2 && l1!=l2){ 
    #standard case -> x.indifferent lies between l1 & l2
    area1 <- min(x.indifferent,x.upper1)-x.lower1
    area2 <- x.upper2 - max(x.indifferent,x.lower2)
  } else if (l1 == x.indifferent && l2 == x.indifferent){ 
    #both are exactly identical (this may only happen if prices are equal too)
    area1 <- 1/2 * (x.upper1 - x.lower1)
    area2 <- area1
  } else if (x.indifferent < l1){ 
    #left case -> all customers go to l2 [if a company can't hold customers, which are exactly on them, then they do not get any]
    area1 <- 0
    area2 <- x.upper2 - x.lower2
  } else if (x.indifferent > l2){ 
    #right case -> all customers go to l1 [if a company can't hold customers, which are exactly on them, then they do not get any]
    area1 <- x.upper1 - x.lower1
    area2 <- 0
  } else {
    #this should never happen
    stop("bad case differentiation in hotelling.profits when calculating areas.\n")
  }
  
  if(print.details){
    p.vec <- c()
    p.vec[1] <- paste0("area1",sep=": ",area1)
    p.vec[length(p.vec)+1] <- paste0("area2",sep=": ",area2)
    p.vec[length(p.vec)+1] <- paste0("x.lower1",sep=": ",x.lower1)
    p.vec[length(p.vec)+1] <- paste0("x.upper1",sep=": ",x.upper1)
    p.vec[length(p.vec)+1] <- paste0("x.lower2",sep=": ",x.lower2)
    p.vec[length(p.vec)+1] <- paste0("x.upper2",sep=": ",x.upper2)
    p.vec[length(p.vec)+1] <- paste0("x.indifferent",sep=": ",x.indifferent)
    p.vec[length(p.vec)+1] <- "\n" #last should be a blank line
    p.all <- paste0(p.vec,collapse = "\n")
    cat(p.all)
  }
  
  #profits
  pi1 <- area1 * p1
  pi2 <- area2 * p2
  
  #results with updated choices
  if(is.lower==1){
    choice1$pi <- pi1
    choice2$pi <- pi2
  } else {
    choice1$pi <- pi2
    choice2$pi <- pi1
  }
  res <- list(choice1=choice1, choice2=choice2)
  
  return(res)
}

#' Generate a Hotelling Game
#'
#' We consider two firms playing a repeated hotelling game, setting price and location. The Utility Function of the uniformly distributed customers is 
#' U = s - t.distance*|x-l|-p.
#' 
#' @param lower.bound Customers are uniformly distributed in the intervall [lower.bound, upper.bound]
#' @param upper.bound Customers are uniformly distributed in the intervall [lower.bound, upper.bound]
#' @param s Prohibitive price of customer
#' @param t.distance Factor determining how much the customer loathes distance
#' @param fix.location If TRUE, then the location can only be set once
#' @param delta Probability of playing another round
make.hotelling.game = function(lower.bound=0, upper.bound=1, s=1, t.distance=1, fix.location=FALSE, delta = 0.9, ...) {
  restore.point("make.hotelling.game")
  
  run.stage.game = function(a,t,t.obs,game.states,...) {
    restore.point("hotelling.game.run.stage.game")
    
    #Take and transform shares to allowed values
    choice1 = a[[1]][["a"]]
    choice2 = a[[2]][["a"]]
    
    #Check for theoretically possible situations, were a warning should be displayed.
    if(choice1$p<0){
        warning(paste0("Strategy of player 1 in period ",t, " returned this p: ", round(choice1$p,3),". p is set to 0."))
        choice1$p <- 0
    }
    if(choice2$p<0){
      warning(paste0("Strategy of player 2 in period ",t, " returned this p: ", round(choice2$p,3),". p is set to 0."))
      choice2$p <- 0
    }
    
    #first round determines fixed locations if TRUE
    if(fix.location){
      if(t==1){
        game.states$fixed.locations = c(choice1$l, choice2$l)
      } else {
        choice1$l = game.states$fixed.locations[1]
        choice2$l = game.states$fixed.locations[2]
      }
    }
    
    #Calculation of payoff
    calc <- hotelling.profits(lower.bound=lower.bound, upper.bound=upper.bound, s=s, t=t.distance, print.details=FALSE, choice1=choice1, choice2=choice2)
    
    payoff <- sapply(1:2,FUN=function(i){
      return(calc[[i]]$pi)
    })
    
    obs = list(a=list(list(p=calc[[1]]$p, l=calc[[1]]$l),
               list(p=calc[[2]]$p, l=calc[[2]]$l)))
    round.stats = quick.df(t=c(t,t),i=1:2,p=c(calc[[1]]$p,calc[[2]]$p), l=c(calc[[1]]$l,calc[[2]]$l), u=payoff) 
    return(list(payoff=payoff,obs=obs, round.stats=round.stats, game.states=game.states))
  } 
  
  check.action = function(ai,i,t,...) {
    restore.point("check.action.hotelling")
    a = ai$a
    is.error = FALSE
    eps <- 10e-5
    
    #General structure
    if(length(a)!=2){
      is.error = TRUE
    } else if(any(!(names(a) %in% c("p","l")))){
      is.error = TRUE
    }
    if(is.error){
      stop(paste0("player ",i, "'s strategy in period ",t, " returned an infeasible action: ", ai))
    }
    #Structure of p
    if (!(!is.null(a$p) && is.finite(a$p) && length(a$p)==1)) {
      stop(paste0("player ",i, "'s strategy in period ",t, " returned an infeasible p: ", a$p))
    }
    #validity of p
    #if(a$p+eps<0 || a$p-eps>s){
    #  stop(paste0("player ",i, "'s strategy in period ",t, " returned an infeasible p: ", a$p,". p is either higher than s or lower than 0."))
    #}
    #Structure of l
    if (!(!is.null(a$p) && is.finite(a$l) && length(a$l)==1)) {
      stop(paste0("player ",i, "'s strategy in period ",t, " returned an infeasible l: ", a$p))
    }
    #validity of l
    #if(a$l+eps<lower.bound || a$l-eps>upper.bound){
    #  stop(paste0("player ",i, "'s strategy in period ",t, " returned an out of bound location: ", a$l))
    #}
    return()
  }
  example.action = function(i=1,t=1,...) {
    res <- list(p=0.5, l=0.5)
    list(a=res)
  }
  example.obs = function(i=1,t=1,game.states,...) {
    list(p=c(0.5, 0.5), l=c(0.5,0.5))
  }
  
  initial.game.states = function() {
    #There are no initial game states
    fixed.locations = c(NA,NA)
    nlist(fixed.locations)
  }
  
  nlist(run.stage.game, initial.game.states, check.action,example.action,example.obs, n=2, private.signals=FALSE, params = nlist(lower.bound, upper.bound, s, t.distance, fix.location), sym=TRUE, delta=delta, name="hotelling")
}


