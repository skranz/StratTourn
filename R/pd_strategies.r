
always.coop = function(...) {
  return("C")
}

always.defect = function(...) {
  return("D")
}
 
tit.for.tat = function(obs,i,t,game,...) {
   # Start nice
   if (t==1)
     return("C")
   # Return the other players previous (observed) action
   j = 3-i
   obs[j]
}

grim.trigger = function(obs,i,t,game,coop,...) {
  restore.point("grim.tigger")
  if (t==1)
    return(nlist("C",coop=TRUE))
  
  # Cooperate if and only if everybody so far has cooperated
  if (coop & obs[1]=="C" & obs[2]=="C") {
    return(nlist("C",coop))
  } else {
    return(nlist("D",coop=FALSE))
  }
}



my.strategy = function(obs,i,t, game, net.nice=0, ...) {  
  restore.point("count.tit.for.tat")
  if (t==1) {
    return(nlist("C",net.nice))
  }
  
  a = obs
  j = 3-i  
  a.num = ifelse(a=="C",1,0)
  net.nice = net.nice + a.num[i]-a.num[j]
  if (game$private.signals)
    net.nice = net.nice-game$params$obs.D.prob
  
  if (net.nice <= 1 | runif(1)<(1/(net.nice^2))*4) {
    return(nlist("C",net.nice))
  }
  return(nlist("D",net.nice))
}

