._RST = new.env()
._RST$seeds = list()
._RST$random.states = list()
._RST$current.state = ".GLOBAL"


#' Evalutes the expression expr with the specified random seed. Afterwards the function restores the original random seed.
#' 
#' This function can be useful in simulation studies in which some random variables, e.g. explanatory variables X shall always be drawn in the same fashion, while other data, like disturbances epsilon shall differ between runs. One can then draw the X using with.random.seed to guarantee the same X in every simulation run.
#' 
#' @param expr an R expression that returns some random value
#' @param seed an integer number that will be used to call set.seed
#' @return the value of expr evaluated under the specifed random seed
#' @export
with.random.seed = function(expr,seed=1234567890) {
  old.seed = get(".Random.seed",.GlobalEnv)
  set.seed(seed)
  ret = eval(expr)
  assign(".Random.seed",old.seed,.GlobalEnv)
  return(ret)
}

#' Gets the name of the current random state
current.random.state = function() {
  ._RST$current.state
}

#' Sets the random number state to a state specified by name
init.random.state = function(name=".GLOBAL", seed = NULL) {
  if (is.null(seed)) {
    seed = ceiling(runif(1,0,.Machine$integer.max))
  }
  set.random.state(name,seed)
} 

#' Draws n random seeds
draw.seed = function(n=1) {
  ceiling(runif(n,0,.Machine$integer.max))  
}

examples.set.random.state = function() {
  set.seed(12345)
  mean(.Random.seed)
  set.random.state()
  mean(.Random.seed)
  runif(1)  
}

#' Sets the random number state to a state specified by name
set.random.state = function(name=".GLOBAL", seed = NULL) {
  restore.point("set.random.state")
  
  #return()
  env = ._RST

  # Don't do anything if the random state has not changed
  if (env$current.state == name)
    return()
  
  
  # Store current random number internal state under the name current.state
  env$random.states[[env$current.state]] = get(".Random.seed",.GlobalEnv)
  
  env$current.state = name
  
  # Posssibly determine a new random seed
  if (is.null(env$random.states[[name]]) & is.null(seed)) {
    seed = draw.seed()
  }
  
  
  if (!is.null(seed)) {
    set.seed(seed)
    env$random.states[[name]]= get(".Random.seed",.GlobalEnv)
    env$seeds[[name]] = seed
  } else {
    assign(".Random.seed",env$random.states[[name]],.GlobalEnv)    
  }
} 

