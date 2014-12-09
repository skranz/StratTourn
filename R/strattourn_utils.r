apply.on.numeric = function(dat, fun, ..., vars=names(dat)) {
  for (var in vars) {
    if (is.numeric(dat[[var]])) {
      dat[[var]] = fun(dat[[var]],...)
    }  
  }
  dat
}


#' Displays the given text
#' @export
display = function (..., collapse = "\n", sep = "") 
{
    str = paste("\n", paste(..., collapse = collapse, sep = sep), 
        "\n", sep = "")
    invisible(cat(str))
}


examples.pos.in.seq.of.ones = function() {
  x = sample(c(0,0,1,1),20, replace=TRUE)
  rbind(x, pos.in.seq.of.ones(x))
  
  x = rd$other.D.prev[c(5:12,60:80)]
  dist = distance.to.previous.event(x)
  round(rbind(x,dist, 0.75^dist),3)
  
  
  pos = pos.in.seq.of.ones(1-x)
  (cumsum(x)>0)*pos
}

#' x is a vector of 0s and 1s (or FALSE and TRUE)
#' 
#' the function returns a vector of length(x) containing for each 1 the position within each sequence of 1s and a 0 for each 0.
pos.in.seq.of.ones <- function(x){
  tmp<-cumsum(x)
  tmp-cummax((!x)*tmp)
}

#' x is a vector of 0s and 1s (or FALSE and TRUE)
#' 
#' the function returns a vector of length(x) containing for each index i the distance to the previous entry of a 1, if x[i]=1 we return 0. If the vector starts with 0's, we set the distance for these 0 entries to Inf.
distance.to.previous.event = function(x) {
  pos = pos.in.seq.of.ones(1-x)
  pos / (cummax(x)>0)
  #(cumsum(x)>0)*pos
}

log_breaks_fun <- function(n = 10){
  function(x) {
      axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}

get.smoothed.vals = function(d,xout=d[[xvar]], xvar="num.err",yvar="u", wvar="num.obs",...) {
  restore.point("get.smoothed.vals")
  res.li = list()
  res.li[[xvar]] = xout
  
  for (yv in yvar) {
    if (!is.null(wvar)) {
      sp = smooth.spline(x=d[[xvar]], y=d[[yv]], w= d[[wvar]],...)
    } else {
      sp = smooth.spline(x=d[[xvar]], y=d[[yv]],...)      
    }
    res.li[[yv]] = predict(sp,x = xout)$y
  }
  df = do.call(data.frame,res.li)
  df
}

deparse.strat = function(strat, name = NULL) {
  str = capture.output(strat)
  last.line = which(str.trim(str)=="}")
  if (length(last.line)>0)  {
    last.line = last.line[length(last.line)]
    str = str[1:last.line]
  }
  str = merge.lines(str)
  if (!is.null(name)) {
    str = paste0(name, " = ", str)
  }
  str
}

