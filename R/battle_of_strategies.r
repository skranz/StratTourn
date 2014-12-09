# Make sure that data.table awareness works!
.onLoad = function(...)  {
  assignInNamespace("cedta.override", union(data.table:::cedta.override,c("StratTourn","dplyr")), "data.table")
}


my.cedta = function (n = 2L) {
    te = topenv(parent.frame(n))
    restore.point("my.cedta")
    if (!isNamespace(te)) 
        return(TRUE)
    nsname = getNamespaceName(te)
    ans = nsname == "data.table" ||
      "data.table" %chin% names(getNamespaceImports(te)) || 
      "data.table" %chin% tryCatch(unlist(getNamespaceImports(te)), error=function() NULL)
    
    ans
}



wtd.var = function (x, weights = NULL, normwt = FALSE, na.rm = TRUE) 
{
    if (!length(weights)) {
        if (na.rm) 
            x <- x[!is.na(x)]
        return(var(x))
    }
    if (na.rm) {
        s <- !is.na(x + weights)
        x <- x[s]
        weights <- weights[s]
    }
    if (normwt) 
        weights <- weights * length(x)/sum(weights)
    xbar <- sum(weights * x)/sum(weights)
    sum(weights * ((x - xbar)^2))/(sum(weights) - 1)
}

wtd.mean = function (x, weights = NULL, normwt = "ignored", na.rm = TRUE) 
{
    if (!length(weights)) 
        return(mean(x, na.rm = na.rm))
    if (na.rm) {
        s <- !is.na(x + weights)
        x <- x[s]
        weights <- weights[s]
    }
    sum(weights * x)/sum(weights)
}

wtd.se <- function(x, w, na.rm=FALSE)
#  Computes the variance of a weighted mean following Cochran 1977 definition
{
  if (na.rm) { w <- w[i <- !is.na(x)]; x <- x[i] }
  n = length(w)
  xWbar = weighted.mean(x,w,na.rm=na.rm)
  wbar = mean(w)
  out = n/((n-1)*sum(w)^2)*(sum((w*x-wbar*xWbar)^2)-2*xWbar*sum((w-wbar)*(w*x-wbar*xWbar))+xWbar^2*sum((w-wbar)^2))
  return(sqrt(out))
}


# Try to get a named field from an object x. If x does not have that field, return NULL  
get.field = function(x, field.name, type=NULL) {
  if (!(field.name %in% names(x))) {
    return(NULL)
  }
  val = x[[field.name]]
  if (!is.null(type)) {
    if (is(val,type)) {
      return(val)
    } else {
      return(NULL)
    }
  }
  return(val)
}


# Try to get a named field from an object x. If x does not have that field, return NULL  
has.field = function(x, field.name, type=NULL, value=NULL, length=NULL) {
  if (!(field.name %in% names(x))) {
    return(FALSE)
  }
  val = x[[field.name]]
  if (!is.null(type)) {
    if (!is(val,type)) {
      return(FALSE)
    }
  }
  if (!is.null(value)) {
    if (!all(value=val)) {
      return(FALSE)
    }
  }
  if (!is.null(length)) {
    if (!length(val)==length) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}


example.get.field = function() {
  get.field("gdh","b")
  
  get.field(c(b="Hi"),"b")
  
}

is.true = function(val) {
  if (is.null(val))
    return(FALSE)
  val[is.na(val)] = FALSE
  return(val)
}


is.false = function(val) {
  if (is.null(val))
    return(FALSE)
  val[is.na(val)] = TRUE  
  return(!val)
}

df.signif = function(df,digits=3,fun=signif) {
  #restore.point("df.signif")
  for (i in 1:NCOL(df)) {
    if (is.numeric(df[[i]])) {
      df[[i]] = fun(df[[i]],digits) 
    }
  }
  df
}

#' store objects when called from a function
debug.store = function(strat,i,t, stage = NULL, do.store = TRUE) {
  if (!do.store) return()
  #message("Store...", do.store)
  store = !identical(.GlobalEnv,sys.frame(-1))  
  if (!store) return()
  if (is.null(stage)) {
    name = paste0(strat,"_i=",i,"_t=",t)
  } else {
    name = paste0(strat,"_i=",i,"_t=",t,"_stage=",stage)    
  }
  store.objects(name,parent.num=-2, deep.copy=FALSE, store.parent.env=FALSE)
}

#' restore objects if and only if called from the global environment
debug.restore = function(strat,i,t, stage = NULL) {
  restore = identical(.GlobalEnv,sys.frame(-1))  
  if (!restore)
    return()
  if (is.null(stage)) {
    name = paste0(strat,"_i=",i,"_t=",t)
  } else {
    name = paste0(strat,"_i=",i,"_t=",t,"_stage=",stage)    
  }
  restore.objects(name,deep.copy=FALSE)
}

gbos = new.env()

#' Runs the string code on a data table
run.dt = function(dt,code,rows=NULL,by=NULL, with=TRUE, return.all = FALSE) {
  restore.point("run.dt")
  
  
  if (!is.null(by)) {
    by.str = paste0(",by=c(", paste0('"',by,'"',collapse=","),")")
  } else {
    by.str = ""
  }
  for (act.code in code) {
    com = paste0(
      "dt[",rows,",",act.code,by.str,",with=",with,"]"
    )
    dt = eval(parse(text=com))
  }
  dt
}

to.length = function(vec,len,fill=NA) {
  restore.point("to.length")
  if (len <= 0)
    return(vector("any",0))
  # Transform lists to vectors
  if (is.list(vec))
    vec = unlist(vec)
  if (length(vec)==len) {
    return(vec)
  }
  if (length(vec)>len) {
    vec[len] = paste0(vec[len:length(vec)], collapse=" ")
    return(vec[1:len])
  }
  if (length(vec)<len) {
    return(c(vec,rep(fill,len-length(vec))))
  }
  
  
}

str.combine = function(..., sep="", collapse=NULL) {
  str = list(...)
  restore.point("str.combine")
  len = sapply(str,length)
  if (any(len==0))
    return(vector("character",0))
  paste0(...,sep=sep,collapse=collapse)
}

#' Loads strategies from a R file and returns a list of strategies 
load.strategies = function(file) {
  env = new.env(parent=globalenv())
  source(file,local=env)
  as.list(env)
}

#' Loads a game maker
#' A game maker is a function that can generate a game
load.game.maker = function(file) {
  env = new.env(parent=globalenv())
  source(file,local=env)
  game.maker = as.list(env)[[1]]
  game.maker
}

my.data.frame = function(..., ignore.empty=TRUE) {
  args = list(...)
  if (ignore.empty)
    args = args[sapply(args,length)>0]
  do.call(data.frame,args)
}



to.named.list=function(val, names) {
  stop("Not yet implemented")
  if (length(val) != length(names))
    stop("val and names have different length")
  li = vector("list")
}
