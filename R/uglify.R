examples.uglify.strat = function() {
  ustrat = uglify.strat(tit.for.tat)
  ustrat = uglify.strat(strange.defector)

  strat(obs = list(a=c("C","C")),t=2, i=1)
  ustrat(obs = list(a=c("C","C")),t=2, i=1)
  
  # Generate a game object
  game = make.pd.game(err.D.prob=0.15)
  
  
  # Pick a pair of strategies
  strats = nlist(ustrat,ustrat)

  # Let the strategies play against each other
  set.storing(TRUE)
  run.rep.game(delta=0.7, game=game, strat = strats, detailed.return=!FALSE)

}

#' Makes a strategy (function) hard to read
#' 
#' Purpose: This way we can give strategies to students
#' who can test their strategies against them. While in principle
#' one can write some code to make the functions easier readable
#' again, this is not straightforward. The effort seems fairly 
#' large. If students do that 
uglify.strat = function(strat,keep.vars=names(formals(strat)),keep.funs=NULL,...) {
  uglify.function(strat, keep.vars=keep.vars,keep.funs=keep.funs,...)
}

#' Makes a function hard to read
#' 
#' Purpose: This way we can give strategies to students
#' who can test their strategies against them. While in principle
#' one can write some code to make the functions easier readable
#' again, this is not straightforward. The effort seems fairly 
#' large. If students do that 
uglify.function = function(f,keep.funs=NULL, keep.vars = names(formals(f)), uglify.strings=TRUE) {
  restore.point("uglify.function")
  call = body(f)

  ign = ignore.uglify.syms(call)
  
  funs = setdiff(find.funs(call),c(keep.funs,"return",ign))
  vars = setdiff(find.variables(call),c(keep.vars,ign))

  ufuns = ugly.name(length(funs))
  uvars = ugly.name(length(funs))

  syms = c(funs,vars)
  usyms = c(ufuns, uvars)

  usyms.sym = lapply(usyms, as.name)
  names(usyms.sym) = syms
  ubody = substitute.call(call, usyms.sym)


  #nums = find.numbers(call)



  fun.env = new.env(parent = environment(f))

  if (uglify.strings)
    ubody = uglify.string.constants(ubody, fun.env)

  for (i in seq_along(funs)) {
    ufun.fun = fun.name.to.fun(funs[i])
    assign(ufuns[i],ufun.fun,fun.env)
  }
  ls(fun.env)

  form = formals(f)
  na = names(form)
  ind = match(na, vars)
  na[!is.na(ind)] = uvars[ind[!is.na(ind)]]
  change = na %in% vars
  names(form) = na


  g = f
  body(g) = ubody
  formals(g) = form

  environment(g) = fun.env
  g
}

ignore.uglify.syms = function(call, ignore.vars=FALSE) {
  if (is.name(call) & ignore.vars)
    return(as.character(call))
  
  if (length(call)<=1) return(NULL)
  if (call[[1]]=="$") {
    ignore.vars = TRUE
  }
  names = lapply(call[-1], function(e1) {
    ignore.uglify.syms(e1, ignore.vars=ignore.vars)
  })
  names = unique(unlist(names, use.names=FALSE))
  names
}

uglify.string.constants = function(call, env) {
  if (is.character(call)) {
    val = call
    var = random.string()
    assign(var,val,env)
    return(as.name(var))
  }
  if (length(call)<=1) return(call)
  for (ind in 2:length(call)) {
    call[[ind]] = uglify.string.constants(call[[ind]],env)
  }
  call
}

find.numbers = function(call) {
  if (is.numeric(call)) return(as.numeric(call))
  if (length(call)<=1) return(NULL)
  names = lapply(call[-1], function(e1) {
    find.numbers(e1)
  })
  names = unique(unlist(names, use.names=FALSE))
  names
}

random.string  = function(n=1,nchar=14) {
  chars = sample(c(letters,LETTERS,0:9),nchar*n, replace = TRUE)
  if (n == 1) return(paste0(chars, collapse=""))
  mat = as.data.frame(matrix(chars, n, nchar))
  do.call(paste0,mat)
}

ugly.name = function(n,nchar=20) {
  paste0("u",random.string(n,nchar))
}

fun.name.to.fun = function(fun.name, direct= (!substring(fun.name,1,1) %in% c(letters,LETTERS)) | fun.name %in% c("if","for","while")) {
  restore.point("make.ugly.fun")
  
  #bt = if (substring(fun.name,1,1) %in% c(letters,LETTERS)) "" else "`"
  bt =  "`"

  if (direct) {
    code=paste0(bt,fun.name,bt)
    fun = eval(parse.as.call(code))
  } else {
    code = paste0("function(...) ",bt,fun.name,bt,"(...)")
    fun = eval(parse.as.call(code))
  }
  fun

}


