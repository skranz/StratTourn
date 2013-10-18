# 
# nested.list.col.names = function(nl,max.len, level=1,max.levels=length(max.len), name.sep="_") {
#   if (is.null(names(nl))) {
#     ind = seq_along(nl)
#   } else {
#     ind = names(nl)
#   }
#   if (level>= max.levels)
#     return(ind[1:min(max.len[level],length(ind))])
#   
#   child.cols = unlist(lapply(nl,function(i) {
#     child.str = nested.list.col.names(max.len=max.len,level=level+1,max.levels=max.levels, name.sep=name.sep)
#     
#   })
#   
#   
#   
#   
# } 

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
debug.store = function(strat,i,t) {
  store = !identical(.GlobalEnv,sys.frame(-1))  
  if (!store)
    return()
  
  name = paste0(strat,"_i=",i,"_t=",t)
  store.objects(name,parent.num=-2, deep.copy=FALSE, store.parent.env=FALSE)
}

#' restore objects if and only if called from the global environment
debug.restore = function(strat,i,t) {
  restore = identical(.GlobalEnv,sys.frame(-1))  
  if (!restore)
    return()
  name = paste0(strat,"_i=",i,"_t=",t)
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
  if (length(vec)==len) {
    return(vec)
  }
  if (length(vec)>len) {
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

rep.game.results.df = function(game, obs.hist, a.hist, payoff.mat, game.states=NULL, strat.states=NULL) {
  restore.point("hfhf")
  T = length(a.hist)
  my.unlist = function(li) {
    vec = unlist(li)
    if (is.null(vec))
      return(NULL)
    cols = length(vec) / T
    names = names(vec[1:cols])
    df = as.data.frame(matrix(vec,nrow=T,byrow=TRUE))
    colnames(df) = names
    df
  }
  a.df = my.unlist(a.hist)
  obs.df = my.unlist(obs.hist)
  state.df = my.unlist(game.states)
  colnames(obs.df) = paste0("obs.", colnames(obs.df))
  colnames(payoff.mat) = paste0("pi",1:NCOL(payoff.mat))
  if (!is.null(state.df) & NCOL(state.df)==1)
    colnames(state.df) = "state"
  
  glob.df = lapply(strat.states, my.unlist)
  for (i in 1:length(glob.df)) {
    if (!is.null(glob.df[[i]]))
      if (NCOL(glob.df[[i]])>0)
        colnames(glob.df[[i]]) = paste0(colnames(glob.df[[i]]),".",i)
  }
  null.glob = sapply(glob.df,function(df) is.null(df) | length(df)==0)
  glob.df = glob.df[!null.glob]
  #glob.df = do.call(cbind, glob.df)
  my.data.frame(list(t=1:T),payoff.mat,a.df,obs.df,state.df, glob.df)    
}

my.data.frame = function(..., ignore.empty=TRUE) {
  args = list(...)
  if (ignore.empty)
    args = args[sapply(args,length)>0]
  do.call(data.frame,args)
}



#' Runs a repeated game
#' @param delta discount factor
#' @param game the game object
#' @param strat a list of strategies
#' @param T.min the minium number of periods that will be played. Payoffs for periods t=1,...,T.min will be disocunted with delta^(t-1). After any period t>=T the game stops with probability 1-delta and all payoffs are discounted with delta^(T-1).
#' @param T.max optionally a maximum number of rounds
run.rep.game = function(delta=game$param$delta, game, strat, T.min=1,T.max = round(runif(1,10000,12000)),detailed.return = TRUE, strat.seed=NULL, game.seed = NULL) {
  restore.point("run.rep.game")

  set.random.state(".GLOBAL")
  
  if (is.null(game.seed))
    game.seed = draw.seed()
  if (is.null(strat.seed))
    strat.seed = draw.seed()

  c(game.seed,strat.seed)
  #Store seeds to replicate for debugging purposes
  gbos$game.seed = game.seed
  gbos$strat.seed = strat.seed
  
  init.random.state("game",game.seed)
  init.random.state("strat",strat.seed)
  
  set.random.state("game")
  T.rand = rnbinom(1,size=1,prob=1-delta)
  T = pmin(T.min + T.rand,T.max)
  
  
  
  strat.id = seq_along(strat)
  n.strat = length(strat.id)
  
  if (n.strat != game$n) {
    stop(paste("The game requires ",game$n, " players but you submitted ", n.strat, " strategies."))
  }
  
  if (game$private.signals) {
    obs = vector("list",game$n)
  } else {
    obs = NULL
  }

  denv = new.env()
  a = vector("list",n.strat)
  game.states = game$init.states
  # Names of strategy states
  sts.names = lapply(strat.id, function(i) {
    setdiff(names(formals(strat[[i]])),c("obs", "i","t","game","..."))
  })
  strat.states = lapply(strat.id, function(i) {
    list()
  })
  
  
  u = rep(0,game$n)
  delta.compound = 1
  t = 1
  for (t in 1:T) {

    if (detailed.return) {
      start.strat.states = strat.states
      start.game.states = game.states
    }
    
    # Set the random state for the strategy
    set.random.state("strat")
    
    # 1. Evaluate strategies of each player
    for (i in strat.id) {
      if (game$private.signals) {
        args = c(list(obs[[i]],i=i,t=t, game=game),game.states,strat.states[[i]])
      } else {
        args = c(list(obs,i=i,t=t, game=game),game.states,strat.states[[i]])
      }

      
      tryCatch(
        strat.res <- do.call(strat[[i]],args),
        error = function(e) {
          message("Error in evaluating strategy ", names(strat)[i], " in period t=", t, " for player i=",i,"\nERROR.HIST:")
          hist = denv$df[1:t,]
          
          print(tail(hist))
          assign(".BOS.ERROR.HIST",hist,.GlobalEnv)
          assign(".BOS.ERROR.t",t,.GlobalEnv)
          assign(".BOS.ERROR.i",i,.GlobalEnv)
          
          stop(as.character(e), call.=FALSE)
        }
      )
      strat.res = format.strat.res(t=t,i=i,game=game,strat.res=strat.res)
      
      a[[i]] = strat.res$a
      strat.states[[i]] = strat.res$strat.states
      game$check.action(ai=a[[i]],i=i,t=t)
    }
    names(a) = game$a.names

    # 2. Evaluate game results
    set.random.state("game")  
    results = game$results.fun(a=a,game.states=game.states,game=game)
    old.obs = obs
    obs = results$obs
    game.states = results$game.states
    
    # 3. Update total payoffs
    if (delta==1) {
      u = u + results$payoff
    } else {      
      u = u + (delta.compound) * results$payoff 
    }
    
    if (t<T.min) {
      delta.compound = delta.compound*delta
    }

    # 4. Update statistics if desired
    if (detailed.return) {
      rep.game.store.detailed.return(t,old.obs,obs,a, payoffs=results$payoff, start.strat.states,next.strat.states=strat.states, game.states = start.game.states,next.game.states=game.states, denv,T, game, strat=strat)
    }
  }
  
  # Compute expexted number of rounds
  if (T.min>1) {
    tau = T.min-1
    t = 1:tau
    sum(delta^(t-1)*(1-delta) ) + delta^tau
    ET = sum(delta^(t-1)*(1-delta)*t) + delta^tau*T
    u/ET
  } else {
    ET = T
  }
  
  u = u/ET
  if (detailed.return) {
    return(list(hist=denv$df,u=u))
  }
  return(u)
}  

format.strat.res = function(t,i,game,strat.res, sts.names=NULL) {
  ai.names = names(game$example.action(i=i,t=t))
  if (length(ai.names)==1) {
    return(list(a=strat.res[[1]], strat.states=strat.res[-1]))
  } else {
    return(list(a = strat.res[1:length(ai.names)], strat.states = strat.res[-(1:length(ai.names))]))
  }  
}

to.named.list=function(val, names) {
  stop("Not yet implemented")
  if (length(val) != length(names))
    stop("val and names have different length")
  li = vector("list")
}


rep.game.store.detailed.return = function(t,obs,next.obs,a, payoffs, strat.states,next.strat.states, game.states,next.game.states,denv,T, game, max.state.vector.size = 4, strat) {
  restore.point("rep.game.store.detailed.return")
  n = game$n
  if (t == 1) {
    restore.point("rep.game.store.detailed.return_t1")
    
    # Get a list of all names
    a.names = lapply(1:n, function(i) {
      ai = a[[i]]
      if (is.list(ai))
        return(paste0(names(ai),i))
      return(paste0("a",i))
    })
    a.names = do.call("c",a.names)
    payoff.names = paste0("pi",1:n)

    ex.obs = game$example.obs()
    
    # Get names of observations
    if (!game$private.signals) {
      obs.names = str.combine("obs_",names(ex.obs))
      obs.len = sapply(ex.obs,length)
      obs.cols = unlist(lapply(seq_along(obs.names), function(j) {
        len = obs.len[j]
        if (len > 1) {
          str.combine(obs.names[j],1:len)
        } else {
          obs.names[j]
        }
      }))
      
    } else {
      obs.names = lapply(1:n, function(i) {
        str.combine("obs",i,"_",names(ex.obs[[i]]))
      })
      obs.len = lapply(1:n, function(i) {
        sapply(ex.obs[[i]], length)
      })
      obs.cols = unlist(lapply(1:n, function(i) {
        sapply(seq_along(obs.names[[i]]), function(j) {
          len = obs.len[[i]][j]
          if (len > 1) {
            str.combine(obs.names[[i]][j],"_",1:len)
          } else {
            obs.names[[i]][j]
          }
        })
      }))
    }
    
    # Get a list of all strategy state names
    strat.states.names = lapply(1:n, function(i) {
      si = next.strat.states[[i]]
      str.combine(names(si),"_",i)
    })
    
    #strat.states.names = lapply(1:n, function(i) {
    #  si = setdiff(names(formals(strat[[i]])),c("obs", "i","t","game","..."))
    #  str.combine(si,"_",i)
    #})
    
    
    strat.states.len = lapply(1:n, function(i) {
      sapply(next.strat.states[[i]], length)
    })
    strat.states.cols = unlist(lapply(1:n, function(i) {
      sapply(seq_along(strat.states.names[[i]]), function(j) {
        len = min(strat.states.len[[i]][j],max.state.vector.size)
        if (len > 1) {
          str.combine(strat.states.names[[i]][j],"_",
                    1:len)
        } else {
          strat.states.names[[i]][j]
        }
      })
    }))
    next.strat.states.cols = str.combine("next_",strat.states.cols)
    
    game.states.names = names(game.states)
    game.states.len = sapply(game.states,length)
    game.states.cols = unlist(lapply(seq_along(game.states.names), function(j) {
      len = min(game.states.len[j],max.state.vector.size)
      if (len > 1) {
        str.combine(game.states.names[j],"_",
                    1:len)
      } else {
        game.states.names[j]
      }
    }))
    next.game.states.cols = str.combine("next_",game.states.cols)
    
    col.names = unlist(c(game.states.cols,obs.cols,a.names,payoff.names,strat.states.cols))
    df = as.data.frame(matrix(NA,T,length(col.names)))
    names(df)=col.names
    denv$df = df
    denv$a.names = a.names
    denv$obs.len = obs.len
    denv$obs.cols = obs.cols
    denv$payoff.names = payoff.names
    denv$strat.states.len = strat.states.len
    denv$strat.states.cols = strat.states.cols
    denv$next.strat.states.cols = next.strat.states.cols
    
    denv$game.states.len = game.states.len
    denv$game.states.cols = game.states.cols
    denv$next.game.states.cols = next.game.states.cols
                       
  }

  denv$df[t,denv$a.names] = unlist(a)
  denv$df[t,denv$payoff.names] = payoffs

  if (game$private.signals) {
    denv$df[t,denv$obs.cols] = unlist(lapply(1:n, function(i) {
      sapply(seq_along(denv$obs.len[[i]]), function(j) {
        if (j > length(obs[[i]][[j]]))
          return(rep(NA,denv$obs.len[[i]][j]))
        to.length(obs[[i]][[j]],denv$obs.len[[i]][j])
      })
    }))  
  } else {
    denv$df[t,denv$obs.cols] = 
      unlist(lapply(seq_along(denv$obs.len), function(j) {
        if (j > length(obs[[j]]))
          return(rep(NA,denv$obs.len[j]))  
        to.length(obs[[j]],denv$obs.len[j])
      }))
  }
  
  denv$df[t,denv$strat.states.cols] = unlist(lapply(1:n, function(i) {
    sapply(seq_along(denv$strat.states.len[[i]]), function(j) {
      if (j > length(next.strat.states[[i]]))
        return(rep(NA,denv$strat.states.len[[i]][j]))
      
      to.length(next.strat.states[[i]][[j]],denv$strat.states.len[[i]][j])
    })
  }))  
  
  denv$df[t,denv$game.states.cols] = 
    sapply(seq_along(denv$game.states.len), function(j) {
      if (j > length(game.states[[j]]))
        return(rep(NA,denv$game.states.len[j]))
      
      to.length(game.states[[j]],denv$game.states.len[j])
    })
  
}       

#' Returns default matchings
all.vs.all.matchings = function(strat,game) {
  n = game$n
  stopifnot(n==2)
  strat.id = seq_along(strat)
  matchings = as.matrix(expand.grid(strat.id, strat.id)[,2:1])
  #if (sym)
  #  matchings = matchings[matchings[,1]<= matchings[,2],]
  matchings
}

first.vs.all.matchings = function(strat,game) {
  n = game$n
  stopifnot(n==2)
  strat.id = seq_along(strat)
  as.matrix(rbind(expand.grid(1, strat.id),expand.grid(strat.id[-1],1)))
}

#' Inits a tournament object
init.tournament = function(strat, game,strat.dev = NULL,delta=game$param$delta, type=ifelse(is.null(strat.dev),"stage1","stage2"), score.fun = "efficiency - 2*instability - 20*instability^2") {
  restore.point("init.tournament")
  
  strat.id = seq_along(strat)
  type = type[1]  
  game$delta = delta
  matchings=NULL
  if (type!="stage2")
    matchings = all.vs.all.matchings(strat,game)    
  
  if (is.null(names(strat)))
    names(strat) = paste0("strat", strat.id)
  
  if (!is.null(strat.dev)) {
    strat.dev = strat.dev[names(strat)]
  }
  
  time.str = gsub("[-:]","",as.character(now()))
  id = paste0("Tourn_",game$name,"_",time.str)
  id = gsub(" ","_",id)
  tourn = list(id=id,type=type,strat = strat, strat.dev = strat.dev, game = game, delta = delta, matchings = matchings, dt=NULL,prev.backup.num=0, score.fun = score.fun)
  class(tourn) = c("Tournament","list")
  return(tourn)
}

#' Runs a tournament with R repetitions of each matching and add these rounds to the tournament objects
#' 
#' By setting backup.each.R to a number, say 10, a backup of the tournament will be created after each 10 repetitions
run.tournament = function(tourn, strat=tourn$strat, strat.dev=tourn$strat.dev, matchings=tourn$matchings, game=tourn$game, delta=tourn$delta, R = 1, LAPPLY=lapply, backup.each.R=NULL, backup.path = getwd(),T.min = ceiling(log(0.01)/log(delta)), verbose=interactive()) {
  restore.point("run.tournament")
 
  if (tourn$type=="stage2") {
    return(run.stage2.tournament(tourn=tourn, strat=strat, strat.dev=strat.dev,  game=game, delta=delta, R=R, LAPPLY=LAPPLY, backup.each.R=backup.each.R, backup.path=backup.path,T.min=T.min, verbose=verbose))
  }
  
  num.chunks = 1
  if (!is.null(backup.each.R)) {
    num.chunks = ceiling(R / backup.each.R)
  }
  R.chunk = ceiling(R / num.chunks)
  
  for (chunk in 1:num.chunks) {
    
    if (chunk == num.chunks) {
      R.chunk = R -(R.chunk*(num.chunks-1))
    }
    
    dt.li = LAPPLY(1:R.chunk, function(r) {
      set.random.state(".GLOBAL")
      game.seed = draw.seed()
      if (verbose)
        message(paste0("game.seed = ", game.seed))
      
      u = LAPPLY(1:NROW(matchings), function(i) {
        restore.point("one.match")
        ind = as.numeric(matchings[i,])
        strat.pair = strat[ind]
        u = run.rep.game(delta=delta,strat=strat.pair,game=game,detailed.return = FALSE, game.seed = game.seed,T.min=T.min)
        names = names(strat)[ind]
        if (verbose)
          message(paste0(r, "/",R,":",names[1]," vs ", names[2]," ",
                       round(u[1],4)," : ",round(u[2],4)))
        return(u)
      })
      u = do.call(rbind,u)
      dt = data.table(matching=1:NROW(matchings),u)
      dt
    })
    dt = rbindlist(dt.li)
    setnames(dt,c("matching",paste0("u",1:game$n)))
    if (!is.null(tourn$dt)) {
      tourn$dt = rbind(tourn$dt,dt, use.names=FALSE)
    } else {
      tourn$dt = dt
    }
    tourn = add.tournament.stats(tourn)
    if (!is.null(backup.each.R)) {
      tourn = save.tournament(tourn,path=backup.path)
    }
  }
    
  return(tourn)
}

run.one.match = function(match.strat,r=1, verbose=TRUE,game.seed,T.min) {
  restore.point("one.match")
  u = run.rep.game(delta=delta,strat=match.strat,game=game,detailed.return = FALSE, game.seed = game.seed,T.min=T.min)
  names = names(match.strat)
  if (verbose)
    message(paste0(r,":",names[1]," vs ", names[2]," ",
                   round(u[1],4)," : ",round(u[2],4)))
  return(u)
}

#' Runs a tournament with R repetitions of each matching and add these rounds to the tournament objects
#' 
#' By setting backup.each.R to a number, say 10, a backup of the tournament will be created after each 10 repetitions
run.stage2.tournament = function(tourn, strat=tourn$strat, strat.dev=NULL, game=tourn$game, delta=tourn$delta, R = 1, LAPPLY=lapply, backup.each.R=NULL, backup.path = getwd(),T.min = ceiling(log(0.01)/log(delta)), verbose=interactive()) {
  restore.point("run.stage2.tournament")
  
  num.chunks = 1
  if (!is.null(backup.each.R)) {
    num.chunks = ceiling(R / backup.each.R)
  }
  R.chunk = ceiling(R / num.chunks)
  
  for (chunk in 1:num.chunks) {
    
    if (chunk == num.chunks) {
      R.chunk = R -(R.chunk*(num.chunks-1))
    }
    
    ul.li = LAPPLY(1:R.chunk, function(r) {
      set.random.state(".GLOBAL")
      game.seed = draw.seed()
      if (verbose)
        message(paste0("game.seed = ", game.seed))
      ul = lapply(seq_along(strat), function(s) {
        act.strat = c(strat[s],strat.dev[[s]])
        u = sapply(seq_along(act.strat), function(s2) {
          
          ret1 = run.one.match(act.strat[c(1,s2)],r=r,verbose=verbose,game.seed=game.seed,T.min=T.min)
          ret2 = run.one.match(act.strat[c(s2,1)],r=r,verbose=verbose,game.seed=game.seed,T.min=T.min)
          (ret1[2]+ret2[1]) / 2 # average payoff of s2 against the original strategy 
        })
        u        
      })
      ul
    })
    # Compute mean over all repetitions
    ul = ul.li[[1]]
    for (r in (1:R)[-1]) {
      for (s in 1:length(ul)) {
        ul[[s]] = ul[[s]]+ul.li[[r]][[s]]
      }
    }
    for (s in 1:length(ul)) {
      ul[[s]] = ul[[s]]/R
    }
    tourn$uv.list = ul
    tourn = add.tournament.stats(tourn)
    tourn$R = tourn$R+R.chunk
    if (!is.null(backup.each.R)) {
      tourn = save.tournament(tourn,path=backup.path)
    }
  }
  
  return(tourn)
}

get.score = function(score.fun, efficiency, instability, u.average) {
  if (is.character(score.fun)) {
    return(eval(parse(text=score.fun)))
  } else if (is.function(score.fun)) {
    return(score.fun(efficiency=efficiency, instability=instability, u.average=u.average))
  }
  stop("score.fun must either be a function or a string that contains the formula")
}


#' Add result statistics to a tournament object
add.tournament.stats = function(tourn) {
  restore.point("add.tournament.stats")

  if (tourn$type == "stage2") {
    return(add.stage2.tournament.stats(tourn,instability.factor))
  }
  
  strat = tourn$strat
  matchings = tourn$matchings
  n=tourn$game$n
  
  code = paste0("m",1:n,"=mean(u",1:n,")", collapse=",")
  mu = quick.by(tourn$dt,by="matching",code)
  match = matchings[mu$matching,]
  mu = mu[,-1]

  if (tourn$game$sym) {
    mat = matrix(NA,length(strat),length(strat))
    
    # Matrix shows payoff of row player
    mat[match] = mu[,1]
    mat[match[,2:1]] = mu[,2]
    
    # For matches between same strategies, use average payoff
    same.match = (match[,1]==match[,2]) * match[,1]
    
    diag(mat)[same.match[same.match>0]] <-(mu[same.match>0,1]+mu[same.match>0,2])/2
  } else {
    mat1 = mat2 = matrix(NA,length(strat),length(strat))
    
    # Payoff of row player
    mat1[match] = mu[,1]
    # Payoff of column player
    mat2[match] = mu[,2]
    
    # Average payoff of row strategy against col strategy
    mat = (mat1 + t(mat2)) / 2
    
  }
  colnames(mat) = rownames(mat) = names(strat)
  
  tourn$mat = mat
  
  efficiency = diag(mat)
  instability = apply(mat,2,max)-efficiency
  u.average = apply(mat,1,mean)
  score = get.score(tourn$score.fun, efficiency, instability, u.average)
  
  br = names(strat[apply(mat,2,which.max)])
  br[instability==0] = names(strat)[instability==0]
  
  
  ord = order(score,decreasing=TRUE)
  tourn$res = data.frame(rank = rank(round(-score,8),ties="min"),score=score, efficiency=efficiency, instability=instability, u.average=u.average, best.answer=br)[ord,,drop=FALSE]

  
  tourn$rep.matchings=table(tourn$dt$matching)
  tourn$stats.are.current = TRUE
  tourn
}



#' Add result statistics to a tournament object
add.stage2.tournament.stats = function(tourn, instability.factor = tourn$instability.factor) {
  restore.point("add.stage2.tournament.stats")
  
  tourn$instability.factor = instability.factor  
  strat = tourn$strat
  n=tourn$game$n
  
  efficiency = sapply(tourn$uv.list, function(uv) uv[1])
  instability = sapply(tourn$uv.list, function(uv) max(uv)-uv[1])
  u.average = rep(NA,length(strat))
  score = get.score(tourn$score.fun, efficiency, instability, u.average)
  
  br = sapply(seq_along(tourn$strat), function(s) {
    c(names(strat)[s],names(strat.dev[[s]]))[which.max(tourn$uv.list[[s]])]
  })
  br[instability==0] = names(strat)[instability==0]
  
  ord = order(score,decreasing=TRUE)
  res = data.frame(rank = rank(round(-score,8),ties="min"),score=score, efficiency=efficiency, instability=instability, u.average=u.average, best.answer=br)
  rownames(res) = names(strat)
  tourn$res = res[ord,,drop=FALSE]
  tourn$stats.are.current = TRUE
  tourn
}

print.stage2.tournament = function(tourn) {
  if (is.null(tourn$uv.list)) {
    str = paste0("\n2nd stage tournament for ", tourn$game$name,
                 "\nStrategies:", paste0(names(tourn$strat),collapse=","),
                 "\nNo results yet...")
    cat(str)
  } else {
    str = paste0("\n2nd Stage tournament for ", tourn$game$name," ", tourn$R, " rep.\n\n")
    cat(str)
    #tourn = add.tournament.stats(tourn)
    for (i in seq_along(tourn$uv.list)) {
      cat("Against ",names(tourn$strat)[i],":\n")
      uv = signif(tourn$uv.list[[i]],3)
      names(uv) = c(names(tourn$strat)[i],names(tourn$strat.dev[[i]]))
      uv = sort(uv,decreasing=TRUE)
      print(uv)
    }
    if (is.character(tourn$score.fun)) {
      cat(paste0("\nRanking with score = ",tourn$score.fun,"\n\n"))
    } else {
      cat(paste0("\nRanking\n"))      
    }
    print(df.signif(tourn$res,3,fun=round))
  }
}


print.Tournament = function(tourn) {
  if (tourn$type=="stage2") {
    return(print.stage2.tournament(tourn))
  }
  if (NROW(tourn$dt)==0) {
    str = paste0("\nTournament for ", tourn$game$name,
                 "\nStrategies:", paste0(names(tourn$strat),collapse=","),
                 "\nNo results yet...")
    cat(str)
  } else {
    rm = range(tourn$rep.matchings)
    if (diff(rm)==0) {
      rep.str = paste0("(",rm[1], " rep.)")
    } else {
      rep.str = paste0("(",rm[1], "-", rm[2], "rep.)")
    }
    str = paste0("\nTournament for ", tourn$game$name," ", rep.str, "\n\n")
    cat(str)
    #tourn = add.tournament.stats(tourn)
    print(signif(tourn$mat,3))
    if (is.character(tourn$score.fun)) {
      cat(paste0("\nRanking with score = ",tourn$score.fun,"\n\n"))
    } else {
      cat(paste0("\nRanking\n"))      
    }
    print(df.signif(tourn$res,3,fun=round))
  }
}

#' Saves a tournament to a file
save.tournament = function(tourn,path=getwd(),file=NULL, add.stats = FALSE) {
  if (is.null(file)) {
    tourn$prev.backup.num = (tourn$prev.backup.num) %% 2 +1
    file = paste0(tourn$id,"_B",tourn$prev.backup.num,".Rdata")
  }
  fn = paste0(path,"/",file)
  if (add.stats)
    tourn = add.tournament.stats(tourn)
  
  save(tourn, file=fn)
  message("Tournament saved under ", fn)
  invisible(tourn)
}

#' Loads a tournament from a file
load.tournament = function(file) {
  load(file,verbose=TRUE)
  return(tourn)
}
