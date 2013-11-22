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
init.tournament = function(strat, game,answers = NULL,delta=game$param$delta, type=ifelse(is.null(answers),"stage1","stage2"), score.fun = "efficiency - 2*instability - 20*instability^2", team=NULL) {
  restore.point("init.tournament")
  
  strat.id = seq_along(strat)
  type = type[1]  
  game$delta = delta
  matchings=NULL
  if (type!="stage2")
    matchings = all.vs.all.matchings(strat,game)    
  
  if (is.null(names(strat)))
    names(strat) = paste0("strat", strat.id)
  
  if (!is.null(answers)) {
    answers = answers[names(strat)]
  }
  
  time.str = gsub("[-:]","",as.character(now()))
  id = paste0("Tourn_",game$name,"_",time.str)
  id = gsub(" ","_",id)
  
  if (is.null(team))
    team = rep("", length(strat))
  
  for (s in seq_along(strat)) {
    attr(strat[[s]],"team.name")=team[s]
  }
  
  tourn = list(id=id,type=type,strat = strat, answers = answers, game = game, delta = delta, team=team, matchings = matchings, dt=NULL,prev.backup.num=0, score.fun = score.fun)
  class(tourn) = c("Tournament","list")
  return(tourn)
}

#' Runs a tournament with R repetitions of each matching and add these rounds to the tournament objects
#' 
#' By setting backup.each.R to a number, say 10, a backup of the tournament will be created after each 10 repetitions
run.tournament = function(tourn, strat=tourn$strat, answers=tourn$answers, matchings=tourn$matchings, game=tourn$game, delta=tourn$delta, R = 1, LAPPLY=lapply, backup.each.R=NULL, backup.path = getwd(),T.min = ceiling(log(0.01)/log(delta)), verbose=interactive(), do.store=FALSE) {
  restore.point("run.tournament")
   
  if (tourn$type=="stage2") {
    return(run.stage2.tournament(tourn=tourn, strat=strat, answers=answers,  game=game, delta=delta, R=R, LAPPLY=LAPPLY, backup.each.R=backup.each.R, backup.path=backup.path,T.min=T.min, verbose=verbose, do.store=do.store))
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
        u = run.rep.game(delta=delta,strat=strat.pair,game=game,detailed.return = FALSE, game.seed = game.seed,T.min=T.min, do.store=do.store)
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

run.one.match = function(match.strat,r=1, verbose=TRUE,game.seed,T.min, do.store=TRUE) {
  restore.point("one.match")
  u = run.rep.game(delta=delta,strat=match.strat,game=game,detailed.return = FALSE, game.seed = game.seed,T.min=T.min,do.store=do.store)
  names = names(match.strat)
  if (verbose)
    message(paste0(r,":",names[1]," vs ", names[2]," ",
                   round(u[1],4)," : ",round(u[2],4)))
  return(u)
}

#' Runs a tournament with R repetitions of each matching and add these rounds to the tournament objects
#' 
#' By setting backup.each.R to a number, say 10, a backup of the tournament will be created after each 10 repetitions
run.stage2.tournament = function(tourn, strat=tourn$strat, answers=tourn$answers, game=tourn$game,delta=tourn$delta, R = 1, LAPPLY=lapply, backup.each.R=NULL, backup.path = getwd(),T.min = ceiling(log(0.01)/log(delta)), verbose=interactive(), do.store = FALSE) {
  
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
        act.strat = c(strat[s],answers[[s]])
        u = sapply(seq_along(act.strat), function(s2) {
          
          ret1 = run.one.match(act.strat[c(1,s2)],r=r,verbose=verbose,game.seed=game.seed,T.min=T.min, do.store=do.store)
          ret2 = run.one.match(act.strat[c(s2,1)],r=r,verbose=verbose,game.seed=game.seed,T.min=T.min, do.store=do.store)
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
    return(add.stage2.tournament.stats(tourn))
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
  tourn$res = data.frame(team=tourn$team,rank = rank(round(-score,8),ties="min"),score=score, efficiency=efficiency, instability=instability, u.average=u.average, best.answer=br)[ord,,drop=FALSE]

  
  tourn$rep.matchings=table(tourn$dt$matching)
  tourn$stats.are.current = TRUE
  tourn
}



#' Add result statistics to a tournament object
add.stage2.tournament.stats = function(tourn) {
  restore.point("add.stage2.tournament.stats", force=TRUE)
  
  strat = tourn$strat
  answers = tourn$answers
  n=tourn$game$n
  
  efficiency = sapply(tourn$uv.list, function(uv) uv[1])
  instability = sapply(tourn$uv.list, function(uv) max(uv)-uv[1])
  u.average = rep(NA,length(strat))
  score = get.score(tourn$score.fun, efficiency, instability, u.average)
  
  # All best answers
  ba.payoff = sapply(seq_along(tourn$strat), function(s) {
    max(tourn$uv.list[[s]])
  })
  
  all.ba = lapply(seq_along(tourn$strat), function(s) {
    ind = which(tourn$uv.list[[s]]==ba.payoff[s])
    sa = c(strat[s], answers[[s]])[ind]
    ba.names = names(sa)
    ba.teams = sapply(sa, function(strat) attr(strat,"team.name"))
    quick.df(strat=rep(names(strat)[s],length(ind)), instability=signif(instability[s],4), best.answer=ba.names, best.answer.team=ba.teams)
  })
  all.ba = do.call(rbind,all.ba)
  tourn$all.best.answers = all.ba
  
  # A single best answer
  br = sapply(seq_along(tourn$strat), function(s) {
    c(names(strat)[s],names(answers[[s]]))[which.max(tourn$uv.list[[s]])]
  })
  br[instability==0] = names(strat)[instability==0]
  
  
  ord = order(score,decreasing=TRUE)
  res = data.frame(rank = rank(round(-score,8),ties="min"),team=tourn$team,score=score, efficiency=efficiency, instability=instability, u.average=u.average, best.answer=br)
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
      cat("\nAgainst ",names(tourn$strat)[i],":\n")
      uv = signif(tourn$uv.list[[i]],3)
      names(uv) = c(names(tourn$strat)[i],names(tourn$answers[[i]]))
      uv = sort(uv,decreasing=TRUE)
      print(uv)
    }
    cat("\nAll best answers:\n")
    print(df.signif(tourn$all.best.answers, fun=round))
    
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
