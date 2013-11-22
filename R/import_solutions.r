find.row = function(txt,pattern,li, stop.if.not.found = TRUE) {
  rows = which(str.starts.with(txt,pattern))
  if (length(rows)==0)
    stop(paste0("Did not find row starting with '", pattern, "' in file ", li$file))
  
  row = rows[1]
  str = str.trim(substring(txt[row],nchar(pattern)+1))
  list(row=row,str=str)
  
}

examples.import.stage1.strats = function() {
  library(StratTourn)
  library(compiler)

  dir = "D:/lehre/cooperation seminar/task1strat"
  num.scen = 1
  scen.strat = import.stage1.strats(dir, num.scen)[[1]]
  cbind(scen.strat$strat.name,scen.strat$team)
  
  
  
  game = make.pd.game(err.D.prob = 0.15)
  strat = scen.strat$strat
  team = scen.strat$team
  
  set.storing(FALSE)
  tourn = init.tournament(game=game,strat=strat, delta=0.95, team=team)  
  enableJIT(3)
  tourn = run.tournament(tourn=tourn, R=2)
  
  
  setwd("D:/lehre/cooperation seminar/")
  save.tournament(tourn,file="task1_tourn.Rdata")
  tourn

  setwd("D:/lehre/cooperation seminar/")
  tourn = load.tournament(file="task1_tourn.Rdata")
  tourn
}

#' Import all strategies from a directory with all team's solutions (as Rmd files) from stage 1 of a tournament
#' @param dir the directory in which the Rmd files are stored
#' @param num.scen the number of scenarios
#' @return a list that contains a list for each scenario. The list for each scenario has a list strat that contains the strategies of all teams.
#' @export
import.stage1.strats = function(dir, num.scen) {
  files = list.files(dir)
  
  files = paste0(dir,"/",files)
  team.li = lapply(files,parse.stage1.Rmd, num.scen=num.scen)
  
  scen.li = vector("list",num.scen)
  
  num.team = length(team.li)
  for (s in 1:num.scen) {
    scen.li[[s]]$code = lapply(1:num.team, function(t) {
      team.li[[t]]$code[[s]]
    })
    
    scen.li[[s]]$strat = lapply(1:num.team, function(t) {
      team.li[[t]]$strat[[s]]
    })
    scen.li[[s]]$team = sapply(1:num.team, function(t) {
      team.li[[t]]$team.name
    })
    
    
    strat.name = sapply(1:num.team, function(t) {
      team.li[[t]]$strat.name[s]
    })
    scen.li[[s]]$org.strat.name = strat.name
    
    dup = duplicated(strat.name)
    if (sum(dup)>0) {
      strat.name[dup] = paste0(strat.name,"_",substring(scen.li[[s]]$team,1,3))
      error("Message duplicated strategy names added 3 letters of team name...")
    }
    dup = duplicated(strat.name)
    if (sum(dup)>0) {
      strat.name[dup] = paste0(strat.name,"_",paste0(sample(c(0:9,letters,LETTERS),3),collapse=""))
      error("Message duplicated strategy names: added 3 random letters")
    }
    scen.li[[s]]$strat.name = strat.name
    names(scen.li[[s]]$strat) = strat.name
  }
  return(scen.li)
}


#file = "C:/libraries/StratTourn/coop1_sk.Rmd"
#num.scen = 1
parse.stage1.Rmd = function(file, num.scen) {
  restore.point("parse.stage1.Rmd")
  library(stringtools)
  li = list(file=file)
  txt = readLines(file)
  
  
  li$team.name = find.row(txt,"**Team-Name:**", li)$str
  
  # Extract code for each scenario
  li$code = lapply(1:num.scen, function(scen) {
    start.row = find.row(txt,paste0("```{r strat_scen",scen))$row
    end.row = find.row(txt[-(1:start.row)],"```")$row + start.row
    return(txt[(start.row+1):(end.row-1)])
  })
  
  li$strat = list()
  li$strat.names = rep("",num.scen)
  
  scen = 1
  for (scen in 1:num.scen) {
    env = new.env()
    tryCatch({
      eval(parse(text=li$code[[scen]]),env)
      fun.name <- ls(env)[1]
      li$strat.names[scen] <- fun.name
      li$strat[[scen]] <- get(fun.name,env)
    }, error = function(e) {
      str = paste0("Warning: Could not parse strategy in ", file, " for scenario ", scen, ". Code chunk does not specify a single correct function.")
      stop(str, call.=FALSE)
      li$strat.names[scen] <<- "NO STRATEGY"
      li$strat[[scen]] <<- NA
    })
    
  }
  names(li$strat) = li$strat.names
  li
}

examples.import.stage2.strats = function() {
  stage1.dir = "D:/lehre/cooperation seminar/task1strat"
  stage2.dir = "D:/lehre/cooperation seminar/task1answers"
  
  num.scen = 2
  
  set.storing(TRUE)
  s2 = import.stage2.strats(stage1.dir, stage2.dir, num.scen)
  game = make.pd.game(err.D.prob = 0.3)
  
  s = 2
  strat = s2[[s]]$strat
  answers = s2[[s]]$answers
  strat.team = s2[[s]]$strat.team
  names(answers)
  str(answers)
  
  tourn = init.tournament(game=game,strat=strat,answers=answers, delta=0.95, team=strat.team)  
  enableJIT(3)
  set.storing(FALSE)
  tourn = run.tournament(tourn=tourn, R=20)

  tourn
}

#' Import Stage 1 strategies and Stage 2 answers
#' @param stage1.dir the directory in which the Stage 1 Rmd files are stored
#' @param stage2.dir the directory in which the Stage 2 Rmd files are stored
#' @param num.scen the number of scenarios
#' @return a list that contains a list for each scenario. The list for each scenario has a list strat that contains the strategies of all teams.
#' @export
import.stage2.strats = function(stage1.dir, stage2.dir, num.scen) {
  s1 = import.stage1.strats(stage1.dir, num.scen)


  dir = stage2.dir
  files = list.files(dir)
  files = paste0(dir,"/",files)
  team.li = lapply(files,parse.stage2.Rmd, num.scen=num.scen)
  
  scen.li = vector("list",num.scen)
  
  get.answer.strat = function(t.li,s, strat.name) {
    restore.point("get.answer.strat")
    answer.name = t.li$answer.for[[s]][[strat.name]]
    if (is.null(answer.name))
      return(NULL)
    
    if (answer.name %in% t.li$answers.names[[s]]) {
      res = t.li$answers[[s]][answer.name]
      attr(res[[1]],"team.name") = t.li$team.name
      return(res)
    } else {
      if (answer.name != 'Name of your answer strategy')
        warning(paste0("Answer strategy '", answer.name, "' not found in scenario ", s, " of team '", t.li$team.name,"'"))
    } 
    return(NULL)
  }
  
  num.team = length(team.li)
  s = 2
  for (s in 1:num.scen) {
    scen.li[[s]]$stage1 = s1[[s]]
    scen.li[[s]]$strat = s1[[s]]$strat
    scen.li[[s]]$strat.team = s1[[s]]$team

    scen.li[[s]]$answers = list()
    scen.li[[s]]$answers.team = list()
    
    
    strat.names = s1[[s]]$strat.name
    for (strat.name in strat.names) {
      ans.li = lapply(team.li, get.answer.strat, s=s,strat.name=strat.name)
      ans.li = do.call("c",ans.li)
      scen.li[[s]]$answers[[strat.name]] = ans.li
      scen.li[[s]]$answers.team[[strat.name]] =sapply(scen.li[[s]]$answers[[strat.name]],
                                                     function(ans) attr(ans,"team.name"))
      
    }
    
  }    
  return(scen.li)
}


examples.parse.stage2.Rmd = function() {
  file = "D:/lehre/cooperation seminar/task1answers/coop1BoneCrushersAnswers.Rmd"
  s = parse.stage2.Rmd(file,4)
}


#file = "C:/libraries/StratTourn/coop1_sk.Rmd"
#num.scen = 1
parse.stage2.Rmd = function(file, num.scen) {
  restore.point("parse.stage2.Rmd")
  library(stringtools)
  li = list(file=file)
  txt = readLines(file)
  
  
  li$team.name = find.row(txt,"**Team-Name:**", li)$str
  
  # Extract code for each scenario
  li$code = lapply(1:num.scen, function(scen) {
    start.row = find.row(txt,paste0("```{r answer_strats_scen",scen),li)$row
    end.row = find.row(txt[-(1:start.row)],"```",li)$row + start.row
    return(txt[(start.row+1):(end.row-1)])
  })
  
  li$answer.for.code = lapply(1:num.scen, function(scen) {
    start.row = find.row(txt,paste0("```{r answer_for_scen",scen))$row
    end.row = find.row(txt[-(1:start.row)],"```")$row + start.row
    return(txt[(start.row+1):(end.row-1)])
  }) 
  
  
  li$answers = list()
  li$answers.names = list()
  
  scen = 1
  for (scen in 1:num.scen) {
    env = new.env()
    tryCatch({
      eval(parse(text=li$code[[scen]]),env)
      fun.names <- ls(env)
      li$answers.names[[scen]] <- fun.names
      li$answers[[scen]] <- lapply(fun.names, function(fun.name) get(fun.name,env))
      names(li$answers[[scen]]) = fun.names
    }, error = function(e) {
      str = paste0("Could not parse strategy in ", file, " for scenario ", scen, ". Code chunk does not specify a single correct function.")
      warning(str)
      li$answers.names[[scen]] <- NULL
      li$answers[[scen]] <- NULL
    })
    
    tryCatch({
      eval(parse(text=li$answer.for.code[[scen]]),env)
      li$answer.for[[scen]] <- get("answer.for", env)
    }, error = function(e) {
      str = paste0("Could not parse answer.for in ", file, " for scenario ", scen, ": ", as.character(e))
      warning(str)
      li$answer.for[[scen]] <- NULL
    })
    
  }
  
  li
}