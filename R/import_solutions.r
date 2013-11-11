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
  strat = c(scen.strat$strat,nlist(nn))
  team = substring(c(scen.strat$team,"prof"),1,8)
  
  set.storing(FALSE)
  tourn = init.tournament(game=game,strat=strat, delta=0.95, team=team)  
  enableJIT(3)
  tourn = run.tournament(tourn=tourn, R=300)
  
  
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