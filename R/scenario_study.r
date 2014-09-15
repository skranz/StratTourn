examples.init.scenario.study = function() {
  setwd("D:/libraries/StratTourn/studies")
  baseline = list(delta = 0.95, err.D.prob = 0.15)
  scenarios = list(
    baseline=list(),
    high.err.D =list(err.D.prob=.3),
    err.C = list(err.C.prob=0.15),
    low.delta = list(delta=0.5)
  )
  
  # new scenarios
  baseline = list(delta = 0.99, err.D.prob = 0.2)
  scenarios = list(
    baseline=list(),
    low.err.D =list(err.D.prob=.05),
    low.delta = list(delta=0.7)
  )

  
  strat.dir = "D:/libraries/StratTourn/task1strat"
  num.scen = 3
  scen.strat.li = import.stage1.strats(strat.dir, num.scen)

  study = init.scenario.study("Noisy PD", game.fun = make.pd.game, baseline, scenarios, scen.strat.li=scen.strat.li)
  run.scenario(study, 2, R=3)

  save.scenario.study(study)

  #study = load.scenario.study("Study_Noisy PD_20140912 093613.stu")
  study = load.scenario.study("Study_Noisy PD_20140913 114354.stu")

  set.restore.point.options(display.restore.point=!TRUE)
  set.storing(FALSE)
  run.scenarios(study, R=100)
  set.storing(TRUE)
  save.scenario.study(study)

  
  
  
  cbind(scen.strat$strat.name,scen.strat$team)

  
  strats =  nlist(always.coop,tit.for.tat,always.defect, net.nice0, net.nice1, forgiving.grim.trigger)

  strat.li = list(strats,strats)
  study = init.scenario.study("Noisy PD", game.fun = make.pd.game, baseline, scenarios, strat.li=strat.li)
  R = 3

  run.scenario(study, 1, R=R)

  set.storing(FALSE)
  run.scenarios(study)
  run.cross.scenarios(study,R=3)
  save.scenario.study(study)

  study = load.scenario.study("Study_Noisy PD_20140910 143903.stu")
  run.scenarios(study, R = 100)
  run.cross.scenarios(study,R=3)

  save.scenario.study(study)

}

init.scenario.study = function(study.name, game.fun, baseline, scenarios, strat.li=NULL, teams.li=NULL, tourn.li = NULL, study.id = NULL, scen.strat.li=NULL) {
  restore.point("init.scenario.study")
  n.scen = length(scenarios)
  
  # scen.strat.li is returned from import.stage1.strat, which parses students' .rmd files
  if (!is.null(scen.strat.li)) {
    if (is.null(strat.li))
      strat.li = lapply(scen.strat.li, function(scen.strat) scen.strat$strat)
    if (is.null(teams.li))
      teams.li = lapply(scen.strat.li, function(scen.strat) scen.strat$team)
    
    
  }
  
  if (is.null(strat.li)) strat.li = vector("list", n.scen)
  if (is.null(teams.li)) teams.li = vector("list", n.scen)
  if (is.null(tourn.li)) tourn.li = vector("list", n.scen)
  
  par.li = lapply(scenarios, function(scen) {
    par = baseline
    par[names(scen)] = scen
    par
  })
  
  game.li = lapply(par.li, function(par) {
    do.call(game.fun, par)  
  })
  game.name = game.li[[1]]$name
  
  if (is.null(study.id)) {
      time.str = gsub("[-:]","",as.character(now()))
      study.id = paste0("Study_",study.name,"_",time.str)
  }
  
  
  study = nlist(study.name,study.id, game.name, baseline, scenarios,par.li, game.li,strat.li, teams.li, tourn.li)
  study = as.environment(study)
  
  tourn.li = lapply(1:n.scen, init.scenario.tournament, study) 
  tourn.id = sapply(tourn.li, function(tourn) tourn$tourn.id)
  names(tourn.li) = tourn.id
  
  study$tourn.li = tourn.li
  study$tourn.id = tourn.id
  
  # cross tournaments for each scenario pair
  grid = expand.grid(list(scen1=1:n.scen, scen2=1:n.scen))
  grid = grid[grid$scen1 != grid$scen2,]
  grid = grid[order(grid$scen1,grid$scen2),]
  rownames(grid) = NULL
  ng = NROW(grid)
  cross.dt = data.table(grid)
  cross.dt$tourn = replicate(ng,list())
  for (row in 1:NROW(grid)) {
    tourn = init.scenario.cross.tournament(grid[row,1],grid[row,2],study)      
    cross.dt$tourn[[row]] = tourn
    
  }
  cross.dt$tourn.id = sapply(cross.dt$tourn, function(tourn) tourn$tourn.id)
  
  study$cross.dt = cross.dt
  study
}

init.scenario.tournament = function(scen.num, study) {
  par = study$par.li[[scen.num]]
  game = study$game.li[[scen.num]]
  strat = study$strat.li[[scen.num]]
                     
  tourn = do.call(init.tournament, list(game=game, strat=strat, id.add=scen.num))
  tourn
}

init.scenario.cross.tournament = function(scen1, scen2, study) {
  par = study$par.li[[scen1]]
  game = study$game.li[[scen1]]
  
  strat1 = study$strat.li[[scen1]]
  strat2 = study$strat.li[[scen1]]
  names(strat1) = paste0(names(strat1),"_1")
  names(strat2) = paste0(names(strat2),"_2")
  strat = c(strat1, strat2)
  id.add = paste0(scen1,"x",scen2)
  tourn = do.call(init.tournament, list(game=game, strat=strat, id.add=id.add))
  tourn
}



run.scenario = function(study,scen.num, ...) {
  tourn = study$tourn.li[[scen.num]]
  study$tourn.li[[scen.num]] = run.study.tourn(tourn,...)
}


run.study.tourn = function(tourn, R=10,use.jit=TRUE,load.tourn.data=TRUE,...) {
  restore.point("run.study.tourn")

  if (is.null(tourn$dt)) {
    try(tourn <- load.tournament(tourn=tourn))
  }
  if (use.jit) {
    library(compiler)
    enableJIT(3)
  }
  tourn = run.tournament(tourn=tourn, R = R,...)
  enableJIT(0)
  tourn
}


run.scenarios = function(study, scen.nums=seq_along(study$scenarios), R=10,...) {
  for (scen.num in scen.nums) {
    display("Scenario ", scen.num)
    tourn = study$tourn.li[[scen.num]]
    study$tourn.li[[scen.num]] = run.study.tourn(tourn,R=R,...)
  }
}

run.cross.scenarios = function(study, R=10,...) {
  restore.point("run.cross.scenarios")
  for (row in 1:NROW(study$cross.dt)) {
    li = study$cross.dt[row,]
    
    display("Scenario ", li$scen1, "x",li$scen2)
    tourn = li$tourn[[1]]
    study$cross.dt$tourn[[row]] = run.study.tourn(tourn,R=R,...)
  }
}


load.scenario.study = function(file, path=getwd()) {
  old.path = getwd()
  # create an object "study"
  setwd(path)
  load(file,verbose=TRUE)
  
  setwd(old.path)
  study
}

save.scenario.study = function(study, path=getwd()) {
  save.scenario.study.tournaments(study, path)
  for (tourn in study$tourn.li) {
    save.tournament(tourn,path = path)
  }
  for (tourn in study$cross.dt$tourn)
    save.tournament(tourn,path = path)

  tourn.li = study$tourn.li
  cross.li = study$cross.dt$tourn
  
  study$tourn.li = lapply(tourn.li, remove.tourn.data)
  study$cross.dt$tourn = lapply(cross.li, remove.tourn.data)
  
  file = paste0(path,"/",study$study.id,".stu")
  save(study, file=file)
  cat("\nScenario study saved under ", file)
  
  study$tourn.li = tourn.li
  study$cross.dt$tourn = cross.li
  
}

save.scenario.study.tournaments = function(study, path=getwd()) {
  for (tourn in study$tourn.li)
    save.tournament(tourn,path = path)
  for (tourn in study$cross.dt$tourn)
    save.tournament(tourn,path = path)
}

remove.tourn.data = function(tourn) {
  tourn$dt =NULL
  tourn
}