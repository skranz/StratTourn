# 

make.answers.rmd = function(in.dir, out.dir, num.scen = 1, num.answer = 2, answer.scen = 1:num.scen) {
  in.dir = "D:/lehre/cooperation seminar/task2strat"
  out.dir = "D:/lehre/cooperation seminar/task2_stage2_template"
  
  tmp.file = "D:/libraries/StratTourn/StratTourn/coop2_answerstrats_vorlage.template.Rmd"
  
  num.answer = 2
  num.scen = 3
  must.scen = 1:3
  scs = import.stage1.strats(in.dir, num.scen)
  teams = scs[[1]]$team
  ignore.teams = c("The Overseer","Prof")
  
  if (!file.exists(out.dir))
    dir.create(out.dir)
  
  setwd(out.dir)
  
  scs = write.strat.code.for.all.scen(scs,teams=teams)  
  
  
  num.teams = length(teams)
  as = assign.answer.strats( teams = teams, num.scen=num.scen, num.answer=num.answer, ignore.teams = ignore.teams)
 
  t = 1
  for (t in 1:num.teams) {
    txt = render.answer.template(tmp.file,t,scs,as, teams, must.scen)
    cat(txt)
    
    setwd(out.dir)
    
    file = paste("answer strats ", teams[t], ".Rmd")
    writeLines(txt,file)
    
    #setwd(out.dir)
    #zipfile = paste0("Team ", teams[t],".zip")
    #zip(zipfile, files =  paste0("./Team ", teams[t]))
  }
  
  
  
  
  
}


render.answer.template = function(tmp.file,t,scs,as, teams, must.scen) {
  restore.point("render.answer.template")
  
  num.scen = length(scs)
  
  subs = list()
  for (scen in 1:num.scen) {
    strats = scs[[scen]]$strat.name
    names(strats) = teams
    field = paste0("answer_for_",scen)
    
    if (scen %in% must.scen) {
      act.strat = strats[as[[t]][[scen]]]
      str = paste0("#Strategien, gegen die ihr eine Antwort entwickeln muesst\n \n")
      str = paste0(str,paste0('"', act.strat,'" = "Name of your answer strategy"', collapse="\n,"           
      ))
      act.strat = setdiff(strats,act.strat)
      str = paste0(str,"\n \n
# Waehlt aus den verbleibenden Strategien mindestens zwei Strategien aus,
# fuer die ihr eine Antwortstrategien definiert\n 
")
      str = paste0(str,"\n,",paste0('"', act.strat,'" = "Name of your answer strategy"', collapse="\n,"           
      ))
      #cat(str)
    } else {
      str = "
# Waehlt die Strategien aus fuer die ihr  eine 
# Antwortstrategien definiert habt und gebt den
# jeweiligen Namen eurer Antwortstrategie an\n 
 "
      act.strat = strats
      str = paste0(str,paste0('"', act.strat,'" = "Name of your answer strategy"', collapse="\n,"           
      ))
    }
    str = sep.lines(str)
    str = paste0("  ",str)
    str = merge.lines(str)
    subs[[field]] = str
  }
  
  library(whisker)
  subs[["team_name"]] = teams[t]
  rmd = paste0(readLines(tmp.file), collapse = "\n")
  cat(rmd)  
  txt = whisker.render(rmd,subs)
  cat(txt)
  txt = gsub('&quot;','"',txt)
  txt
}

write.strat.code.for.all.scen = function(scs,teams) {
  num.scen = length(scs)
  # Generate for each scenario a file with all strategies
  scen = 1
  for (scen in 1:num.scen) {
    sc = scs[[scen]]
    sc$code = lapply(sc$code, function(code) paste0(code, collapse="\n"))
    sc$all.code = paste0(
      "#------------------------------------------------------------\n",
      "# Team: ",teams, "\n\n",
      sc$code, collapse="\n")
    #cat(sc$all.code)
    sc$all.code = paste0("# Scenario ", scen, "\n", "###########################################################\n\n" , sc$all.code, sep="")
    scs[[scen]] = sc
    
    file = paste0("strats_scen",scen,".r")
    writeLines(sc$all.code, file)
  }
  scs
}

assign.answer.strats = function(teams=paste0("team",1:num.teams), num.scen, num.answer = 2, offset.step=1, ignore.teams = NULL) {
  restore.point("assign.answer.strats")
  
  
  used.teams = setdiff(teams, ignore.teams)
  num.teams = length(used.teams)
  
  offsets = rep(seq(1,num.teams-num.answer, by=offset.step), length.out=num.scen)  
  li = lapply(1:num.teams, function(team) {
    li = lapply(1:num.scen, function(scen){
      used.teams[(team+(1:num.answer)+offsets[scen]-2) %% num.teams +1]
    })
    names(li) = paste0("scen",1:num.scen)
    li
  })
  names(li) = used.teams
  as = vector("list", length(teams))
  names(as) = teams
  as[used.teams] = li
  for (t in ignore.teams) {
    as[[t]] = as[[used.teams[1]]]
  }
  return(as)
}
