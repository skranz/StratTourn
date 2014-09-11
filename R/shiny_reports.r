examples.run.shiny.reports = function() {
  run.shiny.reports()
}

sr = new.env(parent=globalenv())

get.sr = function() {
  sr
}

run.shiny.reports = function() {
  library(restorepoint)
  library(shiny)
  library(shinyIncubator)
  library(knitr)
  library(stringtools)
  library(xtable)
  library(shinyBS)
  library(googleVis)
  set.restore.point.options(display.restore.point=!TRUE)
  setwd("D:/libraries/StratTourn/studies")
  set.view.mode("shiny_report")
  sr = get.sr()
  sr$file.path = getwd()
  sr$tourn.file = "Tourn_Noisy_PD_20140910_143903_1.tou"
  set.tourn.file(sr$tourn.file)
  
  sr$rep.li = make.rep.li()
  sr$report = sr$rep.li[[1]]
  
  reset.event.handlers()
  
  ui = make.report.ui()
  server = make.report.server()
  
  runApp(list(ui=ui,server=server), launch.browser=TRUE)
}

make.rep.li = function() {
  reports.yaml = "
payoff_ranking:
  label: payoff ranking
  file: D:/libraries/StratTourn/StratTourn/reports/matches_ranking.rmd
payoff_matrix:
  label: payoff matrix
  file: D:/libraries/StratTourn/StratTourn/reports/matches_payoff_matrix.rmd
duels_plot:
  label: duels plot
  file: D:/libraries/StratTourn/StratTourn/reports/matches_duels_plot.rmd
reciprocity:
  label: reciprocity
  file: D:/libraries/StratTourn/StratTourn/reports/reciprocity_regressions.rmd
payoffs_over_time:
  label: payoff over time
  file: D:/libraries/StratTourn/StratTourn/reports/payoffs_over_time.rmd
duels_over_time:
  label: duels over time
  file: D:/libraries/StratTourn/StratTourn/reports/duels_over_time.rmd
evolution:
  label: evolution
  file: D:/libraries/StratTourn/StratTourn/reports/evolution.rmd


"
  library(yaml)
  rep.li = yaml.load(reports.yaml)

  names = names(rep.li)
  rep.li = lapply(seq_along(rep.li), function(i) {
    rep = rep.li[[i]]
    rep$id = names(rep.li)[i]
    rep$button.id = paste0(rep$id,"_report_btn")
    rep
  })
  names(rep.li)=names
  rep.li
}

make.report.ui = function(sr=get.sr()) {
  strats = sr$strats
  ui = fluidPage("Analyse Tournament",
    progressInit(),             
    sidebarLayout(
      sidebarPanel(
        tabsetPanel(id ="leftPanel",
          tabPanel("Reports",
            bsActionButton("update_strat_btn","update", size="small"),       
            selectizeInput("used_strats", label = "Used strategies:",
            choices = strats, selected = strats, multiple=TRUE,width="100%"),
            select.report.ui(),
            uiOutput("ui.custom.parameters")

          ),
          tabPanel(title="Load",
            select.tournament.ui()
          )
        )
      ),
      mainPanel(
        uiOutput("reportPanel")
      )
    )
  )
  ui
}

make.report.server = function(sr = get.sr()) { 
  server = function(input, output, session) {
    #shinyFileChoose(input, 'tourn.file.dialog', root='.', filetypes=c('.tou'))
    
    new.report = reactiveValues(counter=0)
    update.report = reactiveValues(counter=0, type="report")
    update.tourn = reactiveValues(counter=0)

    
    output$ui.custom.parameters = renderUI({
      cat("render ui.custom.parameters")
      new.report$counter
      make.ui.custom.parameters(session=session)
    })
        
    output$reportPanel = renderUI({
      cat("render reportPanel")
      update.report$counter
      html=compile.report(sr$report, session=session)
      HTML(html)
    })

    output$tourn.info.output = renderUI({
      cat("render tourn.info.output")
      update.tourn$counter
      tourn = get.sr()$tourn
      if (is.null(tourn))
        return(NULL)
      n.matches = NROW(tourn$dt)
      str = paste0(
        tourn$game$name,
        "<BR>matches: ", n.matches,"<BR>",
        "<BR>delta=", tourn$game$delta,"<BR>",
        paste0(names(tourn$game$params)," = ",tourn$game$params, collapse="\n<BR>")
      )
      HTML(str)
    })

    observe({
      report = input[["reports_navbar"]]
      cat("Report menu: ", report)
    })
    
    button.click.handler("load.tourn.btn",click_load_tourn, update.report=update.report, update.tourn=update.tourn)
    button.click.handler("run.tourn.btn",click_run_tourn, update.report=update.report, update.tourn=update.tourn)

    button.click.handler("update_strat_btn", click_update_strat, update.report=update.report)
    for (rep in sr$rep.li) {
      button.click.handler(rep$button.id, click_report_btn, update.report=update.report, new.report=new.report)
    }
  }
  server
}


select.tournament.ui = function(sr=get.sr()) {
  restore.point("select.tournament.ui")
  files = list.files(sr$file.path, pattern=".*\\.tou")
  fluidRow(
    selectizeInput('tourn.file.input',"Tournament",choices=files, selected=sr$tourn.file,multiple=FALSE, width="100%"),
    bsActionButton("load.tourn.btn","load", size="small"),
    bsActionButton("run.tourn.btn","run", size="small"),
    numericInput("rep.tourn.input","R",value = 10,min = 1),
    htmlOutput("tourn.info.output")
  )
}

select.report.ui = function(rep.li=sr$rep.li, sr=get.sr()) {
  restore.point("select.report.ui")
  buttons = lapply(rep.li, function(rep) {
    bsActionButton(rep$button.id,rep$label)
  })
  names(buttons)=NULL
  do.call(fluidRow,buttons)
  #buttons[[1]]
}

adapt.round.data = function(tourn, used.strats=NULL, strats=tourn$strats, sr=get.sr()) {
  if (!identical(used.strats,strats)) {
    rows = rd$strat %in% used.strats & rd$other.strat %in% used.strats
    sr$rd = sr$ard[rows,]
  } else {
    sr$rd = sr$ard
  }

}

set.tourn.data = function(tourn=sr$tourn, used.strats=sr$used.strats, sr=get.sr()) {
  restore.point("set.tourn.data")
  
  # Data for each match
  md = tourn$dt
  md = add.other.var(md,c("strat","u"))
  md$delta.u = md$u - md$other.u
  
  # Names of all strategies
  strats = unique(md$strat)
  
  amd = md
  if (is.null(used.strats)) {
    used.strats = strats
  } else {
    rows = md$strat %in% used.strats & md$other.strat %in% used.strats
    md = amd[rows,]
  }
    
  copy.into.env(dest=sr,names = c("tourn","amd", "md","strats","used.strats"))
  
}


click_run_tourn = function(session,update.report,update.tourn,...,sr=get.sr()) {
  tourn = sr$tourn
  R = isolate(session$input$rep.tourn.input)
  restore.point("click_run_tourn")
  
  do.store = is.storing()
  set.storing(FALSE)
  withProgress(session, {
    shinyIncubator::setProgress(message = "Running tournaments...",
    detail = paste0("Finished 0 / ", R))
    for (r in 1:R) {
      tourn = run.tournament(tourn,R = 1)
      shinyIncubator::setProgress(detail = paste0("Finished ",r," / ", R))
    }
    set.storing(do.store)
  
    sr$tourn = tourn
    shinyIncubator::setProgress(detail = paste0("Save tournament..."))
 
    save.tournament(tourn=tourn, path=sr$file.path)

  })
  
  update.report$counter = isolate(update.report$counter+1)
  update.tourn$counter = isolate(update.tourn$counter+1)
  
}


click_load_tourn = function(session, update.report,update.tourn,...,sr=get.sr()) {
  value = isolate(session$input$tourn.file.input)
  cat("\nchange.tourn.file :", value)
  set.tourn.file(value)
  tourn = sr$tourn
  strats=names(tourn$strat)
  #selectizeInput("used_strats", label = "Used strategies:",
  #          choices = strats, selected = strats, multiple=TRUE,width="100%")
  updateSelectizeInput(session, "used_strats",label = "Used strategies:",
    choices = strats, selected = strats)
  
  update.report$counter = isolate(update.report$counter+1)
  update.tourn$counter = isolate(update.tourn$counter+1)
  
}

click_report_btn = function(session,id,..., update.report,new.report, sr=get.sr()) {
  restore.point("click_update_strat")
  cat("I am in click_update_strat")
  #browser()
  id = str.left.of(id,"_report_btn")
  sr$report = sr$rep.li[[id]]
  set.tourn.data()  

  new.report$counter = isolate(new.report$counter+1)
  update.report$counter = isolate(update.report$counter+1)
  update.report$type = "report"

}


click_update_strat = function(session,..., update.report) {
  restore.point("click_update_strat")
  cat("I am in click_update_strat")
  #browser()
  input=session$input
  used.strats = isolate(input$used_strats)
  set.tourn.data(used.strats=used.strats)
  
  update.report$counter = isolate(update.report$counter+1)
  
}


make.ui.custom.parameters = function(session, sr=get.sr()) {  
  restore.point("make.ui.custom.parameters")
  rep = sr$report
  file = rep$file[1]
  txt = readLines(file)
  
  env = new.env(parent=sr)
  eval.custom.parameters(txt, env)
  
  var = ls(env)
  val = lapply(var, get, pos=env)
  names(val) = var
  
  inputs = lapply(var, function(v) {
    id = paste0("rep_par_",v)
    textInput(id, label=v, value=val[[v]])
  })
  class = sapply(val, function(v) class(v)[1])
  
  df = quick.df(var=var, val=val, class=class, input.id=paste0("rep_par_", var))
  sr$ui.par.df = df
  
  do.call(fluidRow, inputs)
  
  #stop("stop here")
}
  
eval.custom.parameters = function(rmd.code, env) {
  # remove init_data chunk
  txt = rmd.code
  cdf = find.rmd.chunks(txt)
  rows = which(cdf$chunk.name=="init_parameters")
  if (length(rows)==0) {
    sr$ui.par.df = NULL
    return(NULL)
  }
  row = rows[1]
  code = txt[(cdf$start.row[row]+1):(cdf$end.row[row]-1)]
  ca = parse(text=code)
  eval(ca,env)
}  
 
set.tourn.file = function(tourn.file, sr = get.sr()) {
  restore.point("set.tourn.file")
  sr$tourn.file = tourn.file
  tourn = load.tournament(sr$tourn.file)
  set.tourn.data(tourn, used.strats = NULL)

  load.round.data(tourn$rs.file)

}

compile.report = function(rep=sr$report,session, sr=get.sr()) {
  
  restore.point("compile.report")
  file = rep$file[1]
  txt = readLines(file)
  
  env = new.env(parent=sr)
  eval.custom.parameters(txt, env)

  # remove init_data chunk
  cdf = find.rmd.chunks(txt)
  rows = which(cdf$chunk.name=="init_data")
  remove.rows = NULL
  if (length(rows)>0) {
    row = rows[1]
    remove.rows = cdf$start.row[row]:cdf$end.row[row] 
  }
  rows = which(cdf$chunk.name=="init_parameters")
  if (length(rows)>0) {
    row = rows[1]
    remove.rows = c(remove.rows,cdf$start.row[row]:cdf$end.row[row])   
  }
  if (length(remove.rows)>0)
    txt = txt[-(remove.rows)]

   restore.point("compile.report.2")

  # Assign parameters from inputs
  if (!is.null(sr$ui.par.df)) {
    #browser()
    d = sr$ui.par.df
    i = 1
    input = session$input
    for (i in 1:NROW(d)) {
      val = isolate(input[[d$input.id[i]]])
      if (!is.null(val)) {
        val = as(val, d$class[i])
        cat("\nassign ",d$var[i]," =" ,val)
        assign(d$var[i],val, env)
      }
    }
  }
  restore.point("compile.report.3")


  
  # remove yaml description
  rows = which(txt=="---")
  if (length(rows)>=2) {
    txt = txt[-(rows[1]:rows[2])]
  }
  
  txt = c("```{r include=FALSE}\n library(StratTourn);set.view.mode('shiny_report')\n```",txt)
  html = knit2html(text=txt,fragment.only=TRUE, envir=env)
  html
}


load.round.data = function(file=tourn$rs.file, tourn=sr$tourn, sr=get.sr()) {
  restore.point("load.round.data")
# Data for each round
#file = "Tourn_Noisy_PD_20140721_202445_rs.csv"
  rd = fread(file)
  rd = as.tbl(as.data.frame(rd))
  rd = add.other.var(rd,c("strat","u"))
  sr$ard = rd
  sr$rd = rd
  
}
