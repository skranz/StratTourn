

examples.show.tournament = function() {
  run.shiny.reports()
  show.tournament(tourn)

  setwd("D:/libraries/StratTourn/studies")
  tourn.file = "Tourn_Noisy_PD_20141013_093419.tou"
  show.tournament(tourn.file=tourn.file)
  
}

sr = new.env(parent=globalenv())

get.sr = function() {
  sr
}

#' Analyse a tournament interactively in web browser
show.tournament = function(tourn=NULL, tourn.file=NULL, launch.browser=TRUE, file.path=getwd(), strat.shares=NULL) {
  
  restore.point("show.tournament")
  sr = get.sr()
  
  if (is.null(strat.shares)) {
    set.tourn.file(tourn=tourn, tourn.file=tourn.file)
    strats = names(sr$tourn$strat)

    sr$strat.sizes = rep(1,length(strats))
    names(sr$strat.sizes) = strats
  } else {
    strats = names(strat.shares)
    strat.shares = strat.shares[strats]
    sr$strat.sizes = round(strat.shares*100,1)
    sizes.str = paste0(names(sr$strat.sizes),"=", sr$strat.sizes, collapse="\n")
    
    set.tourn.file(tourn=tourn, tourn.file=tourn.file, sizes.str=sizes.str)
    strats = names(sr$tourn$strat)
  }
  sr$used.strats = strats
  
  sr$rep.li = make.rep.li()
  sr$report = sr$rep.li[[1]]
  
  reset.event.handlers()
  
  ui = make.report.ui()
  server = make.report.server()
  
  runApp(list(ui=ui,server=server), launch.browser=launch.browser)  
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
  library(shinyAce)
  set.restore.point.options(display.restore.point=TRUE)
  setwd("D:/libraries/StratTourn/studies")
  set.view.mode("shiny_report")
  sr = get.sr()
  sr$file.path = getwd()
  sr$tourn.file = "Tourn_Noisy_PD_20140912_094618_1.tou"
  sr$tourn.file = "Tourn_Noisy_PD_20140913_114354_1.tou"
  set.tourn.file(sr$tourn.file)
  
  strats = names(sr$tourn$strat)
  sr$strat.sizes = rep(1,length(strats))
  names(sr$strat.sizes) = strats
  
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
  file: matches_ranking.rmd
evolution:
  label: evolution
  file: evolution.rmd
diag_payoffs_over_time:
  label: against itself
  file: diag_payoffs_over_time.rmd
payoffs_over_time:
  label: payoff over time
  file: payoffs_over_time.rmd
duels_over_time:
  label: duels over time
  file: duels_over_time.rmd
duels_diff_over_time:
  label: duels diff over time
  file: duels_diff_over_time.rmd
duel_stats:
  label: duel stats
  file: payoff_diff_ranking.rmd
payoff_matrix:
  label: payoff matrix
  file: matches_payoff_matrix.rmd
duels_plot:
  label: duels plot
  file: matches_duels_plot.rmd
strat_stats:
  label: strat stats
  file: strat_indicators.rmd
strategies:
  label: strategies
  file: show_strat_code.rmd
bargaining_game:
  label: bargaining game
  file: bargaining_game_over_time.rmd
bargaining_agreements:
  label: bargaining agreement
  file: bargaining_game_perc_agree.rmd
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
  sizes.str = paste0(names(sr$strat.sizes),"=", sr$strat.sizes, collapse="\n")
  ui = fluidPage("Analyse Tournament",
    sidebarLayout(
      sidebarPanel(
        tabsetPanel(id ="leftPanel",
          tabPanel("Reports",
            bsActionButton("update_strat_btn","update", size="small"),
            selectizeInput("used_strats", label = "Used strategies:",
            choices = strats, selected = strats, multiple=TRUE,width="100%"),
            aceEditor("sizes_string", sizes.str, height = "50px", fontSize = 12, debounce = 10, wordWrap=TRUE,showLineNumbers = FALSE, highlightActiveLine = FALSE),
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
      #browser()
      update.report$counter
      html=compile.report(sr$report, session=session)
      cat("after compile.report")
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
    fluidRow(
      selectizeInput('tourn.file.input',"tournament",choices=files, selected=sr$tourn.file,multiple=FALSE, width="100%"),
      bsActionButton("load.tourn.btn","load", size="small")
    ),
    fluidRow(
      column(3,
        bsActionButton("run.tourn.btn","run", size="small")
      ),
      column(3,
        helpText("Rounds:")
      ),
      column(3,
        numericInput("rep.tourn.input",NULL,value = 10,min = 1)
      )
    ),
    htmlOutput("tourn.info.output")
  )
}

select.report.ui = function(rep.li=sr$rep.li, sr=get.sr()) {
  restore.point("select.report.ui")
  buttons = lapply(rep.li, function(rep) {
    bsActionButton(rep$button.id,rep$label, size="small")
  })
  names(buttons)=NULL
  do.call(fluidRow,buttons)
  #buttons[[1]]
}

set.tourn.data = function(tourn=sr$tourn, used.strats=sr$used.strats, sizes.string="", sr=get.sr(), set.round.data=TRUE) {
  restore.point("set.tourn.data")

  if (is.null(used.strats))
    used.strats = names(tourn$strat)
  
  strat.sizes = parse.sizes.string(str = sizes.string,used.strats = used.strats,to.shares = FALSE)
  shares = strat.sizes / sum(strat.sizes)

  # Data for each match
  md = tourn$dt
  md$share = shares[md$strat] 
  md = add.other.var(md,c("strat","u","share"))
  md$delta.u = md$u - md$other.u
    
  # Names of all strategies
  strats = unique(md$strat)
  amd = md
 
  if (!setequal(used.strats, strats)) {
    rows = md$strat %in% used.strats & md$other.strat %in% used.strats
    md = amd[rows,]
  }
 
  rank.dt = strat.rank.from.matches(md)
  
  copy.into.env(dest=sr,names = c("tourn","amd", "md","strats","used.strats","strat.sizes", "shares","rank.dt"))
  
  if (set.round.data)
    adapt.round.data()
}


click_run_tourn = function(session,update.report,update.tourn,...,sr=get.sr()) {
  tourn = sr$tourn
  R = isolate(session$input$rep.tourn.input)
  restore.point("click_run_tourn")
  
  do.store = is.storing()
  set.storing(FALSE)
  withProgress(session=session,min=0,max=R+1, expr={
    shiny::setProgress(message = "Running tournaments...",
    detail = paste0("Finished 0 / ", R))
    for (r in 1:R) {
      tourn = run.tournament(tourn,R = 1)
      shiny::setProgress(value=r,detail = paste0("Finished ",r," / ", R))
    }
    set.storing(do.store)
  
    sr$tourn = tourn
    shiny::setProgress(value=R+0.5,detail = paste0("Save tournament..."))
 
    save.tournament(tourn=tourn, path=sr$file.path)

  })
  
  set.tourn.data(tourn, used.strats = NULL,sizes.string = "", set.round.data=FALSE)
  load.round.data(tourn$rs.file)
  
  #click_load_tourn(session, update.report, update.tourn,...,sr=sr)
  
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

  strats = sr$strats
  sizes.string = paste0(names(sr$strat.sizes),"=", sr$strat.sizes, collapse="\n")
  updateAceEditor(session,"sizes_string", value=sizes.string)
  
  
  update.report$counter = isolate(update.report$counter+1)
  update.tourn$counter = isolate(update.tourn$counter+1)
  
}

click_report_btn = function(session,id,..., update.report,new.report, sr=get.sr()) {
  
  
  restore.point("click_report_btn")
  cat("I am in click_update_strat")
  #browser()
  id = str.left.of(id,"_report_btn")
  sr$report = sr$rep.li[[id]]
  used.strats = isolate(session$input$used_strats)
  sizes.string = isolate(session$input$sizes_string)

  #set.tourn.data()

  set.tourn.data(used.strats=used.strats, sizes.string = sizes.string)

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
  sizes.string = isolate(input$sizes_string)

  set.tourn.data(used.strats=used.strats, sizes.string = sizes.string)
  
  update.report$counter = isolate(update.report$counter+1)
  
}


make.ui.custom.parameters = function(session, sr=get.sr()) {  
  restore.point("make.ui.custom.parameters")
  rep = sr$report
  file = system.file(package="StratTourn", "reports",rep$file[1]) 
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
 
set.tourn.file = function(tourn.file=NULL, tourn=NULL, sr = get.sr(), file.path=getwd(), sizes.string="", set.data = TRUE) {
  restore.point("set.tourn.file")

  if (is.null(tourn.file))
    tourn.file = paste0(tourn$tourn.id,".tou")
  
  if (is.null(tourn))
    tourn = load.tournament(tourn.file)

  sr$file.path = file.path
  setwd(file.path)
  sr$tourn.file = tourn.file
  
  set.tourn.data(tourn, used.strats = NULL,sizes.string = sizes.string, set.round.data=FALSE)
  load.round.data(tourn$rs.file)
}

compile.report = function(rep=sr$report,session, sr=get.sr(), parameters.from.input=TRUE, fragment.only=TRUE) {
  
  restore.point("compile.report")
  file = system.file(package="StratTourn", "reports",rep$file[1]) 
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
  
  if (parameters.from.input) {
    rows = which(cdf$chunk.name=="init_parameters")
    if (length(rows)>0) {
      row = rows[1]
      remove.rows = c(remove.rows,cdf$start.row[row]:cdf$end.row[row])   
    }
  }
  if (length(remove.rows)>0)
    txt = txt[-(remove.rows)]

   restore.point("compile.report.2")

  # Assign parameters from inputs
  if (!is.null(sr$ui.par.df) & parameters.from.input) {
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
  html = knit2html(text=txt,fragment.only=fragment.only, envir=env)
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

  adapt.round.data()
}


adapt.round.data = function(tourn=sr$tourn, used.strats=sr$used.strats, strats=names(tourn$strat), shares=sr$shares, sr=get.sr()) {
  restore.point("adapt.round.data")
  
  if (is.null(used.strats))
    used.strats = strats
  if (!identical(used.strats,strats)) {
    rows = sr$ard$strat %in% used.strats & sr$ard$other.strat %in% used.strats
    sr$rd = sr$ard[rows,]
  } else {
    sr$rd = sr$ard
  }
}


parse.sizes.string = function(str, used.strats=sr$used.strats, sr = get.sr(), to.shares=TRUE) {
  restore.point("parse.sizes.string")
  sizes = rep(1,NROW(used.strats))
  names(sizes) = used.strats

  str = str.trim(sep.lines(str))
  str = str[nchar(str)>0]
  str = merge.lines(str, collapse=",")
  str = gsub(" ","",str,fixed=TRUE)
  str = gsub(",,",",",str,fixed=TRUE)
  
  vec = NULL
  try(vec <- eval(parse(text=paste0("c(",str,")"))))

  cols = intersect(names(sizes), names(vec))
  sizes[cols] = vec[cols]
  if (to.shares)
    sizes = sizes / sum(sizes)

  sizes
}


write.reports.html = function(report.file = "reports.html",reports = names(rep.li), rep.li = sr$rep.li, sr=get.sr()) {

  #reports = c("payoff_ranking","duel_stats")
  rep.name = reports[[1]]
  html = sapply(reports, function(rep.name) {
    restore.point("inner.write.reports.html")
    report = rep.li[[rep.name]]
    txt = compile.report(report,parameters.from.input=FALSE,sr=sr)
    paste0(txt, collapse="\n")
  })

  html = paste0(html, collapse = "\n\n")
  if (!is.null(report.file))
    writeLines(html, report.file)
  invisible(html)
}