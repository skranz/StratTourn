shiny.test.simple.ggvis = function() {
  library(restorepoint)
  library(shiny)
  library(knitr)
  library(ggvis)
 # Simplest possible app:
library(shiny)
runApp(list(
  ui = bootstrapPage(
    uiOutput("fig")
  ),
  server = function(input,output, session) {
    
    output$fig = renderUI({
      gg = mtcars %>%
        ggvis(~wt, ~mpg) %>%
        layer_points(fill:="red") %>%
        bind_shiny("p")
      ggvisOutput("p")
    })
  }
))
}

shiny.test.ggvis = function() {
  library(restorepoint)
  library(shiny)
  library(knitr)
  library(stringtools)
  library(xtable)
  library(shinyBS)
  library(ggvis)
  
  set.restore.point.options(display.restore.point=!TRUE)
  setwd("D:/libraries/StratTourn")
  set.view.mode("shiny_report")
  tourn = load.tournament("Tourn_Noisy_PD_20140721_202445.Rdata")
  set.tourn.data(tourn)
  
  rmd.file = 'D:/libraries/StratTourn/StratTourn/reports/matches_duels_plot.rmd'
  txt = readLines(rmd.file)
  cdf = find.rmd.chunks(txt)
  opts = lapply(txt[cdf$start.row], chunk.opt.string.to.list, with.name=TRUE)
  names(opts) = cdf$chunk.name
  
  code = txt[(cdf$start.row[2]+1):(cdf$end.row[2]-1)]
  opt = opts[[2]]
  
  ui = fluidPage(
    fluidRow(
      uiOutput("report")
    )  
  )
  server = function(input, output, session, ...) {
    ggli = chunk.to.shiny.ggvis(code,chunk.opt = opt, session=session)
    
    output$report = renderUI({
      fluidRow(
        HTML("<p>GGVIS:</p>"),
        ggli$output
      )
    })
    
  }  
  runApp(list(ui=ui,server=server))
}