example.global.reactive = function() {

  app = eventsApp()
  app$gr = reactiveValues(update=FALSE, msg="")
  
  
  app$ui = fluidPage(fluidRow(
    uiOutput("info"),
    textInput("globInput","Text for all"),
    textInput("sesInput","Text for session")
  ))
  
  appInitHandler(function(input, output, session,app,...) {
    app$key = runif(1)
    app$sr <- reactiveValues(msg="")
    observe({
      msg = paste0(app$gr$msg, "\n ",app$sr$msg,"\n ", app$key)
      setUI("info",HTML(msg))
    })
  })
  changeHandler("globInput", function(value, app,...) {
    app$gr$msg <- value  
  })
  changeHandler("sesInput", function(value, app,...) {
    app$sr$msg <- value 
  })
  
  runEventsApp(app)

}

