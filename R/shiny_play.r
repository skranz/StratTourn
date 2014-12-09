


action.input.names = function(i, game) {
  ai =   game$example.action(i=i)
  paste0("aiInput_",names(ai),"_",i)
}

make.player.ui = function(i=1, game) {
  obsOut = paste0("obsOut",i)
  
  ain = action.input.names(i=i, game=game)
  li = lapply(ain, function(name) {
    textInput(name)
  })
  aIn = do.call(fluidRow, li)
  
  buttonId = paste0("submit_",i)
  
  fluidRow(
    textOutput(obsOut),
    aIn,
    actionButton(buttonId, "submit")
  )
  
}