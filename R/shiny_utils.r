shiny.widgets.env = as.environment(list(widget.counter=list(), run.event.handlers=TRUE))


reset.event.handlers = function() {
  shiny.widgets.env$widget.counter=list()  
}

ace.editor.height = function(txt, font.size=12,min.height=30, min.lines=2,max.lines=NULL,extra.lines=1, code.lines=NULL) {
  restore.point("ace.editor.height")
  if (is.null(code.lines)) {
    code.lines = max(min.lines,NROW(sep.lines(txt)) + extra.lines)
  }
  if (!is.null(max.lines))
    code.lines = min(code.lines,max.lines)
  max((font.size * 1.1) * code.lines,min.height) 
}

ignore.false.events = function(dummy.id, server.env=parent.frame()) {
  ca = substitute(env=list(dummy.id=dummy.id),
     observe({
      cat("\n early input observer")
      input[[dummy.id]]
      shiny.widgets.env$run.event.handlers=FALSE
     }, priority=10000)
  )
  eval(ca, envir=server.env)
  ca = substitute(env=list(dummy.id=dummy.id),  
    observe({
      cat("\n late input observer")
      input[[dummy.id]]
      shiny.widgets.env$run.event.handlers = TRUE  
    }, priority=-10000)
  )
  eval(ca, envir=server.env)
}

input.event.handler = function(id, fun,...,server.env=parent.frame()) {
  #button.click.handler(wi$check.button,fun=stage_checkBtn.click)
  fun = substitute(fun)
  # Create dynamic observer
  args = list(...)
  ca = substitute(env=list(s_id=id, s_fun=fun,s_args=args),
    observe({
      display("called event handler for ",s_id)
      if (shiny.widgets.env$run.event.handlers) {
        display("run event handler for ",s_id)
        do.call(s_fun, c(list(id=s_id, value=input[[s_id]], session=session),s_args))
      }
    })
  )
  #restore.point("button.click.handler")  
  ca
  eval(ca, envir=server.env)
}

button.click.handler = function(id, fun,..., server.env=parent.frame()) {
  #button.click.handler(wi$check.button,fun=stage_checkBtn.click)
  fun = substitute(fun)
  # Create dynamic observer
  args = list(...)
  ca = substitute(env=list(s_id=id, s_fun=fun,s_args=args),
    observe({
      if (has.widget.counter.increased(s_id, input[[s_id]])) {
        display(s_id, " has been clicked...")
        do.call(s_fun, c(list(id=s_id, value=input[[s_id]], session=session),s_args))
      }
    })
  )
  restore.point("button.click.handler")  
  ca
  eval(ca, envir=server.env)
}

add.click.counter = function(button.id, counter.var, server.env=parent.frame()) {
  restore.point("add.click.counter")
  
  counter.var = as.symbol(counter.var)
  ca = substitute(env=list(counter.var = counter.var),
    counter.var <- reactiveValues(counter=0)
  )
  ca
  eval(ca, envir=server.env)
  
  ca = substitute(env=list(s_id=button.id, counter.var = counter.var),
    observe({
      if (has.widget.counter.increased(s_id, input[[s_id]])) {
        display(s_id, " has been clicked...")
        counter.var$counter = isolate(counter.var$counter+1)
      }
    })
  )
  ca
  eval(ca, envir=server.env)
}


has.widget.counter.increased = function(id, counter, env=shiny.widgets.env) {
  restore.point("has.widget.counter.increased")
  if (isTRUE(counter == 0) | is.null(counter) | isTRUE(counter<=env$widget.counter[[id]])) {
    env$widget.counter[[id]] = counter
    cat("\nno counter increase: ", id, " ",counter)
    return(FALSE)
  }
  env$widget.counter[[id]] = counter
  cat("\ncounter has increased: ", id, " ",counter)
  return(TRUE)  
}
