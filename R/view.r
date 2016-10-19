.view.env = new.env()

view= function (x, ...) {
  UseMethod("view")
}

view.default = function(x,...) {
  if (!is.null(dim(x))) {
    return(view.data.frame(as.data.frame(x),...))
  }

  print(x)
}

set.view.mode = function(mode) {
  .view.env$mode=mode
}

# "console", "html", "shiny"
get.view.mode = function() {
  if (is.null(.view.env$mode))
    .view.env$mode=ifelse(interactive(),"console","html")
  .view.env$mode
}

get.view.style = function(mode) {
  NULL
}

view.data.table = function(x,...) {
  view.data.frame(as.data.frame(x),...)
}

view.data.frame = function(x,...,mode=get.view.mode(),style=get.view.style(mode), digits=NULL, scientific=FALSE, rownames.shiny=FALSE) {
  restore.point("view.data.frame")
  if (mode=="html") {
    library(xtable)
    txt = capture.output(print(xtable(x,...), type = "html",
          html.table.attributes="class='table-bordered 'table-condensed'"
          ,...))
    cat(txt)
    
    #return(paste(capture.output()))
    # x = as.data.frame(x)
    # txt = kable(x, format="html",...)
    # return(txt)
  } else if (mode=="shiny_report") {
    library(shiny)
    cat(renderTable(x, rownames = rownames.shiny)())      
  } else if (mode=="shiny") {
    library(shiny)
    return(renderTable(x, rownames = rownames.shiny))  
  } else {
    print(x, digits=digits)
  }
  return(invisible(x))
}
