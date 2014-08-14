view= function (x, ...) {
  UseMethod("view")
}

view.default = function(x,...) {
  print(x)
}

view.data.frame = function(x,..., knitr=!interactive(), digits=NULL, scientific=FALSE) {
  if (knitr) {
    print(xtable(x,...), type = "html",
          html.table.attributes="class='table-bordered 'table-condensed'"
          ,...)
    #return(paste(capture.output()    
  } else {
    print(x, digits=digits)
  }
  return(invisible(x))
}
