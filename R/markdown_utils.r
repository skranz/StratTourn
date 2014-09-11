find.rmd.chunks = function(txt) {
  restore.point("find.rmd.chunks")
  chunk.start = str.starts.with(txt,"```{r")
  chunk.end = which(str.starts.with(txt,"```") & !chunk.start)
  chunk.start = which(chunk.start)
  chunk.end = remove.verbatim.end.chunks(chunk.start,chunk.end)

  names = parse.chunk.names(txt[chunk.start])
  data.frame(chunk.name=names,start.row=chunk.start, end.row=chunk.end)
}


remove.verbatim.end.chunks = function(chunk.start, chunk.end) {
  restore.point("remove.verbatim.end.chunks")
  df = data.frame(ind =c(0, seq_along(chunk.start), seq_along(chunk.end)),
                  row=c(0, chunk.start,chunk.end),
                  type=c("f",
                         rep("s",length(chunk.start)),
                         rep("e",length(chunk.end))
                       )
                  )
  df = arrange(df,row)
  df$del =  df$type == "e" & !is.true(lag(df$type) == "s")
  
  keep.ind = df$ind[df$type=="e" & !df$del]
  chunk.end[keep.ind]
}

parse.chunk.names = function(header) {
  restore.point("parse.chunk.names")

  str = header
  res = rep("", length(header))
    
  str = str.between(str,"{r","}")
  rows = has.substr(str,",")
  str[rows] = str.left.of(str[rows],",")
  rows = !has.substr(str,"=")
  str[rows] = str.trim(str[rows])
  str =gsub("'","",str,fixed=TRUE)
  str =gsub('"',"",str,fixed=TRUE)
  str
}

chunk.opt.string.to.list = function(str, with.name=FALSE) {
  restore.point("chunk.opt.string.to.list")
  #str = "```{r 'out_chunk_2_1_b', fig.width=5, fig.height=5, eval=FALSE, echo=TRUE}"

  tokens = str.split(str,",")
  str = str.between(str,"{r","}")
  code = paste0("list(",str,")")
  li = eval(parse(text=code,srcfile=NULL))
  
  if (!with.name) {
    li = li[-1]
  }
  li
}

chunk.to.shiny.ggvis = function(code, chunk.opt, plot_id=paste0("ggvis_",sample.int(1e9,1)), envir=parent.frame(),session) {
  #restore.point("chunk.to.shiny.ggvis")
  #browser()
  if (is.character(code)) {
    code = parse(text=code)
  }
  
  local.env = new.env(parent=envir)
  gg = eval(code, local.env)
  bind_shiny(gg, plot_id,session=session)
  
  list(output=ggvisOutput(plot_id=plot_id), gg=gg)
}
