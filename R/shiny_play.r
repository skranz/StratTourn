examples.shiny.play = function() {
  
  #Load package
  library(StratTourn)
  restore.point.options(display.restore.point = TRUE)
  
  # Generate a game object
  game = make.pd.game()

  # Pick a pair of strategies
  strat = nlist(tit.for.tat,tit.for.tat)

  app = eventsApp()
  panels = lapply(1:game$n, function(i) {
    id = paste0("actionFormUI",i)
    tabPanel(title=paste0("Player ",i),uiOutput(id))
  })
  app$ui = fluidPage(
    do.call(tabsetPanel,panels)
  )
  
  
  rr = play.human.against.strat(human.i=1,game=game,T = 2,strat=strat)
  runEventsApp(app, launch.browser = rstudio::viewer)

}

human.strat.skeleton = function(i=i, game=game) {
  function(obs,i,t,...) {
    game$example.action(i=i, t=t, obs=obs)
  }
}

play.human.against.strat = function(human.i=1,game,strat, T=NULL, parent.ui=paste0("actionFormUI",1:game$n)) {
  restore.point("play.human.against.strat")
  
  
  if (!is.list(strat)) stop("strat should be a list.")
  if (length(strat)<game$n) for (i in 2:game$n) strat[i] = strat[1]
  
  strat.skel = lapply(human.i, human.strat.skeleton, game=game)
  strat[human.i] = strat.skel
  names(strat)[human.i] = paste0("human_",human.i)
  
  rr = init.rep.game.run(game=game, strat=strat, T=T)
  rr$strat.skel = strat.skel
  rr$human.i = human.i
  rr$computer.i = setdiff(1:game$n, rr$human.i)
  rr$action.inputs = vector("list",game$n)
  rr$parent.ui = parent.ui
  
  
  
  rr = start.period(t=1,rr=rr)
  rr
}

start.period = function(t=rr$t, rr, app=getApp()) {
  restore.point("start.period")
  
  rr$t = t
  rr$action.done = rep(FALSE, game$n)
  for (i in rr$computer.i) {
    rr = get.period.action(i=i,t=t,rr=rr)
  }
  rr$action.done[rr$computer.i] = TRUE
  for (i in rr$human.i) {
    period.show.human.ui(i=i,t=t,rr=rr)    
  }
  rr
}

finish.period = function(t=rr$t,rr) {
  restore.point("finish.period")
  rr$t = t
  rr = get.period.results(t=t, rr=rr)
  if (t < rr$T) {
    t = t+1
    start.period(t=t, rr=rr)
  } else {
    rr$t = rr$T
    finish.game(rr=rr)
  }
}

finish.game = function(rr,...) {
  restore.point("finish.game")

  li = compute.rep.game.stats(rr)
  res = select(li$res, i, strat, u)
  for (i in rr$human.i) {
    act.i = i
    rs = li$rs %>%
      filter(i==act.i) %>%
      select(t,i,strat,u,a,starts_with("obs"))
    
    ui = fluidRow(column(offset=1, width=10,
      h5("Payoffs:"),
      HTML(html.table(res)),
      br(),
      h5("Rounds:"),
      HTML(html.table(rs))
    ))

    
    parent.ui=rr$parent.ui[[i]]
    setUI(parent.ui,ui)
  }
  
}


period.show.human.ui = function(i,t=rr$t, rr, parent.ui=rr$parent.ui[[i]]) {
  restore.point("period.show.human.ui")

  rr$t = t
  rr$i = i
  rr$action.input.info[[i]] = action.input.info(i=i,t=t,rr=rr)

  if (game$private.signals) {
    obs = rr$obs[[i]]
  } else {
    obs = rr$obs
  }

  obs.df = as.data.frame(as.list(unlist(obs)))
  obs.html = html.table(obs.df)
  
  aii = rr$action.input.info[[i]]
  ids = unlist(aii$id.li)
  input.li = lapply(seq_along(ids), function(ind) {
    textInput(ids[[ind]],label=names(ids)[[ind]])
  })

  buttonId = paste0("submitBtn_",i)
  infoId = paste0("actionInfoUI_",i)
  ui = fluidRow(column(offset=1, width=10,
    h4(paste0(game$name, " period ", t, " player ", i)),
    h5("Your observation:"),
    HTML(obs.html),
    h5("Your action:"),
    input.li,
    uiOutput(infoId),
    actionButton(buttonId, "Submit")
  ))
  #buttonHandler(buttonId, submit.human.action, i=i, t=t, rr=rr)
  app = getApp()
  app$rr = rr
  
  buttonHandler(buttonId, submit.human.action)
  setUI(infoId,p(paste0("Please choose your action for period ",t," and then press the Submit button.")))

  setUI(parent.ui, ui)

}


get.human.action = function(id.li) {
  restore.point("get.human.action")
  
  if (is.list(id.li) | length(id.li)>1) {
    res = lapply(id.li, get.human.action)
    if (!is.list(id.li)) res = unlist(res)
    return(res)
  }
  getInputValue(id.li)
}


recursive.action.input.id = function(x, parent.name = "") {
  if (is.list(x) | length(x)>1) {
    if (is.null(names(x))) names(x) = paste0("el",seq_along(x))
    res = lapply(names(x), function(name) {
      recursive.action.input.id(x=x[[name]], parent.name = paste0(parent.name,"__",name))
    })
    names(res) = names(x)
    if (!is.list(x)) {
      res = unlist(r)
    }
    return(res)
  }
  return(parent.name)
}

action.input.info = function(i,t=rr$t, game=rr$game, rr) {
  restore.point("action.input.info")
  
  ai  = game$example.action(i=i,t=t)
  set = game$action.set(i=i,t=t)
  
  id.li = recursive.action.input.id(x=ai, paste0("actionInput_",i))
  list(id.li=id.li)

}


submit.human.action = function(i=rr$i,t=rr$t,rr=app$rr, app=getApp(),...) {
  restore.point("submit.human.action")

  if (rr$action.done[i]) {
    setUI(infoId,p("You have already submitted an action. You cannot change it anymore. Please wait until other players are finished."))
    return()
  }

  
  infoId = paste0("actionInfoUI_",i)
  aii = rr$action.input.info[[i]]
  ai = get.human.action(aii$id.li) 
  
  restore.point("submit.human.action.inner")
  
  game = rr$game
  ok = try(rr$game$check.action(ai=ai,i=i,t=t), silent=TRUE)
  
  if (is(ok,"try-error")) {
    msg = paste0("You have not entered valid actions:\n", as.character(ok))
    setUI(infoId,p(msg))
    return()
  }
  
  rr$strat[[i]] = function(obs,i,t,...) {
    return(ai)
  }
  rr = get.period.action(i=i,t=t,rr=rr)
  
  rr$action.done[i] = TRUE
  setUI(infoId,p("Your action has been submitted. Please wait until other players are finished."))
  
  if (all(rr$action.done)) {
    finish.period(t=t,rr=rr)
  }
  
}


compute.rep.game.stats = function(rr, T = rr$T) {
  restore.point("compute.rep.game.stats")
  
  temp.T = T
  copy.into.env(source=rr)
  T = temp.T
  
  u = U/T

  # Round stats
  round.stats = rbindlist(round.stats.li)
  rs.dt = data.table(match.id=match.id, strat=rep(names(strat), times=T), round.stats)
  
  if (detailed.return) {
    strat.state.str = unlist(strat.state.str.li)
    rs.dt$strat.state = strat.state.str
  }
  
  
  # action statistics
  #as.df = do.call("rbind",lapply(action.stats, unlist)) / T
  # returned data frame
  res.dt = data.table(match.id = match.id,i=1:game$n, T=T,u.weight=T*T.weight, strat=names(strat), u=u)

  if (detailed.return) {
    return(list(rs=rs.dt,res=res.dt))
  }
  return(list(rs=rs.dt,res=res.dt))

}

init.rep.game.run = function(delta=game$delta, game, strat, T.max = NULL,detailed.return = TRUE, strat.seed=NULL, game.seed = NULL, do.store = TRUE,strat.par=NULL, sample.delta = delta, match.id = sample.int(2147483647,1), T=game$T) {
  
  restore.point("init.rep.game.run")
  
  gbos$do.store = do.store
   
  if (is.null(game.seed))
    game.seed = draw.seed()
  if (is.null(strat.seed))
    strat.seed = draw.seed()
  
  c(game.seed,strat.seed)
  
  #Store seeds to replicate for debugging purposes
  gbos$game.seed = game.seed
  gbos$strat.seed = strat.seed
  
  init.random.state("game",game.seed)
  init.random.state("strat",strat.seed)
  set.random.state("game")
  
  # Draw number of periods from a negative binominal distribution
  if (is.null(T)) {
    ret = sample.T(delta=delta, sample.delta=sample.delta)
    T = ret$T
    T.weight = ret$T.weight
    if (!is.null(T.max))
      T = pmin(T, T.max)
  } else {
    T.weight = T
  }
  
  strat.id = seq_along(strat)
  n.strat = length(strat.id)
  
  if (n.strat != game$n) {
    stop(paste("The game requires ",game$n, " players but you submitted ", n.strat, " strategies."))
  }
  
  if (detailed.return) {
    strat.state.str.li = vector("list",T)
  }
  
  if (!is.null(game$initial.game.states)) {
    game.states = game$initial.game.states()
  } else {
    game.states = NULL
  }

  if (game$private.signals) {
    obs = lapply(1:game$n, game$example.obs, game.states=game.states)
  } else {
    obs = game$example.obs(i=1,game.states=game.states)
  }
  
  denv = new.env()
  a = vector("list",n.strat)
  
  
  # Strategy infos
  si = lapply(strat.id, function(i) {
    get.strat.info(i=i, strat=strat[[i]], game=game, game.states=game.states)
  })
  strat.states = lapply(strat.id, function(i) {
    list()
  })
  
  round.stats.li = vector("list",T)
  U = rep(0,game$n)
  t = 1
  
  rr = new.env()
  copy.into.env(dest=rr,exclude="rr")
  rr
}

get.period.action = function(i=1,t=rr$t, rr) {
  restore.point("get.period.action")
  rr$t = t
  rr$i = i
  copy.into.env(source=rr)
  
  obs.i = get.obs.i(obs = obs, i = i, game = game)    
  # Use only those strat.par that are not returned as a strat.state
  act.strat.par = strat.par[[i]][setdiff(names(strat.par[[i]]),names(strat.states[[i]]))]
  args = c(list(obs = obs.i,i=i,t=t),game$show.par,game.states,strat.states[[i]], act.strat.par)
  restore.point("within.run.rep.game.ii")
  tryCatch({ 
    strat.res <- do.call(strat[[i]],args)
    #Has action parameter been returned?
    if(!(si[[i]]$actions %in% names(strat.res))){
      e <- paste0("\nAction parameter ",si[[i]]$actions, " has to be a part of the return value!\n")
      stop(e)
    }
    #Have all idiosyncratic parameters been returned?
    if(!all(si[[i]]$strat.states %in% names(strat.res))){
      missing.strat.states <- si[[i]]$strat.states[!(si[[i]]$strat.states %in% names(strat.res))]
      if(length(missing.strat.states)==1){
        e <- paste0("\nStrat state ",missing.strat.states, " has to be a part of the return value!\n")
      } else {
        missing.strat.states <- paste0(missing.strat.states,collapse=", ")
        e <- paste0("\nStrat states ",missing.strat.states, " have to be a part of the return value!\n")
      }
      stop(e)
    }
    #Have there been more parameters as what has been expected?
    #This might e.g. be the case, if the return statement of the first period does not specify all parameters.
    #parameters handeled in this way are always initialized as the default value, so an error is necessary
    if(!all(si[[i]]$strat.par %in% names(strat.res))){
      missing.strat.states <- si[[i]]$strat.par[!(si[[i]]$strat.par %in% names(strat.res))]
      if(length(missing.strat.states)==1){
        e <- paste0("\nStrat state ",missing.strat.states, " has to be a part of the return value!\n")
      } else {
        missing.strat.states <- paste0(missing.strat.states,collapse=", ")
        e <- paste0("\nStrat states ",missing.strat.states, " have to be a part of the return value!\n")
      }
      stop(e)
    }
  },
    error = function(e) {
      message("Error in evaluating strategy ", names(strat)[i], " in period t=", t, " for player i=",i,"\nERROR.HIST:")
      hist = denv$df[1:t,]
      
      print(tail(hist))
      assign(".BOS.ERROR.HIST",hist,.GlobalEnv)
      assign(".BOS.ERROR.t",t,.GlobalEnv)
      assign(".BOS.ERROR.i",i,.GlobalEnv)
      
      err = paste0(" evaluating strategy ", names(strat)[i], " in period t=", t, " for player i=",i,": ", str.right.of(as.character(e),":"))
      stop(err, call.=FALSE)
    }
  )
  a[[i]] = strat.res[si[[i]]$actions]  
  strat.states[[i]] = strat.res[si[[i]]$strat.states]
  game$check.action(ai=a[[i]],i=i,t=t)
  
  copy.into.env(dest=rr, exclude=c("rr","..."))
  rr
}

get.period.results = function(t=rr$t, rr) {
  rr$t = t
  copy.into.env(source=rr)
  restore.point("get.period.results")
  
  
  # 2. Evaluate game results
  set.random.state("game")  
  
  results = game$run.stage.game(t=t,a=a,t.obs=obs,game.states=game.states,game=game)
  
  if (detailed.return) {
    strat.state.str.li[[t]] = sapply(strat.states,list.to.str, use.names=TRUE) 
  }
  round.stats.li[[t]] = results$round.stats

  old.obs = obs
  obs = results$obs
  game.states = results$game.states
  
  # 3. Update total payoffs
  U = U + results$payoff
    
  copy.into.env(dest=rr, exclude="rr")
  rr
}
