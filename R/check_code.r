examples.parse.user.strats = function() {

  code = "
  # The famous tit.for.tat strategy: winner of Axelrod's original tournament
  # But Axelrod's tournament had no noise in observations....
  tit.for.tat = function(obs,i,t,...) {
    # Cooperate in the first period
    if (t==1)
      return(list(a='C'))
    writeLines('Hello')
    # In later periods, return the other players previous (observed) action
    j = 3-i
    list(a=obs$a[j])
  }
  #x = 5
  "
  parse.user.strats(code)
  
}

#' Checks whether a given strategy passes a whitelist
whitelist.check.strat = function(strat) {
  # Make cobined call of function header and body
  header = formals(strat)
  names(header) = NULL
  call = call("{", body(strat))
  call[(length(call)+1):(length(call)+length(header))] = header
  wl.funs = strat.whitelist.funs()
  check.whitelist(call, wl.funs = wl.funs, bl.vars=c(".GlobalEnv",".BaseNamespaceEnv"))    
}

parse.user.strats = function(code,  expr = try(parse(text=code))) {
  restore.point("parse.user.strats")
  
  if (inherits(expr, "try-error")) {
    msg = paste0("Error when parsing your code:\n",as.character(expr))
    return(list(ok=FALSE, msg=msg))
  } 

  i = 1
  if (length(expr)==0) {
    return(list(ok=FALSE, msg="Your code must create at least one stratey as a function."))
  }
  wl.funs = strat.whitelist.funs()
  for (i in seq_along(expr)) {
    call = expr[[i]]
    op = as.character(call[[1]])
    msg = paste0("Your code is only allowed to create strategies as functions. The following expression is not allowed:\n", deparse1(call))
    if ( (!op %in% c("=","<-")) | length(call)<3) {
      return(list(ok=FALSE, msg=msg))
    }
    if (!is.symbol(call[[2]])) {
      return(list(ok=FALSE, msg=msg))
    }
    op = try(as.character(call[[3]][[1]]), silent=TRUE)
    if (!identical(op,"function")) {
      return(list(ok=FALSE, msg=msg))
    }
    ret = check.whitelist(call, wl.funs = wl.funs, bl.vars=c(".GlobalEnv",".BaseNamespaceEnv"))
    if (!ret$ok) return(ret)
  }
  
  # Check that functions use no global variables
  env = new.env(parent=globalenv())
  res = try(eval(expr,env))
  if (inherits(res, "try-error")) {
    msg = paste0("Error when evaluating your code:\n",as.character(res))
    return(list(ok=FALSE, msg=msg))
  } 
  strats = get.functions(env)
  if (length(strats)==0) {
    msg = paste0("Your code generates no strategy")
    return(list(ok=FALSE, msg=msg))
  }

  
  strats = get.functions(env)
  
  for (i in seq_along(strats)) {
    fun = strats[[i]]
    name = names(strats)[i]
    glob = find.global.vars(fun)
    if (length(glob)>0) {
      msg = paste0("Your strategy ",name," uses the global variable(s) ", paste0(glob, collapse=", "),". Only local variables are allowed in functions.")
      return(list(ok=FALSE, msg=msg))
    }
  }
  return(list(ok=TRUE, msg="",funs=strats))
}

strat.whitelist.funs = function() {
  c(
"restore.point","debug.store",
"myfun","myfun1","myfun2","myfun3","myfun4","myfun5","myfun6","myfun7","myfun8",
        
"-","!","!=","$","$<-","%%","%*%","%/%","%in%","%o%","%x%","&","&&","(","*","/",":","[","[[","[[<-","[<-","^","{","|","||","~","+","<","<-","<=","=","==",">",">=","abbreviate","abs","acos","acosh","addNA","agrep","agrepl","alist","all","all.equal","all.names","all.vars","any","anyDuplicated","anyNA","aperm","append","apply","array","arrayInd","as.array","as.character","as.complex","as.data.frame","as.Date","as.difftime","as.double","as.factor","as.integer","as.list","as.logical","as.matrix","as.null","as.numeric","as.ordered","as.POSIXct","as.POSIXlt","as.qr","as.raw","as.single","as.symbol","as.table","as.vector","asin","asinh","atan","atan2","atanh","backsolve","besselI","besselJ","besselK","besselY","beta","break","by","c","casefold","cat","cbind","ceiling","char.expand","character","charmatch","chol","chol2inv","choose","class","col","colMeans","colnames","colnames<-","colSums","complex","Conj","cos","cosh","cospi","crossprod","cummax","cummin","cumprod","cumsum","cut","data.class","data.frame","data.matrix","date","default.stringsAsFactors","det","determinant","diag","diag<-","diff","difftime","digamma","dim","dim<-","dimnames","double","drop","droplevels","duplicated","eigen","exp","expand.grid","expm1","F","factor","factorial","Filter","Find","findInterval","floor","for","format","forwardsolve","function","gamma","gl","gregexpr","grep","grepl","grepRaw","gsub","I","identical","identity","if","ifelse","Im","integer","interaction","interactive","intersect","invisible","is.array","is.atomic","is.call","is.character","is.complex","is.data.frame","is.double","is.element","is.environment","is.expression","is.factor","is.finite","is.function","is.infinite","is.integer","is.language","is.list","is.loaded","is.logical","is.matrix","is.na","is.na.data.frame","is.na.numeric_version","is.na.POSIXlt","is.na<-","is.na<-.default","is.na<-.factor","is.na<-.numeric_version","is.name","is.nan","is.null","is.numeric","is.numeric.Date","is.numeric.difftime","is.numeric.POSIXt","is.numeric_version","is.object","is.ordered","is.primitive","is.qr","is.R","is.raw","is.recursive","is.single","is.symbol","is.table","is.unsorted","is.vector","ISOdate","ISOdatetime","isSymmetric","isSymmetric.matrix","isTRUE","jitter","julian","kappa","kronecker","labels","lapply","lbeta","lchoose","length","length<-","length<-.factor","lengths","letters","LETTERS","levels","levels.default","levels<-","levels<-.factor","lfactorial","lgamma","list","log","log10","log1p","log2","logb","logical","lower.tri","ls","make.names","make.unique","Map","mapply","margin.table","mat.or.vec","match","Math.data.frame","Math.Date","Math.difftime","Math.factor","Math.POSIXt","matrix","max","max.col","mean","merge","message","min","missing","Mod","month.abb","month.name","months","months.Date","months.POSIXt","names","names<-","nchar","ncol","NCOL","Negate","nlevels","norm","nrow","NROW","numeric","nzchar","order","ordered","outer","paste","paste0","pi","pmatch","pmax","pmax.int","pmin","pmin.int","polyroot","pretty","pretty.default","prettyNum","print","prmatrix","prod","prop.table","psigamma","q","qr","quarters","range","rank","rapply","rbind","rcond","Re","Recall","Reduce","regexec","regexpr","regmatches","regmatches<-","rep","rep.int","rep_len","repeat","replace","replicate","return","rev","rev.default","rle","round","row","row.names","row.names.data.frame","row.names.default","row.names<-","row.names<-.data.frame","row.names<-.default","rowMeans","rownames","rownames<-","rowsum","rowsum.data.frame","rowsum.default","rowSums","sample","sample.int","sapply","scale","scale.default","seq","seq.Date","seq.default","seq.int","seq.POSIXt","seq_along","seq_len","sequence","setdiff","setequal","sign","simplify2array","sin","single","sinh","sinpi","slice.index","solve","sort","split","sprintf","sqrt","strftime","strptime","strsplit","strtoi","strtrim","structure","strwrap","sub","subset","substr","substr<-","substring","substring<-","sum","summary","svd","sweep","switch","t","T","t.data.frame","table","tabulate","tan","tanh","tanpi","tapply","tcrossprod","tolower","toString","toString.default","toupper","transform","trigamma","trimws","trunc","typeof","union","unique","units","units<-","unlist","unname","unsplit","untrace","upper.tri","vapply","vector","Vectorize","weekdays","weekdays.Date","weekdays.POSIXt","which","which.max","which.min","while","with","with.default","within","within.data.frame","within.list","xor","xtfrm","aggregate","aggregate.data.frame","aggregate.ts","AIC","alias","anova","ansari.test","aov","approx","approxfun","ar","ar.burg","ar.mle","ar.ols","ar.yw","arima","arima.sim","arima0","arima0.diag","ARMAacf","ARMAtoMA","as.dendrogram","as.dist","as.formula","as.hclust","as.stepfun","as.ts","asOneSidedFormula","ave","bandwidth.kernel","bartlett.test","BIC","binom.test","binomial","biplot","Box.test","bw.bcv","bw.nrd","bw.nrd0","bw.SJ","bw.ucv","C","cancor","case.names","ccf","chisq.test","cmdscale","coef","coefficients","complete.cases","confint","confint.default","confint.lm","constrOptim","contr.helmert","contr.poly","contr.SAS","contr.sum","contr.treatment","contrasts","contrasts<-","convolve","cooks.distance","cophenetic","cor","cor.test","cov","cov.wt","cov2cor","covratio","cpgram","cutree","cycle","D","dbeta","dbinom","dcauchy","dchisq","decompose","delete.response","deltat","dendrapply","density","density.default","deriv","deriv3","deviance","dexp","df","df.kernel","df.residual","dfbeta","dfbetas","dffits","dgamma","dgeom","dhyper","diffinv","dist","dlnorm","dlogis","dmultinom","dnbinom","dnorm","dpois","drop.scope","drop.terms","drop1","dsignrank","dt","dummy.coef","dummy.coef.lm","dunif","dweibull","dwilcox","ecdf","eff.aovlist","effects","embed","end","estVar","expand.model.frame","extractAIC","factanal","factor.scope","family","fft","filter","fisher.test","fitted","fitted.values","fivenum","fligner.test","formula","frequency","friedman.test","ftable","Gamma","gaussian","get_all_vars","getCall","getInitial","glm","glm.control","glm.fit","hasTsp","hat","hatvalues","hclust","heatmap","HoltWinters","influence","influence.measures","integrate","interaction.plot","inverse.gaussian","IQR","is.empty.model","is.leaf","is.mts","is.stepfun","is.ts","is.tskernel","isoreg","KalmanForecast","KalmanLike","KalmanRun","KalmanSmooth","kernapply","kernel","kmeans","knots","kruskal.test","ks.test","ksmooth","lag","lag.plot","line","lm","lm.fit","lm.influence","lm.wfit","loadings","loess","loess.control","loess.smooth","logLik","loglin","lowess","ls.diag","ls.print","lsfit","mad","mahalanobis","make.link","makeARIMA","makepredictcall","manova","mantelhaen.test","mauchly.test","mcnemar.test","median","median.default","medpolish","model.extract","model.frame","model.frame.default","model.matrix","model.matrix.default","model.matrix.lm","model.offset","model.response","model.tables","model.weights","monthplot","mood.test","mvfft","na.action","na.contiguous","na.exclude","na.fail","na.omit","na.pass","napredict","naprint","naresid","nextn","nlm","nlminb","nls","nls.control","NLSstAsymptotic","NLSstClosestX","NLSstLfAsymptote","NLSstRtAsymptote","nobs","numericDeriv","offset","oneway.test","optim","optimHess","optimise","optimize","order.dendrogram","p.adjust","p.adjust.methods","pacf","pairwise.prop.test","pairwise.t.test","pairwise.table","pairwise.wilcox.test","pbeta","pbinom","pbirthday","pcauchy","pchisq","pexp","pf","pgamma","pgeom","phyper","plclust","plnorm","plogis","plot.ecdf","plot.spec.coherency","plot.spec.phase","plot.stepfun","plot.ts","pnbinom","pnorm","poisson","poisson.test","poly","polym","power","power.anova.test","power.prop.test","power.t.test","PP.test","ppoints","ppois","ppr","prcomp","predict","predict.glm","predict.lm","preplot","princomp","printCoefmat","profile","proj","promax","prop.test","prop.trend.test","psignrank","pt","ptukey","punif","pweibull","pwilcox","qbeta","qbinom","qbirthday","qcauchy","qchisq","qexp","qf","qgamma","qgeom","qhyper","qlnorm","qlogis","qnbinom","qnorm","qpois","qqline","qqnorm","qqplot","qsignrank","qt","qtukey","quade.test","quantile","quasi","quasibinomial","quasipoisson","qunif","qweibull","qwilcox","rbeta","rbinom","rcauchy","rchisq","rect.hclust","reformulate","relevel","reorder","replications","reshape","resid","residuals","residuals.glm","residuals.lm","rexp","rf","rgamma","rgeom","rhyper","rlnorm","rlogis","rmultinom","rnbinom","rnorm","rpois","rsignrank","rstandard","rstudent","rt","runif","runmed","rweibull","rwilcox","rWishart","sd","se.contrast","selfStart","setNames","shapiro.test","simulate","smooth","smooth.spline","smoothEnds","sortedXyData","spec.ar","spec.pgram","spec.taper","spectrum","spline","splinefun","splinefunH","SSasymp","SSasympOff","SSasympOrig","SSbiexp","SSD","SSfol","SSfpl","SSgompertz","SSlogis","SSmicmen","SSweibull","start","stat.anova","step","stepfun","stl","StructTS","summary.aov","summary.glm","summary.lm","summary.manova","summary.stepfun","supsmu","symnum","t.test","termplot","terms","terms.formula","time","toeplitz","ts","ts.intersect","ts.plot","ts.union","tsdiag","tsp","tsp<-","tsSmooth","TukeyHSD","uniroot","update","update.default","update.formula","var","var.test","variable.names","varimax","vcov","weighted.mean","weighted.residuals","weights","wilcox.test","xtabs","%.%","%>%","add_rownames","anti_join","arrange","as.tbl","as.tbl_cube","as_data_frame","base_agg","base_scalar","base_win","between","bind_cols","bind_rows","chain","changes","collapse","count","cumall","cumany","cume_dist","cummean","data_frame","dense_rank","desc","dim_desc","distinct","do","escape","explain","filter","first","full_join","funs","glimpse","group_by","group_indices","group_size","grouped_df","grouped_dt","groups","id","ident","inner_join","intersect","is.grouped_df","is.grouped_dt","is.tbl","lag","last","lead","left_join","min_rank","mutate","mutate_each","n","n_distinct","n_groups","nth","ntile","order_by","percent_rank","rbind_all","rbind_list","regroup","rename","rename_vars","right_join","row_number","rowwise","sample_frac","sample_n","select","select_vars","semi_join","setdiff","setequal","slice","summarise","summarise_each","summarize","tally","tbl","tbl_cube","tbl_df","tbl_dt","tbl_sql","tbl_vars","top_n","transmute","trunc_mat","type_sum","ungroup","union","with_order")
}
