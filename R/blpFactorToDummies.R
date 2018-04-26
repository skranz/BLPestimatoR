#' @importFrom stats as.formula
#' @importFrom stats model.matrix



# Helper function to convert factor variables in Xlin, Xrandom or Xexo
# to dummy variables.
# Also adapt the variable names in Xlin, Xrandom and Xexo
# 
BLP.factor.to.dummies = function(dat, Xlin, Xrandom, Xexo, starting.guess.theta2) {
  vars = unique(c(Xlin,Xrandom,Xexo))
  fvars = vars[sapply(dat[vars], function(vals) is.factor(vals) | is.character(vals))]

  if (length(fvars)==0) {
    return(list(dat=dat,Xlin=Xlin,Xrandom=Xrandom,Xexo=Xexo, starting.guess.theta2=starting.guess.theta2))
  }

  mm.li = vector("list",length(fvars))

  # Check number of rows of starting.guess.theta2
  if (NROW(starting.guess.theta2)!=length(Xrandom)) {
    stop("starting.guess.starting.guess.theta2 must have the same number of variables as Xrandom.")
  }
  
  for (i in 1:length(fvars)) {
    v = fvars[i]
    form = as.formula(paste0("~",v))
    mm.li[[i]] = model.matrix(form,dat)[,-1]
    new.v =  colnames(mm.li[[i]])
    if (v %in% Xlin) Xlin = c(setdiff(Xlin,v),new.v)
    if (v %in% Xexo) Xexo = c(setdiff(Xexo,v),new.v)
    if (v %in% Xrandom) {
      # Adapt starting.guess.theta2
      cur.ind = which(Xrandom==v)
      new.rows = c(seq_along(Xrandom)[-cur.ind], rep(cur.ind, length(new.v)))
      if (is.matrix(starting.guess.theta2)) {
        starting.guess.theta2 = starting.guess.theta2[new.rows,]
      } else {
        starting.guess.theta2 = starting.guess.theta2[new.rows]
      }
      
      Xrandom = c(Xrandom[-cur.ind],new.v)
    }
  }
 
  mm = as.data.frame(do.call(cbind, mm.li))
  return(list(dat=cbind(as.data.frame(dat), mm),Xlin=Xlin,Xrandom=Xrandom,Xexo=Xexo, starting.guess.theta2=starting.guess.theta2))
}
