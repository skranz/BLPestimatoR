#' @importFrom stats as.formula
#' @importFrom stats model.matrix



subst.vec.el = function(x, old.val=NULL, new.val,old.pos=which(x==old.val)) {
  if (old.pos == 1) {
    return(c(new.val,x[-1]))
  }
  if (old.pos == length(x)) {
    return(c(x[-old.pos],new.val))
  }
  return(c(x[1:(old.pos-1)], new.val, x[(old.pos+1):length(x)]))
}


# Helper function to convert factor variables in Xlin, Xrandom or Xexo
# to dummy variables.
# Also adapt the variable names in Xlin, Xrandom and Xexo
# 
BLP.factor.to.dummies = function(dat, Xlin, Xrandom, Xexo, instruments, starting.guess.theta2) {
  vars = unique(c(Xlin,Xrandom,Xexo, instruments))
  fvars = vars[sapply(dat[vars], function(vals) is.factor(vals) | is.character(vals))]

  if (length(fvars)==0) {
    return(list(dat=dat,Xlin=Xlin,Xrandom=Xrandom,Xexo=Xexo,instruments=instruments, starting.guess.theta2=starting.guess.theta2))
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
    if (v %in% Xlin) Xlin = subst.vec.el(Xlin,v,new.v)
    if (v %in% Xexo) Xexo = subst.vec.el(Xexo,v,new.v)
    if (v %in% instruments) instruments = subst.vec.el(instruments,v,new.v)
    if (v %in% Xrandom) {
      # Adapt starting.guess.theta2
      cur.ind = which(Xrandom==v)
      new.rows = subst.vec.el(seq_along(Xrandom),cur.ind,rep(cur.ind, length(new.v)))
      
      Xrandom = subst.vec.el(Xrandom,v,new.v)
      if (is.matrix(starting.guess.theta2)) {
        starting.guess.theta2 = starting.guess.theta2[new.rows,]
        rownames(starting.guess.theta2) = Xrandom
      } else {
        starting.guess.theta2 = starting.guess.theta2[new.rows]
      }

    }
  }
 
  mm = as.data.frame(do.call(cbind, mm.li))
  return(list(dat=cbind(as.data.frame(dat), mm),Xlin=Xlin,Xrandom=Xrandom,Xexo=Xexo, instruments=instruments, starting.guess.theta2=starting.guess.theta2))
}

