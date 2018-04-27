#' Conveniently extract elasticities from result of estimateBLP
#' 
#' Currently only works for variables whose name was added to the argument \code{postEstimation.control$elasticities} in the call to \code{estimateBLP}
#' 
#' @param blp The result from estimateBLP
#' @param var The variable name of the elasticity to be estimated
#' @param market Number or name of the market for which elasticities shall be computed
#' @param products A character vector or NULL (default). If not NULL only return elasticities for the specified products. Looks up product names in the column specified by \code{prodid} in the call to \code{estimateBLP}
#' @param round.digits If not NULL the number of digits to which elasticities shall be rounded (DEFAULT=3)
#' @export
elasticitiesBLP <- function(blp, var=names(blp$elasticities)[[1]], market=1, products=NULL, round.digits=3) {
  el = blp$elasticities[[var]][[market]]
  if (!is.null(products)) {
    ind = match(products, rownames(el))
    ind = na.omit(ind)
    el = el[ind,ind]
  }
  if (!is.null(round.digits)) {
    el = round(el, round.digits)
  }
  el
}
