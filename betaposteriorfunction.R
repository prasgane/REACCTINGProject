# Function that gives the log-liklihood function for the Adapative Rejection Sampling. Used by arms().
#
betajposterior = function(x, sum.y_xbetaother.sq, sum.y_xbetaother_timesx, sum.xisq, sigma,  sigmabeta,...){
  log.betai.posteriors = - (sum.y_xbetaother.sq - 2*x*sum.y_xbetaother_timesx + x^2*sum.xisq)/ (2 * sigma^2) - x^2/(2*sigmabeta^2)
  return(log.betai.posteriors)
}

c.posterior = function(x, d, sigmasq, lambda.c){
    x*log(d) - lgamma(x) + (-1-x)*log(sigmasq) - lambda.c*x
}
d.posterior = function(x, c, sigmasq, lambda.d){
    c*log(x) - x*(lambda.d+1/sigmasq)
}
