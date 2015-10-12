#' bivariate donut
#' 
#' \theta ~ U(0,2*pi)
#' r ~ N(1,1/4)
#' 
#' x = f(\theta,r) = r*cos(\theta)
#' y = f(\theta,r) = r*sin(\theta)
#' 
#' r = f(x,y) = sqrt(x^2+y^2)
#'
#' theta = f(x,y) = arcsin(y/r) = arccos(x/r)
r_bivariate_donut <- function(n, r_mu=1, r_sd=r_mu/4) {
  thetas <- runif(n,0,2*pi)
  rs <- rnorm(n, r_mu, r_sd)
  x <- rs * cos(thetas)
	y <- rs * sin(thetas)
	return(cbind(x=x, y=y))
}

d_bivariate_donut <- function(x, y, r_mu=1, r_sd=r_mu/4, log=TRUE) {
  r <- sqrt(x^2,y^2)
#	theta <- asin(y/r)
# dunif(theta, 0,2*pi)
  cds <- dnorm(r,r_mu,r_sd,log=log)
  if (isTRUE(log)) 
		return(sum(cds))
	else
		return(prod(cds))
}

library(ggplot2)
draw_donut <- data.frame(r_bivariate_donut(10^4, 2,2/6))

pl_sim <- ggplot(data=draw_donut, aes(x=x, y=y)) + 
	geom_point() + geom_density2d(adjust=2) +
	theme_minimal()

pl_sim_only_density <- ggplot(data=draw_donut, aes(x=x, y=y)) + 
	geom_density2d(adjust=2) +
	theme_minimal()

print(pl_sim)
print(pl_sim_only_density)
print(cov(draw_donut))

#' What does Gibbs do?
#' What does Metropolis do?



