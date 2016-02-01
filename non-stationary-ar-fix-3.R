#' Standard scalar auto-regressive (AR1) model with a 
#' simple intercept and a centered parameterization. 
#'
#' The simulated data sets generated here are all 
#' stationary (single coef < 1):

library(rstan)
library(dplyr)
library(ggplot2)
library(tidyr)

n_iterations <- 700
n_warmup <- 500

n_steps <- 100
n_obs <- n_steps + 1;
beta <- 2
sigma <- 1

x <- vector(mode='numeric', length=n_obs)
x[1] <- 0
for ( i in 2:n_obs ) {
  x[i] <- rnorm(1,beta*x[i-1], sigma)
}

m1 <- stan('non-stationary-ar-fix-3.stan', chains=10, data=list(
  n_obs=n_obs, x=x), iter=n_iterations); 
s1 <- rstan::extract(m1, inc_warmup=TRUE, permuted=FALSE)
dimnames(s1)[['iterations']] <- as.character(1:n_iterations)
s1 <- reshape2::melt(s1)

pl_traces <- ggplot(data=s1, 
  aes(x=iterations, y=value, colour=chains)
) + geom_point() + facet_wrap( ~ parameters, scales='free_y')

print(pl_traces)

output_store <- '~/output-store/stan-talk'
pdf(file=file.path(output_store,'standard-ar-traces.pdf'), width=8, height=6, useDingbats=FALSE)
print(pl_traces); dev.off()

pl_pairs <- s1 %>% spread(parameters, value) %>% select(3:ncol(.)) %>% pairs
print(pl_pairs)




