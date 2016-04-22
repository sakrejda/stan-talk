library(rstan); library(magrittr); library(dplyr); library(tidyr)
library(shinystan)

## #1 Simulate from a standard normal and look at two ways of plotting:
m1 <- stan('standard-normal.stan', chains=10); 

# Manual plotting:
s1 <- rstan::extract(m1, inc_warmup=TRUE, permuted=FALSE)
s1df <- s1[,1,] %>% data.frame %>% mutate(iteration=1:nrow(.)) 
pl_traces <- ggplot(
  data=s1df %>% gather(parameter, value, x, lp__), 
  aes(x=iteration, y=value)
) +
  geom_line() + 
  facet_grid(parameter ~ ., scales='free_y') + 
  theme_minimal() + theme(strip.text.y=element_text(angle=0))

# Shinystan plotting, what is it.
launch_shinystan(m1)

## #2 Directly written Cauchy simulation, look at correlation, 
#     look for divergences, look at treedepth
m2 <- stan('standard-cauchy.stan', chains=20); 
launch_shinystan(m2)

## #3 Re-parameterized Cauchy simulation, look at correlation,
#     look at divergences, look at treedepth
m3 <- stan('standard-cauchy-fix.stan', 
  chains=20, iter=500, warmup=300); 
launch_shinystan(m3)

### AR simulation and model:
n_steps <- 100
n_obs <- n_steps + 1;
beta <- .5
sigma <- 1

x <- vector(mode='numeric', length=n_obs)
x[1] <- 0
for ( i in 2:n_obs ) {
  x[i] <- rnorm(1,x[i-1]*beta, sigma)
}

m4 <- stan('standard-ar.stan', data=list(
  n_obs=n_obs, x=x), chains=4, iter=400, warmup=200); 

# Do explore/estimate
launch_shinystan(m4)


### Donut! Do explore on tri-variate plot, 3 are: exp(ld), x, y 
m5 <- stan('donut-sim-corrected.stan', chains=1, data=list(r_mu=r_mu, r_sd=r_sd),
iter=5000, thin=5)

launch_shinystan(m5)


### 8 Schools:
schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

# centered, look at correlation, treedepth, step size, divergences 
#           (as marked in explore plots).
m7 <- stan(file = 'schools-8-center.stan', data = schools_dat, 
            iter = 1000, chains = 4)
launch_shinystan(m7)

# non-centered, look at correlation, treedepth, step size, 
#               divergences.
m6 <- stan(file = 'schools-8.stan', data = schools_dat, 
            iter = 1000, chains = 4)
launch_shinystan(m6)



