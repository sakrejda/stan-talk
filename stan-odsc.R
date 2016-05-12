## The "Hello world" of simulation studies.

library(rstan); library(magrittr); library(dplyr); library(tidyr)
library(shinystan)

# We are simulating M data sets of N data points with a
# mean of mu and standard deviation of sigma from the 
# normal distribution.
M <- 50  
N <- 15
mu <- 3.52
sigma <- 7.11

## This stan call uses the no-U-turn sampler (NUTS) to
## sample from a normal distribution.  This is complete
## overkill, but useful for thinking about Stan programs.
m1 <- stan('simulate-normal.stan', data=list(N=N, mu=mu, sigma=sigma),
  iter=M+300, warmup=300, chains=1)

## Extract the data sets in ggplot style.
y <- rstan::extract(m1, pars='y') %>% data.frame %>% 
  mutate(group=1:n()) %>% gather(point, y, -group)

## Calculate group statistics.
group_stats <- y %>% group_by(group) %>% 
  summarise(mean=mean(y), sigma=sd(y))

## Plot summaries:
#  x-axis: simulated value.
#  y-axis: data set index (there should be M of them).
#  red dots: data set mean.
#  blue line: true mean.
pl <- ggplot(data=y, aes(x=y, y=group)) + 
  geom_jitter(shape=4) + 
  geom_point(data=group_stats, aes(x=mean, y=group), 
    color="red", size=3) +
  geom_vline(xintercept=mu, color="blue") + theme_minimal()

## Let's get the batch means and their credible
## intervals (80%) as our estimates:
group_means <- matrix(data=NA, nrow=4, ncol=M)
rownames(group_means) <- c('group','10%','mu','90%')
y <- rstan::extract(m1, pars="y")[['y']]
for (m in 1:M) {
  m2 <- stan('estimate-normal.stan', data=list(N=N, y=y[m,]), chains=1)
  group_means[1,m] <- m
  group_means[2:4,m] <- rstan::extract(m2, pars='mu')[['mu']] %>%
    quantile(probs=c(0.1,.5,.9))
  group_means[3,m] <- rstan::extract(m2, pars='mu')[['mu']] %>%
    mean
}

## Plot estimates:
#  x-axis: simulated value.
#  y-axis: data set index (there should be M of them).
#  red dots: data set mean.
#  blue line: true mean.
pl_est <- pl + geom_errorbarh(
  data=group_means %>% t %>% data.frame(check.names=FALSE),
  aes(xmin=`10%`, x=mu, xmax=`90%`, y=group), color='blue') +
geom_point(
  data=group_means %>% t %>% data.frame(check.names=FALSE),
  aes(x=mu, y=group), color='blue')

print(pl_est)


