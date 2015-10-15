library(rstan); m1 <- stan('standard-cauchy.stan', chains=1); 
s1 <- extract(m1, inc_warmup=TRUE, permuted=FALSE)
s1df <- s1[,1,] %>% data.frame %>% mutate(iteration=1:nrow(.)) 


plot(s1[,1,1], type='l')
plot(s1[,1,2], type='l')

# Oops, what happened there?

