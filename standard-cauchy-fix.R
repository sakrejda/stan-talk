library(rstan); m1 <- stan('standard-cauchy-fix.stan', chains=1); 
s1 <- extract(m1, inc_warmup=TRUE, permuted=FALSE)
plot(s1[,1,1], type='l')
plot(s1[,1,2], type='l')

# Hey, where did that problem go?

