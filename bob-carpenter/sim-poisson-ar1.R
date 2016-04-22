T <- 200;
alpha <- 4.6;  # log(alpha) approx. 100
sigma <- .25;
rho <- 0.7;
u <- rep(NA, T);
u[1] <- 0.5;
for (t in 2:T) {
  u[t] <- rnorm(1, rho * u[t - 1], sigma);
}
y <- rep(NA, T);
for (t in 1:T) {
  y[t] <- rpois(1, exp(alpha + u[t]));
}

fit_ar1 <- stan("poisson-ar1.stan", data=c("T", "y"),
                iter=800, warmup=500, chains=1);

qoi <- c("alpha", "sigma", "rho", "u[100]", "lp__");
print(fit_ar1, pars=qoi);
pairs(fit_ar1, pars=qoi);




