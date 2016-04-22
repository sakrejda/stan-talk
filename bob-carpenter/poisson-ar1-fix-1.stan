/**
 * Time Series of Count Data: Serially Correlated Error
 *
 * Model derived from:
 *
 *   Cameron and Trivedi (1998) Regression Analysis of Count Data.
 *   Cambridge University Press.  [Section 7.2.1, equations (7.4) and
 *   (7.5); section 7.2.2 enumeration item 3]
 *
 * Notes:
 *   - intercept only (no predictors), so use alpha rather than beta
 *   - stationarity requires rho < 1, but not enforced by constraint
 *   - priors introduced here
 */
data {
  int<lower=0> T;
  int y[T];
}
parameters {
  real alpha;           // intercept
  real<lower=0> rho;    // autoregression parameter
  real<lower=0> sigma;  // noise scale on latent state evolution
  vector[T-1] u_raw;          // raw latent state ("error term")
}
transformed parameters {
  vector[T] u;          // latent state ("error term")
  u[1] <- alpha;
  for (t in 2:T)
    u[t] <- rho*u[t-1] + u_raw[t-1]*sigma;
}
model {
  // priors
  alpha ~ normal(0, 5);
  rho ~ gamma(4, 10);
  sigma ~ gamma(2, 10);

  // raw latent state
  u_raw ~ normal(0, 1);

  // likelihood
  for (t in 1:T)
    y[t] ~ poisson_log(u[t]);
}
