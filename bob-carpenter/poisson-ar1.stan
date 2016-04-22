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
  vector[T] u;          // latent state ("error term")
}
model {
  // priors
  alpha ~ normal(0, 5);
  rho ~ lognormal(0, 2);
  sigma ~ lognormal(0, 2);

  // latent state
  // u[1] ~ normal(0, 5);
  for (t in 2:T)
    u[t] ~ normal(rho * u[t - 1], sigma);

  // likelihood
  for (t in 1:T)
    y[t] ~ poisson_log(alpha + u[t]);
}
