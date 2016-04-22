data {
  int<lower=0> J; // number of schools 
  real y[J]; // estimated treatment effects
  real<lower=0> sigma[J]; // s.e. of effect estimates 
}
parameters {
  real mu; 
  real<lower=0> tau;
  real eta[J];
}
model {
  eta ~ normal(0, 1);
  for (j in 1:J)
    eta[j] ~ normal(mu,tau);
  y ~ normal(eta, sigma);
}



