data {
  int n_obs;
  vector[n_obs] x;
}
  
transformed data {
  int n_steps;
  n_steps <- n_obs-1;
}

parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}


model {
  alpha ~ normal(0,1);
  beta ~ normal(0,.3);
  sigma ~ gamma(2,2);
  {
    vector[n_steps] x_mu;
    x_mu <- alpha + beta * head(x,n_steps);
    tail(x,n_steps) ~ normal(x_mu,sigma);
  }
}


