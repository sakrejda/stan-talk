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
  vector[1] beta;
  vector<lower=0>[1] sigma;
}

model {
  alpha ~ normal(0,1);
  beta ~ normal(0,2);
  sigma ~ gamma(2,2);
  for ( i in 2:n_obs ) {
    x[i] ~ normal(alpha + beta*x[i-1],sigma);
  }
}


