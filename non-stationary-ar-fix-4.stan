data {
  int n_obs;
  vector[n_obs] x;
}
  
transformed data {
  int n_steps;
  vector[n_obs] x_pos;
  n_steps <- n_obs-1;
  for ( i in 1:n_obs ) {
    x_pos[i] <- x[i] + fabs(min(x)) + 1;
  }

}

parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}


model {
  alpha ~ normal(0,1);
  beta ~ normal(0,2);
  sigma ~ gamma(2,2);
  {
    real ll;
    for ( i in 2:n_obs ) {
      ll <- normal_log(x_pos[i]/x_pos[i-1],
        alpha/x_pos[i-1] + beta, sigma/(x_pos[i-1]));
      increment_log_prob(ll);
    }
  }
}


