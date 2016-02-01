data {
  int n_obs;
  vector[n_obs] x;
}
  
transformed data {
  int n_steps;
  vector[n_obs] x_magnitude;
  vector[n_obs] x_sign;
  vector[n_obs-1] x_growth;
  n_steps <- n_obs-1;
  for ( i in 1:n_obs ) {
    x_magnitude[i] <- abs(x[i]);
  }
  for ( i in 1:n_obs ) {
    if (x[i] < 0) 
      x_sign[i] <- 1;
    else 
      x_sign[i] <- 0;
  }
  for ( i in 1:n_steps ) {
    x_growth[i] <- log(x_magnitude[i+1]+.01) -
                   log(x_magnitude[i]+.01);
  }
}

parameters {
  real<lower=0> beta;
  real<lower=0> sigma;
}


model {
  beta ~ normal(0,2);
  sigma ~ gamma(2,2);
  {
    x_growth ~ normal(log(beta),log(sigma));
  }
}


