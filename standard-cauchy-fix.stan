
parameters {
  real<lower=-pi()/2,upper=pi()/2> x_raw;
}

transformed parameters {
  real x;
	x <- tan(x_raw);
}

model {
  // x_raw ~ uniform(-.5*pi(),.5*pi);    // implied.
}







