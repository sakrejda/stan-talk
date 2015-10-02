
parameters {
  real x_raw;
}

transformed parameters {
  real x;
	x <- tan(x_raw);
}

model {
  // z ~ unif(); // implied!
}







