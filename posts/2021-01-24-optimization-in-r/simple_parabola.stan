parameters {
  real<lower=0> x; // easily does constrained optimization
}

model {
  target += 15 + 10*x - 2*x^2;
}

