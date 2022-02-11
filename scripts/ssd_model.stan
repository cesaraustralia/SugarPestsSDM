// The input data is a vector 'y' of length 'N'.
data {
  // training data
  int<lower=0> N;
  int y[N];

  // Times of the observations
  int<lower=0> T; // number of time periods to predict to
  int t[N]; // Time periods of the observations
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  // int<lower=0> N;
  vector[T] s;

  real<lower=0> sigma_s; // variance for hyperprior on initial s
  real<lower=0> sigma_t_s; // the time dependence

  real theta_0; // intercept for the process model
}


transformed parameters {
  vector[N] mu;

  for (i in 1:N){
    // linear model for poisson link
    mu[i] = exp(s[i]);
  }

}

// The model to be estimated.
model {
  // priors
  sigma_s ~ normal(0, 10);
  sigma_t_s ~ normal(0, 10);
  theta_0 ~ normal(0, 10);

  // initial priors
  s[1] ~ normal(0, sigma_s);

  // process model - state at time
  for (i in 2:T){
    s[i] ~ normal(s[i-1] + theta_0, sigma_t_s);
  }

  // data model - the number of pest, independent of process model
  // Note: We've now included the t index to link the observations to the correct time periods
  y ~ poisson(mu[t]);
}

// make predictions
generated quantities {
  // So we'll predict all periods:
  vector[T] prediction;

  for (i in 1:T){
    prediction[i] = poisson_rng(exp(s[i]));
  }
}
