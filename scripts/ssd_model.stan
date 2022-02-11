// The input data is a vector 'y' of length 'N'.
data {
  // training data
  int<lower=0> N; // number of observed insects
  int y[N];

  // Times of the observations
  int<lower=0> T; // number of time periods to predict to
  int t[N]; // Time periods of the observations
  
  // covariates for the process model
  vector[T] month;
  
  // the indices of observations over the whole time period
  int<lower=0> ids[N];
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  /// **** change this to int<lowewr=0> s[T]; to avoid negatives? ****
  vector[T] s; // latent time series; the process model

  real<lower=0> sigma_s0; // variance for hyperprior on initial s
  real<lower=0> sigma_s; // the time dependence

  real beta0; // intercept for the process model
  real b_month; // slope of month
}


transformed parameters {
  vector[N] mu;

  // for (i in 1:N){
  //   mu[i] = exp(s[i]);
  // }
  
  // the exponentail of the hidden process for poisson
  mu = exp(s[ids]);

}

// The model to be estimated.
model {
  // priors
  sigma_s0 ~ normal(0, 10);
  sigma_s ~ normal(0, 10);
  beta0 ~ normal(0, 10);
  b_month ~ normal(0, 3);

  // initial priors
  s[1] ~ normal(0, sigma_s0);

  // process model - state at time
  for (i in 2:T){
    s[i] ~ normal(s[i-1] + beta0 + b_month * month[i], sigma_s);
  }

  // data model - the number of pest, independent of process model
  // Note: We've now included the t index to link the observations to the correct time periods
  y ~ poisson(mu[t]); // [t] can be remove compeletely
}

// make predictions
generated quantities {
  // So we'll predict all periods:
  vector[T] prediction;

  for (i in 1:T){
    prediction[i] = poisson_rng(exp(s[i]));
  }
}
