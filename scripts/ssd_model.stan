// defining he input data
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


// the parameters accepted by the model
parameters {
  vector[T] s; // latent time series; the process model

  real<lower=0> sigma_s0; // variance for hyperprior on initial s
  real<lower=0> sigma_s; // the time dependence
  
  real<lower=0> phi; // neg. binomial dispersion parameter
  
  real beta0; // intercept for the process model
  real b1_month; // slope of month
  real b2_month; // slope of month
}


// calculate vatiables that do not rely on model parameters
transformed parameters {
  // transfer month to sine and cosine as circular variable
  vector[T] month_cos;
  vector[T] month_sin;
  
  month_cos = cos(2 * 3.141593 * (month / 12));
  month_sin = sin(2 * 3.141593 * (month / 12));
}


// The model to be estimated.
model {
  // priors
  phi ~ cauchy(0., 3);
  sigma_s0 ~ normal(0, 10);
  sigma_s ~ normal(0, 10);
  beta0 ~ normal(0, 10);
  b1_month ~ normal(0, 5);
  b2_month ~ normal(0, 5);

  // initial state
  s[1] ~ normal(0, sigma_s0);

  // process model - state at each time
  for (i in 2:T){
    s[i] ~ normal(s[i-1] + beta0 + b1_month * month_sin[i] + b2_month * month_cos[i], sigma_s);
  }

  // data model - the number of pest, independent of process model
  // y ~ poisson_log(s[ids]); 
  y ~ neg_binomial_2_log(s[ids], phi);
}

// make predictions
generated quantities {
  // predict to all periods:
  int<lower=0> prediction[T];
  vector[T] mu;
  
  mu = exp(s);
  
  for (i in 1:T){
    prediction[i] = neg_binomial_2_rng(mu[i], phi);
  }
}
