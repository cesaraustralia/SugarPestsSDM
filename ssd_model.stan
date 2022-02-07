// The input data is a vector 'y' of length 'N'.
data {
  // training data
  int<lower=0> N;
  int y[N];
  
  // // prediction/test data
  int<lower=0> N_pred;
  // int y_pred[N_pred];
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  // int<lower=0> N;
  vector[N] s;
  
  real<lower=0> sigma_s; // variance for hyperprior on initial s
  real<lower=0> sigma_t_s; // the time dependence
  
  real theta_0; // intercept for serve probability
}


transformed parameters {
  vector[N] alpha;
  vector[N] mu;
  
  for (i in 1:N){
    // linear model
    alpha[i] = s[i] + theta_0;
    
    // mu of poisson
    mu[i] = exp(alpha[i]);
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
  for (i in 2:N){
    s[i] ~ normal(s[i-1], sigma_t_s);
  }
  
  
  
  // data model - the number of pest, independent of process model
  y ~ poisson(mu);
  // y ~ poisson_log(alpha);
  // for(i in 1:N){
  //   y[i] ~ poisson(alpha[i]);
  // }
}

// make predictions
generated quantities {
  vector[N_pred] prediction;

  for (i in 1:N_pred){
    prediction[i] = poisson_rng(mu[i]);
  }
}
