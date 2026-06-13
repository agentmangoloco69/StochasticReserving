// The input data is a vector 'f' (development factor) and a vector "w" (weight) of length 'N'.
// Also a matrix with col indicators
data {
  int<lower=0> N;
  int<lower=0> C;
  vector[N] f;
  vector[N] w;
  array[N] int col;
  matrix[N,C] X;
  vector[C] sigmas;
  
}

// The parameters accepted by the model.
parameters {
  vector[C] coefs;
}

// The model to be estimated
model {
//  coefs ~ uniform(-100,100); // no need to set priors
  for (i in 1:N) {
    f[i] ~ normal(exp(X[i]*coefs), sigmas[col[i]]/sqrt(w[i]));
  }
}
