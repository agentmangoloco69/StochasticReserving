//
functions {
  real negbin_lpdf(vector f, vector w, vector lamda, vector rootphi){
    vector[num_elements(f)] lprob;
    real lprobsum;
    for (i in 1:num_elements(f)) {
      lprob[i] = w[i]*((f[i]-1)*log(lamda[i]-1)-(f[i]*log(lamda[i])))/square(rootphi[i]);
    }
    lprobsum = sum(lprob);
    return lprobsum;
  }
}

// The input data is a vector 'f' (development factor) and a vector "w" (weight) of length 'N'.
// Also a matrix with col indicators
data {
  int<lower=0> N;
  int<lower=0> C;
  vector[N] f;
  vector[N] w;
  matrix[N,C] X;
  vector[N] rootphi;
  
}

// The parameters accepted by the model.
parameters {
//  real intercept;
  vector[C] coefs;
}

transformed parameters {
  vector[N] lambda;
  lambda = exp(exp(X*coefs));
}

// The model to be estimated
model {
//  coefs ~ uniform(-100,100); // no need to set priors
  f ~ negbin(w, lambda, rootphi);
}
