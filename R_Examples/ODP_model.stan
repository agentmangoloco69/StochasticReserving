//
functions {
  real odp_lpdf(vector cl, vector mu, vector rootphi){
    vector[num_elements(cl)] lprob;
    real lprobsum;
    for (i in 1:num_elements(cl)) {
      lprob[i] = (cl[i]*log(mu[i])-mu[i])/square(rootphi[i]);
    }
    lprobsum = sum(lprob);
    return lprobsum;
  }
}

// The input data is a vector 'cl' (incremental claims), a row vector and a column vector of length 'N'.
// Also a design matrix with intercept, row and col indicators
data {
  int<lower=0> N;
  int<lower=0> C;
  vector[N] cl;
  matrix[N,C] X;
  vector[N] rootphi;
}

// The parameters accepted by the model.
parameters {
  vector[C] coefs;
}

transformed parameters {
  vector[N] mu;
  mu = exp(X*coefs);
}

// The model to be estimated
model {
//  coefs ~ uniform(-100,100); // no need to set priors
  cl ~ odp(mu, rootphi);
}


