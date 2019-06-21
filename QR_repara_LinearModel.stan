//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; // number of data items
  int<lower=0> K; // number if predictors
  matrix[N, K] x; // predictor matrix
  vector[N] y;    // outcome vector
}

transformed data {
  matrix[N, K] Q_ast;
  matrix[K, K] R_ast;
  matrix[K, K] R_ast_inverse;
  // thin and scale the QR decomposition
  Q_ast = qr_Q(x)[, 1:K] * sqrt(N - 1);
  R_ast = qr_R(x)[1:K, ] / sqrt(N - 1);
  R_ast_inverse = inverse(R_ast);
} 

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real alpha; // intercept
  vector[K] theta; //coefficients on Q_ast
  real<lower=0> sigma; //error scale
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  y ~ normal(Q_ast * theta + alpha, sigma); // lilelihood

}

generated quantities {
  vector[K] beta;
  vector[N] y_pred;
  
  beta = R_ast_inverse * theta; // coefficients on x
  
  // unknown how to write vectorize representation
  for(n in 1:N)
    y_pred[n] = normal_rng(Q_ast[n, ] * theta + alpha, sigma);
  
}

