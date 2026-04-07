
#Load libraries

library(nloptr)
library(modelsummary)

#Set seed and generate data

set.seed(100)

N <- 100000
K <- 10

# X: first column of 1s, rest standard normal
X <- cbind(1, matrix(rnorm(N * (K - 1)), nrow = N, ncol = K - 1))

# ε ~ N(0, 0.25)
eps <- rnorm(N, mean = 0, sd = 0.5)

# β vector
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

# Y = Xβ + ε
Y <- X %*% beta + eps


# 5. OLS estimate (closed form)

beta_hat_ols <- solve(t(X) %*% X) %*% t(X) %*% Y
print(beta_hat_ols)

# Compare with true beta
print(beta)

# 6. Gradient Descent for OLS

gradient_descent <- function(X, Y, lr = 0.0000003, tol = 1e-6, max_iter = 5000){
  beta_gd <- rep(0, ncol(X))
  for(i in 1:max_iter){
    grad <- -t(X) %*% (Y - X %*% beta_gd)
    beta_gd_new <- beta_gd - lr * grad
    if(max(abs(beta_gd_new - beta_gd)) < tol) break
    beta_gd <- beta_gd_new
  }
  return(beta_gd)
}

beta_hat_gd <- gradient_descent(X, Y)
print(beta_hat_gd)


# 7. nloptr OLS (L-BFGS and Nelder-Mead)

ols_obj <- function(beta, X, Y){
  sum((Y - X %*% beta)^2)
}

# L-BFGS
res_lbfgs <- nloptr(
  x0 = rep(0, K),
  eval_f = ols_obj,
  eval_grad_f = function(beta, X, Y) as.vector(-t(X) %*% (Y - X %*% beta)),
  opts = list(algorithm = "NLOPT_LD_LBFGS", xtol_rel = 1e-8),
  X = X, Y = Y
)
beta_hat_lbfgs <- res_lbfgs$solution
print(beta_hat_lbfgs)

# Nelder-Mead
res_nm <- nloptr(
  x0 = rep(0, K),
  eval_f = ols_obj,
  opts = list(algorithm = "NLOPT_LN_NELDERMEAD", xtol_rel = 1e-8),
  X = X, Y = Y
)
beta_hat_nm <- res_nm$solution
print(beta_hat_nm)


# 8. MLE estimate using L-BFGS
loglik_mle <- function(theta, Y, X){
  beta <- theta[1:(length(theta)-1)]
  log_sig <- theta[length(theta)]
  sig <- exp(log_sig)
  n <- length(Y)
  
  # Negative log-likelihood scaled by n
  -(-n/2 * log(2 * pi) - n * log(sig) - sum((Y - X %*% beta)^2)/(2 * sig^2)) / n
}

gradient_mle <- function(theta, Y, X){
  beta <- theta[1:(length(theta)-1)]
  log_sig <- theta[length(theta)]
  sig <- exp(log_sig)
  n <- length(Y)
  
  grad <- rep(0, length(theta))
  grad[1:(length(theta)-1)] <- (-t(X) %*% (Y - X %*% beta)/(sig^2)) / n
  grad[length(theta)] <- ((n/sig - crossprod(Y - X %*% beta)/(sig^3)) * sig) / n
  return(grad)
}

theta0 <- c(rep(0,K), log(1))

res_mle <- nloptr(
  x0 = theta0,
  eval_f = loglik_mle,
  eval_grad_f = gradient_mle,
  opts = list(
    algorithm = "NLOPT_LD_LBFGS",
    xtol_rel = 1e-8,
    maxeval = 1000
  ),
  Y = Y, X = X
)

beta_mle <- res_mle$solution[1:K]
sigma_mle <- exp(res_mle$solution[K+1])
print(beta_mle) 
print(sigma_mle) 

# 9. Easy OLS via lm()
# -1 to remove intercept
model <- lm(Y ~ X - 1)  
summary(model)

# Export to .tex using modelsummary
modelsummary(model, output = "PS8_ModelSummary.tex")