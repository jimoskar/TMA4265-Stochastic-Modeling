
#2.a
std = 0.5
mu = 0.5
cov_function <- function(theta1, theta2){
  val = (1+15*abs(theta1 - theta2))*exp(-15*abs(theta1 -theta2)) * std^2
  return (val)
}

theta_grid = seq(from = 0.25, to = 0.50, by = 0.0049999)
theta_grid

sample_points =  c(0.30,0.35,0.39,0.41,0.45)
sample_values = c(0.5, 0.32, 0.40, 0.35, 0.60)


get_cov <-  function(vecA,vecB) {
  nA <- length(vecA)
  nB <- length(vecB)
  result <- matrix(nrow = nA, ncol = nB)
  for (i in 1:nA) {
    for (j in 1:nB) {
      result[i,j] = cov_function(vecA[i],vecB[j])
    }
  }
  return (result)
}

Sigma_AB <-  get_cov(theta_grid, sample_points)
Sigma_AB
Sigma_AA <-  get_cov(theta_grid, theta_grid)
Sigma_BB <- get_cov(sample_theta, sample_points)


muA <-  rep(mu,length(theta_grid))
muB <- rep(mu,length(sample_points))

muC <- muA + Sigma_AB %*% solve(Sigma_BB) %*% (sample_values - muB)
muC
Sigma_C <- Sigma_AA - Sigma_AB %*% solve(Sigma_BB) %*% t(Sigma_AB)
Sigma_C


L = chol(Sigma_C)
z <- rnorm(length(theta_grid))
predict <- muC + L %*% z


plot(theta_grid, predict)
lines(theta_grid, predict)

#prediction interval
z005 = qnorm(0.95)

upper = muC + z005*sqrt(diag(Sigma_C))
lower = muC - z005*sqrt(diag(Sigma_C))

lines(theta_grid,upper, col = "green")
lines(theta_grid,lower, col = "red")

#2.b

library(expm)   
y <- rep(0.30,length(theta_grid))
std_matrix <- Sigma_C %^% -0.5
muC
standardize <- solve(std_matrix) %*% (y - muC)
standardize
probs <-  pnorm(standardize)
plot(theta_grid, probs)
lines(theta_grid, probs)

#2.c

sample_points =  c(0.30,0.35,0.39,0.41,0.45, 0.33)
sample_values = c(0.5, 0.32, 0.40, 0.35, 0.60, 0.40)

Sigma_AB <-  get_cov(theta_grid, sample_points)
Sigma_AB
Sigma_AA <-  get_cov(theta_grid, theta_grid)
Sigma_BB <- get_cov(sample_points, sample_points)


muA <-  rep(mu,length(theta_grid))
muB <- rep(mu,length(sample_points))

muC <- muA + Sigma_AB %*% solve(Sigma_BB) %*% (sample_values - muB)
muC
Sigma_C <- Sigma_AA - Sigma_AB %*% solve(Sigma_BB) %*% t(Sigma_AB)
Sigma_C


L = chol(Sigma_C)
z <- rnorm(length(theta_grid))
predict <- muC + L %*% z


plot(theta_grid, predict)
lines(theta_grid, predict)

#prediction interval
z005 = qnorm(0.95)

upper = muC + z005*sqrt(diag(Sigma_C))
lower = muC - z005*sqrt(diag(Sigma_C))

lines(theta_grid,upper, col = "green")
lines(theta_grid,lower, col = "red")

y <- rep(0.30,length(theta_grid))
std_matrix <- Sigma_C %^% -0.5
muC
standardize <- solve(std_matrix) %*% (y - muC)
standardize
probs <-  pnorm(standardize)
plot(theta_grid, probs)
lines(theta_grid, probs)













