#Project 2 Stochastic Modelling
#Promblem 2: Calibrating Climate Models

#2.a
std = 0.5
mu = 0.5
cov_function <- function(theta1, theta2){
  val = (1+15*abs(theta1 - theta2))*exp(-15*abs(theta1 -theta2)) * std^2
  return (val)
}

theta_grid = seq(from = 0.25, to = 0.50, by = 0.0049999)

#Known values
sample_points =  c(0.30,0.35,0.39,0.41,0.45)
sample_values = c(0.5, 0.32, 0.40, 0.35, 0.60)


#Function for creating covariance matrix
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
Sigma_AA <-  get_cov(theta_grid, theta_grid)
Sigma_BB <- get_cov(sample_points, sample_points)


muA <-  rep(mu,length(theta_grid))
muB <- rep(mu,length(sample_points))

muC <- muA + Sigma_AB %*% solve(Sigma_BB) %*% (sample_values - muB)
Sigma_C <- Sigma_AA - Sigma_AB %*% solve(Sigma_BB) %*% t(Sigma_AB)


#Calculates a realization.
L = chol(Sigma_C)
z <- rnorm(length(theta_grid))
predict <- muC + L %*% z

#plot the conditional mean or a realization.
plot(theta_grid, muC)
lines(theta_grid, muC)

#prediction interval.
z005 = qnorm(0.95)

upper = muC + z005*sqrt(diag(Sigma_C))
lower = muC - z005*sqrt(diag(Sigma_C))

lines(theta_grid,upper, col = "green")
lines(theta_grid,lower, col = "red")



#2.b
y <- rep(0.30,length(theta_grid))
var_vec<- diag(Sigma_C)
std_vec <-  sqrt(var_vec)
standardize <- (y - muC) / std_vec
probs1 <- pnorm(standardize)

plot(theta_grid, probs1,
     main = "Conditional Probability as Function of Theta", 
     ylab = "Conditional Prob.",
     xlab = "Theta")
lines(theta_grid, probs1)

#2.c

#Same as in a), but with one more point.
sample_points =  c(0.30,0.35,0.39,0.41,0.45, 0.33)
sample_values = c(0.5, 0.32, 0.40, 0.35, 0.60, 0.40)

Sigma_AB <-  get_cov(theta_grid, sample_points)
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
var_vec<- diag(Sigma_C)
std_vec <-  sqrt(var_vec)
standardize <- (y - muC) / std_vec
probs2 <- pnorm(standardize)


plot(theta_grid, probs2,
     main = "Conditional Probability as Function of Theta", 
     ylab = "Conditional Prob.",
     xlab = "Theta")
lines(theta_grid, probs2)
lines(theta_grid, probs1)
