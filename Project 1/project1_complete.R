
# ==============================================
# PROBLEM 1D
# ==============================================


beta <-  0.05
gamma <-  0.10
alpha  <-  0.01

P  <-  matrix(c(1-beta, beta, 0, 0, 1 - gamma, gamma, alpha, 0, 1-alpha), nrow <- 3, ncol <- 3, byrow <- TRUE)


# This function returns the mean over a specified number of simulations of the expected values
simulate_expected_values <- function(P, numSim = 1) {
  
  result <- matrix(nrow = numSim, ncol = 3)
  
  for (j in 1:numSim) {
  
    v1 <- 0 #total number of "loops" in state 0
    v2 <- 0 #total number of "loops" in state 1
    v3 <- 0 #total number of transitions from 0 to 1 and 1 to 2
    
    v1_counter <- 0 #number of transitions from 0 to 1
    v2_counter <- 0 # number of transitions from 1 to 2
    v3_counter <- 0 # number of complete cycles
    
    year <- 365
    state <- 0
    for (i in 1:(year*50)) {
      
      
      newstate <- sample.int(3, size <- 1, replace <- TRUE, prob <- P[state+1,])-1
      
      if (newstate != state) { # a new state has been transistioned to 
        if(state == 0) {
          v1_counter <- v1_counter + 1
          v1 <- v1 + 1
          v2 <- v2 + 1
          v3 <- v3 +1
        }
        else if(state == 1) {
          v2_counter <- v2_counter + 1
          v2 <- v2 + 1
          v3 <- v3 + 1
        }
        else if(state == 2) {
          v3_counter <- v3_counter + 1
          v3 <-  v3 + 1
        }
        
      }
      else { #The transistion was to the same state
        if (state == 0) {
          v1 <- v1 +1
          v2 <-  v2 + 1
          v3 <- v3 + 1}
        else if (state == 1) {
          v2 <- v2 +1
          v3 <- v3 + 1}
        else if (state == 2){
          v3 <- v3 + 1
        }
      }
      
      state <- newstate
      
    }
    
    result[j,] <- c(v1/v1_counter,v2/v2_counter,v3/v3_counter)
  }
  return (c(mean(result[,1]),mean(result[,2]),mean(result[,3]))) #returns the mean over the simulations
}

# +++++++++++++++++++
# Run code under here
# +++++++++++++++++++



#simulate_expected_values(P,100)



# ==============================================
# PROBLEM 1F
# ==============================================
#Highlight the code underneath and run it to get the answers to f)



N <- 1000 # Individuals.

Y0 <- c(950, 50, 0) # Starting state.

# Probabilities (to begin with).
gamma <- 0.10
alpha <- 0.01
beta <- function(Y){
  return (0.5*Y[2]/N)
}
n <- 300 # Time steps.

values <- matrix(data=NA,nrow=3,ncol=n) # Preallocate matrix for simulated values. 
values[, 1] <- Y0

# Run simulation.
for (t in 2:n){
  # Use a binomial to simulate. 
  old_susc <- values[1, t-1]
  old_inf <- values[2, t-1]
  old_rec <- values[3, t-1]
  
  new_inf <- rbinom(n = 1, size = old_susc, beta(values[, t-1]))
  new_rec <- rbinom(n = 1, size = old_inf, gamma) 
  new_susc <- rbinom(n = 1,size =  old_rec, alpha) 
  Y <- c(old_susc - new_inf + new_susc, old_inf - new_rec + new_inf, old_rec - new_susc + new_rec)
  values[, t] <- Y
}

par_lty <- c(3,2,1)
par_col <- c("blue", "red", "green")
plot(1:n, values[1, ], type = "l", lty = par_lty[1], col = par_col[1], xlab="Time [days]", ylab = "Individuals", main = "One Realization")
lines(1:n, values[2, ], type = "l", lty = par_lty[2], col = par_col[2])
lines(1:n, values[3, ], type = "l", lty = par_lty[3], col = par_col[3])
legend("topright", legend= c("Susceptible", "Infected", "Recovered"), lty = par_lty, col = par_col)


# ==============================================
# PROBLEM 1G
# ==============================================
#Highlight the code underneath and run it to get the answers to g)

N <- 1000 # Individuals.

Y0 <- c(950, 50, 0) # Starting state.

# Probabilities (to begin with).
gamma <- 0.10
alpha <- 0.01
beta <- function(Y){
  return (0.5*Y[2]/N)
}
n <- 36500 # Time steps. 100 years. 

values <- matrix(data=NA,nrow=3,ncol=n) # Preallocate matrix for simulated values. 
values[, 1] <- Y0

sim <- function(values){
  # Run simulation.
  for (t in 2:n){
    # Use a binomial to simulate. 
    old_susc <- values[1, t-1]
    old_inf <- values[2, t-1]
    old_rec <- values[3, t-1]
    
    new_susc <- rbinom(n = 1, size = old_susc, 1-beta(values[, t-1]))
    new_inf <- rbinom(n = 1, size = old_inf, 1-gamma) 
    new_rec <- rbinom(n = 1,size =  old_rec, 1-alpha) 
    Y <- c(new_susc - (new_rec - old_rec), new_inf - (new_susc - old_susc), new_rec - (new_inf - old_inf))
    values[, t] <- Y
  }
  return (values)
}

take.mean <- function(amount, fun = sim){
  # Take mean of 'amount' number of simulations. 
  average_susc <- c(length = amount)
  average_inf <- c(length = amount)
  average_rec <- c(length = amount)
  
  for (i in 1:amount){
    # Take an average. 
    val <- sim(values)
    susc <- val[1, ncol(val)]/N
    inf <- val[2, ncol(val)]/N
    rec <- val[3, ncol(val)]/N
    average_susc[i] <- susc
    average_inf[i] <- inf
    average_rec[i] <- rec
  }
  return (list("val" = val, "avg1" = mean(average_susc), "avg2" = mean(average_inf), "avg3" = mean(average_rec)))
}

amount <- 100 # Number of simulations to take mean over. 
avg_prop <- take.mean(amount, sim)
val <- avg_prop$val # Values to plot. 

# Parameters for color and linetypes in plot. 
par_lty <- c(3,2,1)
par_col <- c("blue", "red", "green")

# Plot the lines. 
plot(1:n, val[1, ], type = "l", lty = par_lty[1], col = par_col[1], xlab="Time [days]", 
     ylab = "Individuals", main = capture.output(cat("Mean over ", amount, " realizations")))
lines(1:n, val[2, ], type = "l", lty = par_lty[2], col = par_col[2])
lines(1:n, val[3, ], type = "l", lty = par_lty[3], col = par_col[3])
legend("topright", legend= c("Susceptible", "Infected", "Recovered"), lty = par_lty, col = par_col)

# Print simulated values. 
print("Mean Proportions:", quote=FALSE)
cat("Susceptible ", avg_prop$avg1, "\n")
cat("Infected ", avg_prop$avg2, "\n")
cat("Recovered ", avg_prop$avg3)

# ==============================================
# PROBLEM 1H
# ==============================================


#Returns the mean over the maximum number of infected in a simulation and the corresponding time for a given number of simulations
estimate_max <- function(Y0,iterations,simulations) {
  maximum_sum = 0
  maximum_time_sum = 0
  for (i in 1:simulations){
    Y = Y0
    I_max = 0
    n_max = 0
    for (j in 1:iterations) {
      if (Y[2] > I_max) {
        I_max = Y[2]
        n_max = j
      }
      Y = simulate_step(Y)
    }
    
    maximum_sum = maximum_sum + I_max
    maximum_time_sum = maximum_time_sum + n_max
  }
  return (c(maximum_sum/simulations,maximum_time_sum/simulations))
}

Estimates <- estimate_max(Y0,300,1000)
print(Estimates)


# ==============================================
# PROBLEM 2A
# ==============================================

lambda = 1.5

#Simulates the poisson process for a given number of simulations
simulate_pois <- function(lambda, t_start,t_end, plot, numSim = 1) {
  result <-  rep(NA,numSim)
  for (i in 1:numSim) {
    n = rpois(1,lambda = lambda*(t_end - t_start))
    result[i] = n
    v_vec = runif(n,t_start,t_end)
    w_vec = sort(v_vec)
    x_vec <- seq(from = 0, to = n, by = 1)
    
    if(plot && i == 1) {
      
      plot(NULL, NULL, xlim = c(t_start, t_end), ylim = c(0, n), xlab = "Time [days]", lwd = 2, ylab = "Claims", main = "Insurance Claims: 10 Realizations", cex = 2)
      for(i in 1:n) {
        lines(w_vec[i:(i+1)], rep(x_vec[i],2), lwd = 1)
      }
    }
    else if(plot) {
      for(i in 1:n) {
        lines(w_vec[i:(i+1)], rep(x_vec[i],2), lwd = 1)
      }
    }
  }
  return (result)
}

# +++++++++++++++++++
# Run code under here
# +++++++++++++++++++


#Estimate the probability in question
res <- simulate_pois(lambda,0,59,plot = FALSE, numSim = 1000)
num <- sum(res > 100)/length(res)
num

#plotting
simulate_pois(lambda,0,59,plot = TRUE, numSim = 10)


# ==============================================
# PROBLEM 2B
# ==============================================


res <- simulate_pois(lambda,0,59,plot = FALSE, numSim = 1000)
mean(res)
var(res)

#Takes a list of poisson distributed values (claims) and returns the size of the claims summed together
calculate.claims <- function(claims) {
  result = rep(0,length(claims))
  for (i in 1:length(claims)) {
    for (j in 1:claims[i]) {
      c <-  rexp(1,rate = 10)
      result[i] <- result[i] + c
    }
  }
  return(result)
}


# +++++++++++++++++++
# Run code under here
# +++++++++++++++++++


claims <- simulate_pois(lambda,0,59,plot = FALSE, numSim = 1000*10)
res <- calculate.claims(claims)

mean(res)
var(res)


