
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



#Ininitial distribution over the categories
Y0 = c(950,50,0)
N = sum(Y0)


#Probablities in the transition probability matrix
alpha = 0.01
gamma = 0.10
beta_n <- function(Y) {
  return (0.5 * Y[2]/N)
}



#Simulates a step in the Markov chain
simulate_step <-  function(Y) {
  I_new = rbinom(size = Y[1],1,beta_n(Y))
  R_new = rbinom(size = Y[2],1,gamma)
  S_new = rbinom(size = Y[3],1,alpha)
  
  Y_new = c(Y[1] - I_new + S_new, Y[2] - R_new + I_new, Y[3] - S_new + R_new)
  
  return (Y_new)
}


#simulates and plots the markov chain for a specified number of iterations
simulate_and_plot_realization <- function(Y0,iterations) {
  Y = Y0
  S_n = integer(iterations)
  S_n[1] = Y[1]
  
  I_n = integer(iterations)
  I_n[1] = Y[2]
  
  R_n = integer(iterations)
  R_n[1] = Y[3]
  
  
  for (i in 2:(iterations+1)) {
    Y = simulate_step(Y)
    S_n[i] = Y[1]
    I_n[i] = Y[2]
    R_n[i] = Y[3]
  }
  t = seq(1,iterations+1, by = 1)
  plot(t,S_n, type = "l", col = "blue", ylab = "[number of individuals]", xlab = "n [days]")
  lines(t,I_n, col = "red")
  lines(t,R_n, col = "green")
  legend("topright",c("S (susceptible)","I (infected)", "R (recovered)"), fill = c("blue","red", "green"))
  
  M = matrix(c(S_n,I_n,R_n), nrow = 3, byrow = TRUE)
  return (M)
  
}

# +++++++++++++++++++
# Run code under here
# +++++++++++++++++++


#iterations = 300
#Y= simulate_and_plot_realization(Y0,iterations)



# ==============================================
# PROBLEM 1G
# ==============================================

iterations = 365 * 100
Y = simulate_and_plot_realization(Y0,iterations)
S_n = Y[1,]
I_n = Y[2,]
R_n = Y[3,]

# Print simulated values. 
print("Mean Proportions:", quote=FALSE)
cat("Susceptible: ",sum(S_n)/(iterations*N), "\n")
cat("Infeceted:", sum(I_n)/(iterations*N), "\n")
cat("Recovered: ", sum(R_n)/(iterations*N), "\n")



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


