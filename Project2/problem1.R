#Project 2 Stochastic Modelling

#1.c


alpha = 0.1
lambda = 1/100
mu_L = 1/7
mu_H = 1/20

simulate_markov <- function(state0, tEnd, plot) {
  
  time_infected = 0
  time_between_Ih = 0
  time_between_Ih_list <- c()

  if (plot) {
    plot(NULL, NULL, 
         xlim = c(0, tEnd), 
         ylim = c(-0.2, 2.2), 
         xlab = "Time (days)", 
         lwd = 5,
         col = "red",
         ylab = "State", 
         main = "One Realization, 5 years")
    }
  
  time = 0
  state = state0
  while (time < tEnd) {
    if (state == 0) {
      #Sojourn time
      S =  rexp(1,rate = lambda)
      if (plot) {
        lines(c(time,time + S), c(state,state), lwd = 1)
      }
      
      #In case the sojourn time exceeds the end time
      if ( time + S > tEnd) {
        S = tEnd - time
      }
      time = time + S
      sample = runif(1)
      if (sample <= alpha) {
        state = 2
        if (time_between_Ih > 0) {
          time_between_Ih_list <- c(time_between_Ih_list, time - time_between_Ih)
          time_between_Ih = 0
        }
      }
      else {
        state = 1
      }
    }
    
    else if (state == 1) {
      S = rexp(1,rate = mu_L)
      if (plot) {
        lines(c(time,time + S), c(state,state), lwd = 2)
      }
      
      if ( time + S > tEnd) {
        S = tEnd - time
      }
      time = time + S
      time_infected = time_infected + S
      state = 0
    }
    
    else if (state == 2) {
      S = rexp(1,rate = mu_H)
      if (plot) {
        lines(c(time,time + S), c(state,state), lwd = 3)
      }
      
      if ( time + S > tEnd) {
        S = tEnd - time
      }
      time = time + S
      time_infected = time_infected + S
      time_between_Ih = time
      state = 0
    }
  }  
  
  
 
  return (c(time_infected/tEnd,mean(time_between_Ih_list)))
}

tEnd = 1000*365
result <-  simulate_markov(0, tEnd, FALSE)
time_infected <- result[1]
cat("Proportion of time infected: ",time_infected)

print(result[2])


