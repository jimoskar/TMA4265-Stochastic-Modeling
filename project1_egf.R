#f)

Y0 = c(950,50,0)
N = sum(Y0)

alpha = 0.01
gamma = 0.10
beta_n <- function(Y) {
  return (0.5 * Y[2]/N)
}

simulate_step <-  function(Y) {
  I_new = rbinom(size = Y[1],1,beta_n(Y))
  R_new = rbinom(size = Y[2],1,gamma)
  S_new = rbinom(size = Y[3],1,alpha)
  
  Y_new = c(Y[1] - I_new + S_new, Y[2] - R_new + I_new, Y[3] - S_new + R_new)
  
  return (Y_new)
}



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
  plot(t,S_n, type = "l", col = "blue", ylab = "[number of individuals]", xlab = "n [days]", main = "One Realization")
  lines(t,I_n, col = "red")
  lines(t,R_n, col = "green")
  legend("topright",c("S (susceptible)","I (infected)", "R (recovered)"), fill = c("blue","red", "green"))
  
  M = matrix(c(S_n,I_n,R_n), nrow = 3, byrow = TRUE)
  return (M)
  
}

print(rbinom(size = 10,n = 1,0.5))

iterations = 300
#Y= simulate_and_plot_realization(Y0,iterations)

#g)

iterations = 365 * 100
Y = simulate_and_plot_realization(Y0,iterations)
S_n = Y[1,]
I_n = Y[2,]
R_n = Y[3,]
print(sum(S_n)/(iterations*N))
print(sum(I_n)/(iterations*N))
print(sum(R_n)/(iterations*N))



#h)

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








