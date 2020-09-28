lambda = 1.5

prob = 1 - ppois(q = 100, lambda = lambda*59)
prob

pgamma(59,101,rate = 1.5)




simulate_pois <- function(lambda, t_start,t_end, plot, numSim = 1) {
  result <-  rep(NA,numSim)
  for (i in 1:numSim) {
    n = rpois(1,lambda = lambda*(t_end - t_start))
    result[i] = n
    v_vec = runif(n,t_start,t_end)
    w_vec = sort(v_vec)
    x_vec <- seq(from = 0, to = n, by = 1)
    
    if(plot && i == 1) {
    
      plot(NULL, NULL, xlim = c(t_start, t_end), ylim = c(0, n), xlab = "Time [days]", lwd = 2, ylab = "Claims", main = "Insurance Claims")
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

simulate_pois(lambda,0,59,plot = TRUE)
res <- simulate_pois(lambda,0,59,plot = FALSE, numSim = 1000)
num <- sum(res > 100)/length(res)
num

simulate_pois(lambda,0,59,plot = TRUE, numSim = 10)


