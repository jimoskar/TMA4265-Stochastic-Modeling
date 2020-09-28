#Problem 1

P = matrix(c(0.1,0.7,0.2,0.5,0.1,0.4,0.3,0.6,0.1),
           nrow = 3,
           ncol = 3,
           byrow = true)
P
X3 = P %*% P  %*% P 
X3

#Problem 2

a = 0.30
b = 0.25

q = b/(a+b) #Probability of rain in the long run
q

N = 365

P = matrix(c(1-a,a,b,1-b),
           nrow = 2,
           ncol = 2,
           byrow = true)
P

x = vector('numeric', length = N +1)

raindays = 1

for(n in 1:N){
  newstate = sample.int(2, size = 1, replace = TRUE, prob = P[x[n]+1,])-1
  x[n+1] = newstate
  if(newstate == 0) {
    raindays = raindays +1 
  }
  
}

print(raindays/N)

plot(0:N, x, type = "o", lwd = 2, cex.axis = 1.5,
     main = "Realization", xlab = "Time", ylab = "Value",
     cex.lab = 1.5, cex.main = 1.5)

