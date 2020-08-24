

runif(1)

##Problem 1

# one toss
N = 1000
biased_heads = 0
totalheads = 0

for(i in 1:N){
  r1 = runif(1)
  #pick biased coin
  if(r1 < 0.5) {
    biased_heads = biased_heads + 1
    totalheads = totalheads +  1
  }
  else {
    r2 = runif(1)
    if(r2 < 0.5)
      totalheads = totalheads + 1
  }
}

biased_heads/totalheads


#two tosses

N = 100000
biased_heads = 0
totalheads = 0

for(i in 1:N){
  r1 = runif(1)
  #pick biased coin
  if(r1 < 0.5) {
    biased_heads = biased_heads + 1
    totalheads = totalheads +  1
  }
  else {
    r2 = runif(1)
    if(r2 < 0.5) {
      r3 = runif(1)
      if (r3 < 0.5) {
        totalheads = totalheads + 1
      }
    }
  }
}

biased_heads/totalheads


##Problem 2


lambda = 6
std = 1
mu = -2
years = 10000
totalsum = 0

for( i in 1:years){
  N = rpois(1,lambda)
  sum = 0
  for(j in 1:N) {
    sum = sum + rlnorm(1,mu)
  }
  totalsum = totalsum + sum
}


totalsum/years





