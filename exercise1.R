

runif(1)

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


