##Project 1 d)
beta = 0.05
gamma = 0.10
alpha = 0.01

P = matrix(c(1-beta, beta, 0, 0, 1 - gamma, gamma, alpha, 0, 1-alpha), nrow = 3, ncol = 3, byrow = TRUE)

print(P)
P[1,2]

v1 = 0 #total number of "loops" in state 0
v2 = 0 #total number of "loops" in state 1
v3 = 0 #total number of transitions from 0 to 1 and 1 to 2

v1_counter = 0 #number of transitions from 0 to 1
v2_counter = 0 # number of transitions from 1 to 2
v3_counter = 0 # number of complete cycles

year = 365

state = 0
for (i in 1:(year*20)) {

  
  newstate = sample.int(3, size = 1, replace = TRUE, prob = P[state+1,])-1
  
  if (newstate != state) {
    if(state == 0) {
      v1_counter = v1_counter + 1
      v3 = v3 +1
    }
    else if(state == 1) {
      v2_counter = v2_counter + 1
      v3 = v3 + 1
    }
    else if(state == 2) {
      v3_counter = v3_counter + 1
    }
    
  }
  else {
    if (state == 0) {
      v1 = v1 +1
      v3 = v3 + 1}
    else if (state == 1) {
      v2 = v2 +1
      v3 = v3 + 1}
    else if (state == 2){
      v3 = v3 + 1
    }
  }
  
  state = newstate
  
}
    
print(v1/v1_counter)
print(1/beta)

print(v2/v2_counter)
print(1/gamma)

print(v3/v3_counter)
print(1/beta + 1/alpha + 1/gamma)

  
 


