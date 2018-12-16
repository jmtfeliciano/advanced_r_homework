# Josemari Feliciano (jtf44)
# R-Project


# current statistics
medical_practices <- 86
mean <- 70
st_dev <- 5
num_sims <- 1000

# empty vector of valid simulations
cv_for_valid_sims <- c()

# runs until we have desired number of valid simulations
while(length(cv_for_valid_sims) < num_sims) {
  current_participants <- rnorm(medical_practices,mean,st_dev)
  if(sum(current_participants) >= 6000 && sum(current_participants) <= 6100) {
    current_cv <- sd(current_participants) / mean(current_participants)
    cv_for_valid_sims <- c(cv_for_valid_sims, current_cv)
  }
}

# need to check how many simulations have CV < 0.23
acceptable <- sum(cv_for_valid_sims < 0.23)



acceptable <- 0
for(i in 1:1000){
  if(cv_for_valid_sims[i] < 0.23){
    acceptable <- acceptable + 1
  }
}


if(acceptable/1000 > 0.90){
  is
}








cluster_randomized_simulator <- function(seed,medical_practices,mean,num_sims,test,st_dev,min,max){
  if(!require("stats")) install.packages("stats") 
  library("stats") 
  set.seed(seed)
  
  if(test==1){
    
  }
  
}

cluster_randomized_simulator(seed=123, medical_practices=86, mean=70, num_sims=1000, test=1, st_dev=c(5,10,15,20))
cluster_randomized_simulator(seed=123, medical_practices=86, mean=70, num_sims=1000, test=2, min=c(40,45,50), max=c(90,95,100))





