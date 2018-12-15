# Josemari Feliciano (jtf44)
# R-Project


# current statistics
medical_practices <- 86
mean <- 70
st_dev <- 10
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






# cluster_randomized_simulator <- function(seed,mean,st_dev,min,max){
#}

# cluster_randomized_simulator(seed=123, mean=70, st_dev=c(5,10,15,20))
# cluster_randomized_simulator(seed=123, mean=70, min=c(40,45,50), max=c(90,95,100))





