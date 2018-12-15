# Josemari Feliciano (jtf44)
# R-Project


# current statistics
seed <- 123
medical_practices <- 86
mean <- 70
st_dev <- 5
num_sims <- 1000

# empty vector of valid simulations
cv_for_valid_sims <- c()

# runs to create
while(length(cv_for_valid_sims) < num_sims) {
  current_participants <- rnorm(medical_practices,mean,st_dev)
  if(sum(current_participants) >= 6000 && sum(current_participants) <= 6100) {
    current_cv <- sd(current_participants) / mean(current_participants)
    cv_for_valid_sims <- c(cv_for_valid_sims, current_cv)
  }
}

cv_for_valid_sims