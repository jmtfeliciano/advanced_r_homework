# Non-generalized solution to experiment on
# Trial for one scenario to play with as I write solution for my general function
if(!require("stats")) install.packages("stats") 
library("stats") 

# current statistics
medical_practices <- 86
mean <- 70
st_dev <- 5
num_sims <- 1000

all_scenarios <- data.frame(scenario=numeric(4),
                            average_cv=numeric(4),
                            cutoff_90=numeric(4),
                            acceptability=logical(4))

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

# 90% cutoff
quantile(cv_for_valid_sims, 0.90)








