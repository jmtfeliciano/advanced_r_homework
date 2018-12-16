# Josemari Feliciano (jtf44)
# R-Project


cluster_randomized_simulator <- function(seed,medical_practices,mean,num_sims,dist,st_dev,min,max){
  
  # loads necessary stats package, install it if necessary
  if(!require("stats")) install.packages("stats") 
  library("stats") 
  set.seed(seed)
  
  #initializes an empty dataframe with length reflecting necessary # of scenarios
  if(dist==1) {
    all_scenarios <- data.frame(scenario=numeric(length(st_dev)),
                                average_cv=numeric(length(st_dev)),
                                cutoff_90=numeric(length(st_dev)),
                                acceptability=logical(length(st_dev)))
  }else if(dist==2) {
    all_scenarios <- data.frame(scenario=numeric(length(min)),
                                average_cv=numeric(length(min)),
                                cutoff_90=numeric(length(min)),
                                acceptability=logical(length(min)))
  }else{
    stop("You are an idiot, please read my documentation.  Only enter 1 or 2, dammnit")
  }
  
  # performs scenarios for the specified test
  if(dist==1){
    for(current_scenario in 1:length(st_dev)){ # start of current scenario loop
      
      cv_for_valid_sims <- c()
      
      while(length(cv_for_valid_sims) < num_sims) {
        current_participants <- rnorm(medical_practices,mean,st_dev[current_scenario])
        if(sum(current_participants) >= 6000 && sum(current_participants) <= 6100) {
          current_cv <- sd(current_participants) / mean(current_participants)
          cv_for_valid_sims <- c(cv_for_valid_sims, current_cv)
        }
      }
      
      all_scenarios[current_scenario,1] <- current_scenario
      all_scenarios[current_scenario,2] <- mean(cv_for_valid_sims)
      all_scenarios[current_scenario,3] <- quantile(cv_for_valid_sims, 0.90)
      all_scenarios[current_scenario,4] <- 0.90 <= sum(cv_for_valid_sims < 0.23)/num_sims 
      
    } # end of current scenario loop
    
    
  }else if(dist==2){
    
    min <- sort(min)
    max <- sort(max, decreasing=TRUE)
    
    for(current_scenario in 1:length(min)){ # start of current scenario loop
      
      cv_for_valid_sims <- c()
      
      while(length(cv_for_valid_sims) < num_sims) {
        current_participants <- runif(medical_practices,min[current_scenario],max[current_scenario])
        if(sum(current_participants) >= 6000 && sum(current_participants) <= 6100) {
          current_cv <- sd(current_participants) / mean(current_participants)
          cv_for_valid_sims <- c(cv_for_valid_sims, current_cv)
        }
      }
      
      all_scenarios[current_scenario,1] <- current_scenario
      all_scenarios[current_scenario,2] <- mean(cv_for_valid_sims)
      all_scenarios[current_scenario,3] <- quantile(cv_for_valid_sims, 0.90)
      all_scenarios[current_scenario,4] <- 0.90 <= sum(cv_for_valid_sims < 0.23)/num_sims 
      
    } # end of current scenario loop
    
  }
  
 
  return(all_scenarios)  

}

cluster_randomized_simulator(seed=123, medical_practices=86, mean=70, num_sims=1000, dist=1, st_dev=c(5,10,15,20))
cluster_randomized_simulator(seed=123, medical_practices=86, mean=70, num_sims=1000, dist=2, min=c(40,45,50), max=c(90,95,100))





