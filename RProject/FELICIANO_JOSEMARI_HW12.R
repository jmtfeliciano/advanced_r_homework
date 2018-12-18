# Josemari Feliciano (jtf44)
# R-Project


######################################################################################################

#Function: cluster_randomized_simulator()

#Author: Josemari Feliciano

#Creation Date: December 16, 2018 (version 3.4.0)

#Purpose: - To return a data frame which includes the scenario, average CV, 90% cut off and acceptability 
# decision given the mean and standard deviation for varying standard deviation for normal distribution, or 
# given varying min and max for uniform distribution

# Required Parameters: 
#   - seed: seed number that will be used to set via set.seed()
#   - medical_practices:  number of 'medical practices' or similar source population we will get people from
#   - mean: required for normal distribution (dist = 1); but optional for uniform distribution (dist = 2); 
#         for normal distribution, will be used to generate numbers via rnorm()
#   - num_sims: number of simulations that will be performed for each scenario    
#   - dist: 1 or 2; 1 will perform calculations for normal distribution; 2 will perform calculations for uniform distribution
#   - st_dev: required for normal distribution (dist = 1); but optional for uniform distribution (dist = 2); 
#         for normal distribution, will be used to generate numbers via rnorm()
#   - min: required for uniform distribution (dist = 2); but optional for uniform distribution (dist = 1); 
#         for uniform distribution, will be used to generate numbers via runif()
#   - max: required for normal distribution (dist = 2); but optional for uniform distribution (dist = 1); 
#         for uniform distribution, will be used to generate numbers via runif()
#   
#      
#Output: Returns a data frame for one or more of the specified scenarios 

#Example: cluster_randomized_simulator(seed=123, medical_practices=86, mean=70, num_sims=1000, dist=1, st_dev=c(5,10,15,20))
########################################################################################################




cluster_randomized_simulator <- function(seed,medical_practices,mean=0,num_sims,dist,st_dev,min,max){
  
  
  
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
    if(length(min) != length(max)) stop("Please ensure to include same length of vector for both max and min for running uniform distribution")
    
  }else{
    stop("Please ensure that you enter a valid distribution to check.  dist=1 if you want to test normal dist; dist=2 if you want to check uniform dist")
  }
  
  

  
  
  
  
  # calculates for each scenario depending if the user wants to check normal or uniform dist.
  if(dist==1){
    
    # loops through each st dev
    for(current_scenario in 1:length(st_dev)){ 
      
      # contains CV for all those with valid sum of participants
      cv_for_valid_sims <- c() 
      while(length(cv_for_valid_sims) < num_sims) {
        current_participants <- ceiling(rnorm(medical_practices,mean,st_dev[current_scenario]))
        
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
  
    for(current_scenario in 1:length(min)){ # start of current scenario loop
      
      cv_for_valid_sims <- c()
      
      while(length(cv_for_valid_sims) < num_sims) {
        current_participants <- ceiling(runif(medical_practices,min[current_scenario],max[current_scenario]))
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
cluster_randomized_simulator(seed=123, medical_practices=86, num_sims=1000, dist=2, min=c(40,45,50), max=c(100,95,90))



