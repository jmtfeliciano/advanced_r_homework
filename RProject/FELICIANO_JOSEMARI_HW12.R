# Josemari Feliciano (jtf44)
# R-Project



######################################################################################################
#
# Outline
#   - cv_calculator() function which is required by my cluster_randomized_simulator() function
#         - Did this part to satisfy the following :  "Write a function that will calculate the coefficient of variation (CV) [CV=SD/Mean] for a vector of data."
#   - cluster_randomized_simulator() function
# 
######################################################################################################




######################################################################################################

#Function: cv_calculator()

#Author: Josemari Feliciano

#Creation Date: December 16, 2018 (version 3.4.0)

#Purpose: - To calculate and return the cv of a given vector of numbers

# Required Parameters: 
#   - population_vector: a vector of numbers that will be used to calculate cv

#Output: Returns a single numerical that represents the cv value

#Example: cv_calculator(population_vector=c(4,5,6,7,10,921))
########################################################################################################



cv_calculator <- function(population_vector) {
  return(sd(population_vector)/mean(population_vector))
}


######################################################################################################

#Function: cluster_randomized_simulator()

#Author: Josemari Feliciano

#Creation Date: December 16, 2018 (version 3.4.0)

#Purpose: - To return a data frame which includes the scenario, average CV, 90% cut off and acceptability 
# decision given the mean and standard deviation for varying standard deviation for normal distribution, or 
# given varying min and max for uniform distribution

#Special Note/Requirement:  Function needs a function called `cv_calculator()` to calculate the CV.  
#               cluster_randomized_simulator() will call cv_calculator(data) which will calculate the cv given the data's sd and mean
#
# Required Parameters: 
#   - seed: seed number that will be used to set via set.seed()
#   - medical_practices:  number of 'medical practices' or similar source population we will get people from
#   - num_sims: number of simulations that will be performed for each scenario    
#   - dist: 1 or 2; 1 will perform calculations for normal distribution; 2 will perform calculations for uniform distribution
#   - st_dev: required for normal distribution (dist = 1); but optional for uniform distribution (dist = 2); 
#         for normal distribution, will be used to generate numbers via rnorm()
#   - min: required for uniform distribution (dist = 2); but optional for uniform distribution (dist = 1); 
#         for uniform distribution, will be used to generate numbers via runif(). min[1] will be matched with max[1], .... , min[n] will be matched with max[n]
#   - max: required for normal distribution (dist = 2); but optional for uniform distribution (dist = 1); 
#         for uniform distribution, will be used to generate numbers via runif(). min[1] will be matched with max[1], .... , min[n] will be matched with max[n]
#
# Optional Parameters:  
#   - mean: for normal distribution (dist = 1), the default is 0; 
#         - not used at all for uniform distribution (dist = 2) but default remains 0; 
#      
#Output: Returns a data frame for one or more of the specified scenarios with scenario #, average cv, 90% cutoff, and acceptability decision

#Example: cluster_randomized_simulator(seed=123, medical_practices=86, mean=70, num_sims=1000, dist=1, st_dev=c(5,10,15,20))
########################################################################################################


### NOTE:  Please ensure to load my cv_calculator() function which exists before my cv_calculator documentation

cluster_randomized_simulator <- function(seed,medical_practices,num_sims,dist,st_dev,min,max,mean=0){
  
  # loads necessary stats package, install it if necessary
  if(!require("stats")) install.packages("stats") 
  library("stats") 
  # sets seed
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
    
    # error if user input wrong size for either min and max vector
    if(length(min) != length(max)) stop("Please ensure to include same length of vector for both max and min for running uniform distribution")
    
  }else{
    stop("Please ensure that you enter a valid distribution to check.  dist=1 if you want to test normal dist; dist=2 if you want to check uniform dist")
  }

  
  
  
  # calculates for each scenario depending if the user wants to check normal (dist=1) or uniform dist (dist=2).
  if(dist==1){
    
    # loops through each st dev
    for(current_scenario in 1:length(st_dev)){ 
      
      # contains CV for all those with valid sum of participants
      cv_for_valid_sims <- c() 
      
      # performs while-loop until it has desired number
      while(length(cv_for_valid_sims) < num_sims) {
        
        # creates vector of participants  based on # of medical practice, mean, and current st dev
        current_participants <- ceiling(rnorm(medical_practices,mean,st_dev[current_scenario]))
        
        # if it contains desired sum, the vector from above will be passed to cv_calculator
        # the cv returned from cv_calculator() will be added to cv_for_valid_sims
        if(sum(current_participants) >= 6000 && sum(current_participants) <= 6100) {
          cv_for_valid_sims <- c(cv_for_valid_sims, cv_calculator(current_participants))
        }
      }
      
      all_scenarios[current_scenario,1] <- current_scenario
      all_scenarios[current_scenario,2] <- mean(cv_for_valid_sims)
      all_scenarios[current_scenario,3] <- quantile(cv_for_valid_sims, 0.90)
      all_scenarios[current_scenario,4] <- (all_scenarios[current_scenario,3] < 0.23)
      
    } # end of current scenario loop
    
    
  }else if(dist==2){
  
    #loops through each min and max; min[1] gets matched with max[2], ... etc
    for(current_scenario in 1:length(min)){ # start of current scenario loop
      
      cv_for_valid_sims <- c()
      
      while(length(cv_for_valid_sims) < num_sims) {
        
        # creates vector of participants based on # of medical practice, mean, and current st dev 
        current_participants <- ceiling(runif(medical_practices,min[current_scenario],max[current_scenario]))
        if(sum(current_participants) >= 6000 && sum(current_participants) <= 6100) {
          cv_for_valid_sims <- c(cv_for_valid_sims, cv_calculator(current_participants))
        }
      }
      
      all_scenarios[current_scenario,1] <- current_scenario
      all_scenarios[current_scenario,2] <- mean(cv_for_valid_sims)
      all_scenarios[current_scenario,3] <- quantile(cv_for_valid_sims, 0.90)
      all_scenarios[current_scenario,4] <- (all_scenarios[current_scenario,3] < 0.23)
      
    } # end of current scenario loop
    
  }
  
  # return data.frame needed for request output: scenario average_cv cutoff_90 acceptability
  return(all_scenarios)  

}



# required function calls for this project
cluster_randomized_simulator(seed=123, medical_practices=86, mean=70, num_sims=1000, dist=1, st_dev=c(5,10,15,20))
cluster_randomized_simulator(seed=123, medical_practices=86, num_sims=1000, dist=2, min=c(40,45,50), max=c(100,95,90))



