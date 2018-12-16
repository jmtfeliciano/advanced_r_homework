# Josemari Feliciano (jtf44)
# R-Project


cluster_randomized_simulator <- function(seed,medical_practices,mean,num_sims,test,st_dev,min,max){
  if(!require("stats")) install.packages("stats") 
  library("stats") 
  set.seed(seed)
  
  if(test==1){
    
  }
  
}

cluster_randomized_simulator(seed=123, medical_practices=86, mean=70, num_sims=1000, test=1, st_dev=c(5,10,15,20))
cluster_randomized_simulator(seed=123, medical_practices=86, mean=70, num_sims=1000, test=2, min=c(40,45,50), max=c(90,95,100))





