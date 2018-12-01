
###### Josemari Feliciano
###### BIS 679





#####################################################################################################
#Function: permuted

#Author: Josemari Feliciano

#Creation Date: November 8, 2018 

#Purpose: The purpose of this function is to create a vector of treatment assignments
#          based on permutted block randomization technique 

#Required Parameters: 
#      sample_size = the desired sample size for the study (numerical)
#      blocks = a set of possible block sizes that will be used in block randomization (numerical vector)
#      seed = desired seed for randomization, will be used in set.seed() inside (numerical)

#Output:  Returns a vector with the sequential assignment of treatments 

#Example: permuted(sample_size=8, blocks=c(2,4), seed=999) will return
#                 [1] "B" "A" "B" "A" "A" "B" "B" "A"

#####################################################################################################



permuted <- function(sample_size, blocks, seed) {
  
  # sets seed 
  set.seed(seed)
  
  # initializes an empty vector
  master_list <- c()
  
  # while-loop will continue until master list has equal or greater than desired sample size
  # this randomly selects a block, create a random sample with that block size, then add it to the master list
  while(length(master_list) < sample_size) { 
    current_block <- sample(blocks, size=1, replace=TRUE)  # randomly selects a number from the block
    current_pool <- c(rep('A',current_block/2), rep('B',current_block/2)) # creates half As, half Bs 
    current_scrambled <- sample(current_pool) # scrambles series of A,B's using `sample()`
    master_list <- c(master_list, current_scrambled) # adds current sample to the end of master list
  }
  
  # returns the created sequence of treatment assignments
  return(master_list)
  
}