#######################################
#                                     #
#     Josemari Feliciano (jtf44)      #
#     HW 10                           #
#                                     #
#######################################





#####################################################################################################
#                         
#  OUTLINE:
#    - Dr. Esserman's documentation and actual code for coverage() 
#    - MY documentation and actual code for plot.coverage() which will use Dr. Esserman's coverage()
#
#####################################################################################################





######################################################################################################

#Function: coverage

#Author: Denise Esserman

#Creation Date: November 24, 2015 (version 3.2.2)

#Modified: November 06, 2017 (version 3.4.2)

#Purpose: This function calculates the coverage probabilities for one sample proportion using
#         three methods (1) exact, (2) asymptotic, (3) asymptotic with continuity correction

# Required Parameters: 
#      n.vector = a vector of the sample sizes that we want to explore
#      p.vector= a vector of probabilities that we want to explore
#      seed = the seed to start the random number generator 

#Optional Parameters:
#      nsims=the number of simulations to conduct - default is 1000

#Libraries: Requires the installation of binom package 

#Output: Returns an array with the coverage probabilites.  Each matrix within the array
#represents a given sample size in n.vector.  The rows in the matrix are the probabilities we
#want to explore and the columns represent the 3 methods.

#Example: Calculate the coverage probabilities for two sample sizes and 5 proportions
#coverage(n.vector=c(30,100),p.vector=c(0.1,0.2,0.3,0.4,0.5), seed=121212)
########################################################################################################


coverage<-function(n.vector=n.vector, p.vector=p.vector, seed=seed, nsims=1000) {
  #check to see if the package is already installed
  if (!require("binom")) install.packages("binom")
  require(binom) #load the package
  set.seed(seed)
  #nsims=nsims
  y=list(p.vector,c("Exact", "Asymptotic", "Continuity Corrected"), n.vector) 
  cover.prob<-array(rep(NA, length(p.vector)*3*length(n.vector)),dim=c(length(p.vector),3, length(n.vector)),dimnames=y)
  
  for(j in 1:length(n.vector)) {
    for(i in 1:length(p.vector)){
      #Get vector of length nsims containing the number of successes from the binomial distribution 
      # with size n and probability of succcess p
      successes<-rbinom(nsims,size=n.vector[j],p=p.vector[i])
      #Get the upper and lower bounds of each of the three types of confidence intervals
      lower.exact<-binom.confint(successes,n.vector[j],method="exact")$lower
      upper.exact<-binom.confint(successes,n.vector[j],method="exact")$upper
      lower.asym<-binom.confint(successes,n.vector[j],method="asymptotic")$lower
      upper.asym<-binom.confint(successes,n.vector[j],method="asymptotic")$upper
      lower.cc<-lower.asym-0.5/n.vector[j]
      upper.cc<-upper.asym+0.5/n.vector[j]
      cover.prob[i,1,j]<-mean(lower.exact<=p.vector[i] & upper.exact>=p.vector[i])
      cover.prob[i,2,j]<-mean(lower.asym<=p.vector[i] & upper.asym>=p.vector[i])
      cover.prob[i,3,j]<-mean(lower.cc<=p.vector[i] & upper.cc>=p.vector[i])
    }
  }
  
  return(cover.prob) #return the array
}



######################################################################################################

#Function: plot.coverage()

#Author: Josemari Feliciano

#Creation Date: November 23, 2018 (version 3.4.0)

#Purpose: To plot coverage probability computed via exact, asymptotic, 
#         and asumptotic w/ continuity correction in the same plot

# Required Parameters: 
#      N.VECTOR = a vector of the sample sizes that we want to plot
#      P.VECTOR = a vector of probabilities that we want to plot
#      SEED = the seed to start the random number generator 

#Libraries: Requires the installation of binom package

#Output: No direct return value. It will simply print a plot.

#Example: Calculate the coverage probabilities for two sample sizes and two probabilities
#         plot.coverage(N.VECTOR=c(10,20), P.VECTOR=c(0.1,0.2), SEED=10201020)

########################################################################################################




plot.coverage <- function(N.VECTOR, P.VECTOR, SEED){
  
  
  # stops the function if N.VECTOR is length 0 or greater than 6
  if(length(N.VECTOR) > 6 | length(N.VECTOR) == 0) {
    stop("FUNCTION STOPPED:  Length of vector with sample sizes must be between 1 and 6.  Rerun function with right length.  ")
  }
  
  
  # gets an array of coverage probabilites using Dr. Esserman's function and saves it ascoverage_array
  coverage_array <- coverage(n.vector=N.VECTOR, p.vector=P.VECTOR, seed=SEED)
  
  
  # these code partions my columns and rows accordingly
  if(length(N.VECTOR) >= 2) {
    par(mfrow=c(2,ceiling(length(N.VECTOR)/2)))
  }else {
    par(mfrow=c(1,1)) 
  }
  
  
  # the for-loop will plot coverage probability for each sample size
  for(i in 1:length(N.VECTOR)){
    
    
    # subsets for the specific sample size
    sample_data <- data.frame(coverage_array[,,i])
    
    # creates plot of coverage probability for exact, asymptotic, and continuity asymptotic 
    plot(P.VECTOR, sample_data$Exact, type='l', ylim=c(0, 1),
         xlab='True Probability', ylab='Coverage Probability',
         main = paste0('Coverage Probability for 3 Methods\n n=', N.VECTOR[i]))
    points(P.VECTOR, sample_data$Asymptotic, type='l', col='red', lty=2)
    points(P.VECTOR, sample_data$Continuity.Corrected, type='l', col='green', lty=2)
    
    # creates legend 
    legend(cex=0.50, legend=c('Exact', 'Asymptotic','Continuity.Corrected'), col=c('black', 'red', 'green'), lty=c(1,2,3), x="bottomright")
  }
  
  # not necessary but it just resets the partitions back to normal :D
  par(mfrow=c(1,1)) 
}


# Creates the plots requested by the homework prompt
plot.coverage(N.VECTOR=c(40),P.VECTOR=seq(0.01, 0.50, by=0.01),SEED=1998)
plot.coverage(N.VECTOR=c(10,20,30),P.VECTOR=seq(0.01, 0.50, by=0.01),SEED=1998)
plot.coverage(N.VECTOR=seq(10, 80, by=10),P.VECTOR=seq(0.01, 0.50, by=0.01),SEED=1998)
plot.coverage(N.VECTOR=c(25,50,100,200),P.VECTOR=seq(0.01, 0.50, by=0.01),SEED=1998)



