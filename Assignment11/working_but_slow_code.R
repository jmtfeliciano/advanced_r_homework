


### METHOD 1 ###############


# sets working directory
wd<-"/Users/jfeliciano/Documents/advancedr/homework/Assignment11"
setwd(wd) 

# loads election data from directory
election_data <- read.csv("Election data.csv", header=TRUE)

# changeable for later
# number of simulation
num_simulation <- 10000
seed <- 12345

ptm <- proc.time()
# Method 1


dem_all_ec <- c()
rep_all_ec <- c()
ind_all_ec <- c()

for(sim in 1:num_simulation){
  dem_ec_tally <- 0
  rep_ec_tally <- 0
  ind_ec_tally <- 0
  
  
  for(state in 1:nrow(election_data)) {
    current_prob <- rmultinom(1, 1, prob = c(election_data[state,2], election_data[state,3],  election_data[state,4]))
    if(current_prob[1] == 1) {
      dem_ec_tally <- dem_ec_tally + election_data[state,"Electoral.Votes"]
    }else if(current_prob[2] == 1){
      rep_ec_tally <- rep_ec_tally + election_data[state,"Electoral.Votes"]
    }else{
      ind_ec_tally <- ind_ec_tally + election_data[state,"Electoral.Votes"]
    }
  }
  
  dem_all_ec <- c(dem_all_ec, dem_ec_tally)
  rep_all_ec <- c(rep_all_ec, rep_ec_tally)
  ind_all_ec <- c(ind_all_ec, ind_ec_tally)
}

hist(dem_all_ec, col=rgb(0,0,1,0.2), main="Overlapping Simulated Electoral College Win", xlab="Electoral College Distribution")
hist(rep_all_ec, col=rgb(1,0,0,0.2), add=T)
box()
abline(v = 270, lwd=4, col="yellow")

hist(dem_all_ec2, col=rgb(0,0,1,0.2), main="Overlapping Simulated Electoral College Win", xlab="Electoral College Distribution")
hist(rep_all_ec2, col=rgb(1,0,0,0.2), add=T)
box()
abline(v = 270, lwd=4, col="yellow")






########## METHOD 2 ##########################



# sets working directory
wd<-"/Users/jfeliciano/Documents/advancedr/homework/Assignment11"
setwd(wd) 
library(dplyr)

# loads election data from directory
election_data <- read.csv("Election data.csv", header=TRUE)

# changeable for later
# number of simulation
num_simulation <- 10000
seed <- 242424242424


# Method 1





simulated_state_winner <- function(state){
  methods <- list()
  methods$method1 <- rmultinom(1, 1, prob = c(election_data[state,2], election_data[state,3],  election_data[state,4]))
  methods$method2 <- rmultinom(1, 1, prob = c(election_data[state,2]+election_data[state,4], election_data[state,3], 0))
  methods$method3 <- rmultinom(1, 1, prob = c(election_data[state,2], election_data[state,3]+election_data[state,4], 0))
  methods$method4 <- rmultinom(1, 1, prob = c(election_data[state,2]+election_data[state,4]/2, election_data[state,3]+election_data[state,4]/2, 0))
  
  electoral_distribution <- c()
  
  for(i in 1:4){
    dem_ec_tally <- 0
    rep_ec_tally <- 0
    
    if(methods[[i]][1] == 1){
      dem_ec_tally <- election_data[state,"Electoral.Votes"]
    }else if(methods[[i]][2] == 1){
      rep_ec_tally <- election_data[state,"Electoral.Votes"]
    }
    electoral_distribution <- c(electoral_distribution, dem_ec_tally, rep_ec_tally)
  }
  
  return(electoral_distribution)
}


one_national_election <- function(arg){
  one_simulation <- sapply(1:51, simulated_state_winner)
  ec_sum <- c()
  for(i in 1:nrow(one_simulation)){
    ec_sum <- c(ec_sum, sum(one_simulation[i,]))
  }
  return(ec_sum)
}


all_election <- sapply(1:10000, one_national_election)


par(mfrow=c(2,2)) 

hist(all_election[1,], col=rgb(0,0,1,0.2), main="Simulated Clinton-Trump EC Distribution\nMethod 1", xlab="Electoral College Distribution")
hist(all_election[2,], col=rgb(1,0,0,0.2), add=T)
box()
abline(v = 270, lwd=3, col="yellow")

hist(all_election[3,], col=rgb(0,0,1,0.2), main="Simulated Clinton-Trump EC Distribution\nMethod 2", xlab="Electoral College Distribution")
hist(all_election[4,], col=rgb(1,0,0,0.2), add=T)
box()
abline(v = 270, lwd=3, col="yellow")

hist(all_election[5,], col=rgb(0,0,1,0.2), main="Simulated Clinton-Trump EC Distribution\nMethod 3", xlab="Electoral College Distribution")
hist(all_election[6,], col=rgb(1,0,0,0.2), add=T)
box()
abline(v = 270, lwd=3, col="yellow")

hist(all_election[7,], col=rgb(0,0,1,0.2), main="Simulated Clinton-Trump EC Distribution\nMethod 4", xlab="Electoral College Distribution")
hist(all_election[8,], col=rgb(1,0,0,0.2), add=T)
box()
abline(v = 270, lwd=3, col="yellow")


win_dem_1 <- sum(all_election[1,] > 270 & (all_election[1,] > all_election[2,]))/num_simulation
win_rep_1 <- sum(all_election[2,] > 270 & (all_election[1,] < all_election[2,]))/num_simulation
undecided_1 <- 1- win_dem_1 - win_rep_1 

print(paste("using method 1, the democrats win", win_dem_1, ", whereas the republicans win", win_rep_1, ", whereas the prob for undecided election is", undecided_1))


win_dem_2 <- sum(all_election[3,] > 270 & (all_election[3,] > all_election[4,]))/num_simulation
win_rep_2 <- sum(all_election[4,] > 270 & (all_election[3,] < all_election[4,]))/num_simulation
undecided_2 <- 1- win_dem_2 - win_rep_2

print(paste("using method 2, the democrats win", win_dem_2, ", whereas the republicans win", win_rep_2, ", whereas the prob for undecided election is", undecided_2))



win_dem_3 <- sum(all_election[5,] > 270 & (all_election[5,] > all_election[6,]))/num_simulation
win_rep_3 <- sum(all_election[6,] > 270 & (all_election[5,] < all_election[6,]))/num_simulation
undecided_3 <- 1- win_dem_3 - win_rep_3

print(paste("using method 3, the democrats win", win_dem_3, ", whereas the republicans win", win_rep_3, ", whereas the prob for undecided election is", undecided_3))


win_dem_4 <- sum(all_election[7,] > 270 & (all_election[7,] > all_election[8,]))/num_simulation
win_rep_4 <- sum(all_election[8,] > 270 & (all_election[7,] < all_election[8,]))/num_simulation
undecided_4 <- 1- win_dem_4 - win_rep_4

print(paste("using method 4, the democrats win", win_dem_4, ", whereas the republicans win", win_rep_4, ", whereas the prob for undecided election is", undecided_4))
