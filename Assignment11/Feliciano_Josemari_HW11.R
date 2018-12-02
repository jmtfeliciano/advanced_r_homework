# Josemari Feliciano
# Homework 11

# sets working directory
wd<-"/Users/jfeliciano/Documents/advancedr/homework/Assignment11"
setwd(wd) 

# loads election data from directory
election_data <- read.csv("Election data.csv", header=TRUE)

# changeable for later
# number of simulation
num_simulation <- 10000
seed <- 12345


# it loops through the states to determine win


dem_all_ec <- c()
rep_all_ec <- c()
ind_all_ec <- c()

for(sim in 1:num_simulation){
  dem_ec_tally <- 0
  rep_ec_tally <- 0
  ind_ec_tally <- 0
  
  for(state in 1:nrow(election_data)) {
    current_prob <- rdirichlet(1, c(election_data[state,2], election_data[state,3],  election_data[state,4]))
    if(current_prob[1] > current_prob[2] && current_prob[1] > current_prob[3]) {
      dem_ec_tally <- dem_ec_tally + election_data[state,"Electoral.Votes"]
    }else if(current_prob[2] > current_prob[1] && current_prob[2] > current_prob[3]){
      rep_ec_tally <- rep_ec_tally + election_data[state,"Electoral.Votes"]
    }else{
      ind_ec_tally <- ind_ec_tally + election_data[state,"Electoral.Votes"]
    }
  }
  
  dem_all_ec <- c(dem_all_ec, dem_ec_tally)
  rep_all_ec <- c(rep_all_ec, rep_ec_tally)
  ind_all_ec <- c(ind_all_ec, ind_ec_tally)
}


hist(dem_all_ec, col="blue", main="Overlapping Histogram", xlab="Electoral College Distribution")
hist(rep_all_ec, col="red", add=T)
box()
lines(x=270, lwd=4)

