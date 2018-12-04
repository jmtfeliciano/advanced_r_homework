# Josemari Feliciano
# Homework 11

# sets working directory
wd<-"/Users/jfeliciano/Documents/advancedr/homework/Assignment11"
setwd(wd) 

# loads election data from directory
election_data <- read.csv("Election data.csv", header=TRUE)
par(mfrow=c(2,2)) 

election_simulator <- function(seed_num, num_sim, method){
  
  set.seed(seed_num) # assigns seed
  
  dem_ec_tally <- rep(0,num_sim)
  rep_ec_tally <- rep(0,num_sim)
  
  for(state in 1:nrow(election_data)) {
    
    if(method==1) {
      current_state <- rmultinom(num_sim, 1, prob = c(election_data[state,'Democrat_Per'], election_data[state,'Republican_Per'],  election_data[state,'Other_Per']))
    }else if(method==2){
      current_state <- rmultinom(num_sim, 1, prob = c(election_data[state,'Democrat_Per'] + election_data[state,'Other_Per'], election_data[state,'Republican_Per']))
    }else if(method==3){
      current_state <- rmultinom(num_sim, 1, prob = c(election_data[state,'Democrat_Per'], election_data[state,'Republican_Per'] + election_data[state,'Other_Per']))
    }else if (method==4){
      current_state <- rmultinom(num_sim, 1, prob = c(election_data[state,'Democrat_Per'] + election_data[state,'Other_Per']/2, election_data[state,'Republican_Per'] + election_data[state,'Other_Per']/2))
    }
    
    for(col in 1:ncol(current_state)) {
      if(current_state[1,col] == 1){
        dem_ec_tally[col] <- dem_ec_tally[col] + election_data[state,"Electoral.Votes"] 
      }else if(current_state[2,col] == 1){
        rep_ec_tally[col] <- rep_ec_tally[col] + election_data[state,"Electoral.Votes"] 
      }
    }
  }
  
  hist(dem_ec_tally, col=rgb(0,0,1,0.2), main="Overlapping Simulated Electoral College Win", xlab="Electoral College Distribution")
  hist(rep_ec_tally, col=rgb(1,0,0,0.2), add=T)
  box()
  abline(v = 270, lwd=4, col="yellow")
  
  # calculate probability for winning
  dem_win <- 0
  rep_win <- 0 
  
  for(index in 1:length(dem_ec_tally)) {
    if(dem_ec_tally[index] > rep_ec_tally[index] && dem_ec_tally[index] >= 270) {
      dem_win <- dem_win + 1
    }else if(dem_ec_tally[index] < rep_ec_tally[index] && rep_ec_tally[index] >= 270) {
      rep_win <- rep_win + 1
    }
  }
  
  print(paste0("Using Method ",method, ", the probabilities for the Democrats and Republicans to outright win are ",dem_win/num_sim, " and ",rep_win/num_sim,", respectively."," The probability of an undecided election is ",1-dem_win/num_sim-rep_win/num_sim))
  
}

election_simulator(seed_num=1234, num_sim=10000, method=1)
election_simulator(seed_num=1234, num_sim=10000, method=2)
election_simulator(seed_num=1234, num_sim=10000, method=3)
election_simulator(seed_num=1234, num_sim=10000, method=4)



