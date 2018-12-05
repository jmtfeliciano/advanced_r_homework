# Josemari Feliciano
# Homework 11






election_simulator <- function(file_location, seed_num, num_sim, method){
  
  # reads the file
  election_data <- read.csv(file_location, header=TRUE)
  
  # sets the seed
  set.seed(seed_num) # assigns seed
  
  # creates the tally of electoral college for each party for each simulation
  dem_ec_tally <- rep(0,num_sim)
  rep_ec_tally <- rep(0,num_sim)
  
  # calculates the electoral college votes for each state in each simulation
  for(state in 1:nrow(election_data)) {
    
    # determines the distribution of wins for each state for #num_sim simulations for all 4 different methods
    if(method==1) { 
      current_state <- rmultinom(num_sim, 1, prob = c(election_data[state,'Democrat_Per'], election_data[state,'Republican_Per'],  election_data[state,'Other_Per']))
    }else if(method==2) {
      current_state <- rmultinom(num_sim, 1, prob = c(election_data[state,'Democrat_Per'] + election_data[state,'Other_Per'], election_data[state,'Republican_Per']))
    }else if(method==3) {
      current_state <- rmultinom(num_sim, 1, prob = c(election_data[state,'Democrat_Per'], election_data[state,'Republican_Per'] + election_data[state,'Other_Per']))
    }else if(method==4) {
      current_state <- rmultinom(num_sim, 1, prob = c(election_data[state,'Democrat_Per'] + election_data[state,'Other_Per']/2, election_data[state,'Republican_Per'] + election_data[state,'Other_Per']/2))
    }
    
    # adds the electoral votes to the vector of winner for current state for every single num_sim simulations
    for(col in 1:ncol(current_state)) {
      if(current_state[1,col] == 1){
        dem_ec_tally[col] <- dem_ec_tally[col] + election_data[state,"Electoral.Votes"] 
      }else if(current_state[2,col] == 1){
        rep_ec_tally[col] <- rep_ec_tally[col] + election_data[state,"Electoral.Votes"] 
      }
    }
  }
   
  # once all calculations are done, creates a histogram with labels and a yellow line for 270 electors threshold
  hist(dem_ec_tally, col=rgb(0,0,1,0.2), xlim=c(0, 540), main=paste0("Simulated Electoral College Distribution\nUsing Method ",method," with ",num_sim, " simulations"), xlab="Electoral College Accrual")
  hist(rep_ec_tally, col=rgb(1,0,0,0.2), add=T)
  legend("topright", c("Democratic", "Republican", "Overlap"), fill=c("Blue", "Red", "Purple"), cex=0.70)
  box()
  abline(v = 270, lwd=4, col="yellow")
  
  # declares wins as 0, will be used to tabulate probabilities later
  dem_win <- 0
  rep_win <- 0 
  
  # for each simulation, calculate who wins (whenever election is not undecided)
  for(index in 1:length(dem_ec_tally)) {
    if(dem_ec_tally[index] > rep_ec_tally[index] && dem_ec_tally[index] >= 270) {
      dem_win <- dem_win + 1
    }else if(dem_ec_tally[index] < rep_ec_tally[index] && rep_ec_tally[index] >= 270) {
      rep_win <- rep_win + 1
    }
  }
  
  # prints the probabilities for dem, rep, undecided (undecided is simply 1- p(dem) - p(rep))
  print(paste0("Using Method ",method, ", the probabilities for the Democrats and Republicans to outright win are ",dem_win/num_sim, " and ",rep_win/num_sim,", respectively."," The probability of an undecided election is ",1-dem_win/num_sim-rep_win/num_sim))
  
}






# sets the working directory as directed
wd<-"/Users/jfeliciano/Documents/advancedr/homework/Assignment11"
setwd(wd) 

# all the function calls to perform 4 homework prompts
# each function should take 10 seconds each

election_simulator(file_location="Election data.csv", seed_num=1234, num_sim=10000, method=1)
election_simulator(file_location="Election data.csv", seed_num=1234, num_sim=10000, method=2)
election_simulator(file_location="Election data.csv", seed_num=1234, num_sim=10000, method=3)
election_simulator(file_location="Election data.csv", seed_num=1234, num_sim=10000, method=4)


