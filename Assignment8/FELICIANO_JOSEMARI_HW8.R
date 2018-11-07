# Josemari Feliciano
# HW 8

# Declares a changeable wd and sets working directory 
wd<-"/Users/jfeliciano/Documents/advancedr/homework/Assignment8"
setwd(wd) 
require(dplyr)

# Loads the file required for this homework
demo_data <- read.csv("Demographics.csv", header = TRUE)
test_master <- read.csv("TestScores_original.csv", header = TRUE)
test_makeup <- read.csv("TestScores_makeup.csv", header = TRUE)

# method 1: removes all the NA test scores -- this will prevent collision when merging combined data into wide format later
# test_original <- test_original[!is.na(test_original$TestScore),]
# row_stacked_data <- rbind(test_original, test_makeup)

# another way -- for loop
# For-loop goes row by row, repl
for(row1 in 1:nrow(test_master)) {
  for(row2 in 1:nrow(test_makeup)) {
    if(test_makeup[row2,]$StudentID == test_master[row1,]$StudentID && test_makeup[row2,]$TestNumber == test_master[row1,]$TestNumber) {
      test_master[row1,]$TestNumber = test_makeup[row2,]$TestNumber
      break # for efficiency, exits out of inner for-loop to proceed to the next column in the original data set 
    }
  }
}

# logic:  if testscore is NA, replace with 0.  
# 
test_master[is.na(test_master$TestScore),]$TestScore <- 0

# 

# Transforms data from long to wide
wide_data <- reshape(test_master, timevar="TestNumber", idvar = "StudentID", v.names = "TestScore", direction="wide")


sum(wide_data[,2:6],na.rm=TRUE)/5

#  
wide_data <- wide_data %>% mutate(test_avg = mean(cat("TestScore.",1:5,sep="")))

full_data <- merge(demo_data, wide_data, by="StudentID", all= TRUE)
